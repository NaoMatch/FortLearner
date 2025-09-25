from __future__ import annotations

import time
from itertools import product

import numpy as np
from flearner.FortKMeans import FortKMeans
from sklearn.cluster import KMeans
from sklearn.metrics import adjusted_rand_score

def make_structured_kmeans_data(
    n_samples=1_000_000,
    n_features=100,
    n_clusters=8,
    *,
    latent_dim=8,           # 低次元でクラスタを作り→高次元へ射影して相関を作る
    seed=0,
    imbalance_alpha=0.6,    # 小さいほどクラスタサイズが不均衡（Dirichlet）:contentReference[oaicite:2]{index=2}
    cluster_sep=6.0,        # クラスタ中心の分離度（潜在空間）
    outlier_frac=0.01,      # 外れ値の割合
    dtype=np.float32,
):
    rng = np.random.default_rng(seed)

    # 1) 不均衡なクラスタ比
    weights = rng.dirichlet(np.full(n_clusters, imbalance_alpha))          # Dirichlet :contentReference[oaicite:3]{index=3}
    counts  = rng.multinomial(n_samples, weights)

    # 2) 高次元への射影行列（列直交）で相関を作る
    A = rng.normal(size=(n_features, latent_dim))
    Q, _ = np.linalg.qr(A)
    W = Q[:, :latent_dim].astype(dtype, copy=False)  # (n_features x latent_dim)

    X_lat_list = []
    y_list = []

    for k, nk in enumerate(counts):
        if nk == 0:
            continue
        # 3) 楕円分布（異方性）：回転R × スケールdiag(s) で形を変える（潜在空間）:contentReference[oaicite:4]{index=4}
        R, _ = np.linalg.qr(rng.normal(size=(latent_dim, latent_dim)))
        scales = rng.lognormal(mean=0.0, sigma=0.5, size=latent_dim)  # 軸ごとにばらつきを変える
        T = R @ np.diag(scales)

        mu = rng.normal(scale=cluster_sep, size=latent_dim)   # 中心
        Z  = rng.normal(size=(nk, latent_dim))                # 標準正規
        Xk_lat = Z @ T.T + mu                                 # 楕円ガウス
        X_lat_list.append(Xk_lat.astype(dtype))
        y_list.append(np.full(nk, k, dtype=np.int32))

    X_lat = np.vstack(X_lat_list)
    y     = np.concatenate(y_list)

    # 4) 高次元へ射影 → 特徴ごとの単位差を付与
    X = X_lat @ W.T
    feat_scales = rng.lognormal(mean=0.0, sigma=1.0, size=n_features).astype(dtype)
    X *= feat_scales

    # 5) 外れ値を注入
    n_out = int(outlier_frac * n_samples)
    if n_out > 0:
        idx = rng.choice(n_samples, size=n_out, replace=False)
        lo  = np.percentile(X, 2, axis=0)
        hi  = np.percentile(X, 98, axis=0)
        span = (hi - lo)
        X[idx] = rng.uniform(lo - 3*span, hi + 3*span, size=(n_out, n_features)).astype(dtype)
        y[idx] = -1  # 外れ値の目印（学習には未使用でもOK）

    return X.astype(dtype, copy=False), y

NUM_THREADS = 2
N_INIT      = 3
MAX_ITER    = 100
PRINT_LOG   = False

# ベンチマーク設定
SAMPLE_SIZES   = [2_000, 10_000, 50_000, 200_000, 1_000_000]
N_FEATURES_SET = [8, 32, 128]
CLUSTER_COUNTS = [2, 4, 8, 16, 32]

def standardize(X: np.ndarray) -> np.ndarray:
    mean = X.mean(axis=0, dtype=np.float64)
    return (X - mean).astype(np.float32, copy=False)


def run_single_case(n_samples: int, n_features: int, n_clusters: int, seed: int = 0) -> dict[str, float]:
    latent_dim = min(n_clusters, n_features, 16)

    X, y_true = make_structured_kmeans_data(
        n_samples=n_samples,
        n_features=n_features,
        n_clusters=n_clusters,
        latent_dim=latent_dim,
        seed=seed,
        outlier_frac=0.01,
        dtype=np.float32,
    )

    mask = (y_true != -1)
    X = standardize(X)

    fkm = FortKMeans(
        n_clusters=n_clusters,
        n_init=N_INIT,
        max_iter=MAX_ITER,
        num_threads=NUM_THREADS,
        print_log=PRINT_LOG,
        init="kmeans++",
    )

    start = time.perf_counter()
    fkm.fit(X)
    fkm_fit = time.perf_counter() - start

    start = time.perf_counter()
    fkm_labels = np.asarray(fkm.predict(X)).ravel()
    fkm_predict = time.perf_counter() - start
    fkm_inertia = -fkm.score(X)

    skm = KMeans(
        n_clusters=n_clusters,
        n_init=N_INIT,
        max_iter=MAX_ITER,
        algorithm="lloyd",
        random_state=seed,
    )

    start = time.perf_counter()
    skm.fit(X)
    skm_fit = time.perf_counter() - start

    start = time.perf_counter()
    skm_labels = np.asarray(skm.predict(X)).ravel()
    skm_predict = time.perf_counter() - start
    skm_inertia = skm.inertia_

    # 精度指標（外れ値は除外）。負のクラスタIDは ARI 計算対象から除外。
    ari_fkm = adjusted_rand_score(y_true[mask], fkm_labels[mask])
    ari_skm = adjusted_rand_score(y_true[mask], skm_labels[mask])

    return {
        "n_samples": n_samples,
        "n_features": n_features,
        "n_clusters": n_clusters,
        "fkm_fit": fkm_fit,
        "fkm_predict": fkm_predict,
        "fkm_inertia": fkm_inertia,
        "fkm_ari": ari_fkm,
        "skm_fit": skm_fit,
        "skm_predict": skm_predict,
        "skm_inertia": skm_inertia,
        "skm_ari": ari_skm,
    }


def main() -> None:
    cases = list(product(SAMPLE_SIZES, N_FEATURES_SET, CLUSTER_COUNTS))
    results = []
    for n_samples, n_features, n_clusters in cases:
        if n_clusters >= n_samples:
            continue
        print(
            f"Running: samples={n_samples:,}, features={n_features}, clusters={n_clusters}",
            flush=True,
        )
        results.append(run_single_case(n_samples, n_features, n_clusters))

    header = (
        "samples",
        "features",
        "clusters",
        "fkm_fit(s)",
        "fkm_pred(s)",
        "fkm_inertia",
        "fkm_ARI",
        "skm_fit(s)",
        "skm_pred(s)",
        "skm_inertia",
        "skm_ARI",
    )
    print("\n" + " ".join(f"{h:>13}" for h in header))
    for r in results:
        print(
            f"{r['n_samples']:13,}"
            f" {r['n_features']:13}"
            f" {r['n_clusters']:13}"
            f" {r['fkm_fit']:13.4f}"
            f" {r['fkm_predict']:13.4f}"
            f" {r['fkm_inertia']:13.2f}"
            f" {r['fkm_ari']:13.4f}"
            f" {r['skm_fit']:13.4f}"
            f" {r['skm_predict']:13.4f}"
            f" {r['skm_inertia']:13.2f}"
            f" {r['skm_ari']:13.4f}"
        )


if __name__ == "__main__":
    main()
