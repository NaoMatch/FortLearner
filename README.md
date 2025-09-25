# FortLearner

ステータス: 未リリース（準備中）。安定版の PyPI 配布は近日公開予定です。テスト配布（TestPyPI）は利用可能です。

Fortran 製の高速な機械学習カーネルと、Python から扱うための軽量バインディングを提供します。

主なアルゴリズム:
- KMeans（k‑means++ 初期化、チャンク距離計算、OpenBLAS を用いた高速化）

対応プラットフォーム（現状）:
- Python 3.9–3.12
- Linux x86_64（manylinux wheel。同梱 OpenBLAS）
- macOS / Windows については順次対応予定（当面はソースビルド）

## インストール

安定版（準備中）:

```bash
pip install flearner
```

備考: 現在は未リリースのため、上記コマンドはまだ有効ではありません。以下の TestPyPI をご利用ください。

TestPyPI（テスト配布）:

```bash
pip install -i https://test.pypi.org/simple flearner==0.1.0 --extra-index-url https://pypi.org/simple
```

ソースから（共有ライブラリのビルドが必要）:

```bash
make lib         # flearner/lib/libfortlearner.* を生成
pip install -e . # 開発モードでインストール
```

失敗時のヒント（Linux）:

```bash
sudo apt-get install -y libopenblas-dev gfortran   # Ubuntu/Debian
sudo dnf install -y openblas-devel gcc-gfortran    # Fedora/RHEL
```

## 使い方（Python）

```python
import numpy as np
from flearner.FortKMeans import FortKMeans

X = np.asfortranarray([[0.0, 0.0], [1.0, 1.1], [10.0, -1.0]], dtype=np.float64)

km = FortKMeans(n_clusters=2, random_state=0)
km.fit(X)
labels = km.predict(X)
score  = km.score(X)
km.dump("model.bin")
km.close()

km2 = FortKMeans.load("model.bin")
print(km2.predict(X))
km2.close()
```

詳細は `docs/python/model_persistence.md` も参照してください。

## 配布ポリシー（PyPI / GitHub）

- PyPI（wheel/sdist）
  - manylinux wheel には OpenBLAS を同梱（静的/同梱リンク）し、`pip install` 直後に動作する形を提供します。
  - macOS/Windows も将来的に wheel 対応予定です（当面はソースビルド）。
- GitHub（ソース公開）
  - リポジトリ自体はソース中心で、生成済みバイナリはコミットしません。
  - リリース時は CI によりビルドした wheel/sdist を GitHub Releases のアセットとして提供します。

## ビルドとテスト

```bash
make lib                 # 共有ライブラリをビルド
pytest -q -m "not integration"   # ユニット（FakeLib ベース）
pytest -q -m integration         # 結合（共有ライブラリ必須、無ければ skip）
make allrun              # Fortranテスト→lib→Pythonテスト→デモ（lib未ロードならデモはskip）
```

## 貢献（Contributing）

バグ報告・提案は Issues にて歓迎します。変更提案や機能追加は、まず Issue で相談してください（合意形成を優先します）。開発の流れや規約は `CONTRIBUTING.md` を参照してください。

## ライセンス / サードパーティ

- 本体: MIT License（`LICENSE`）
- 同梱ライブラリ: OpenBLAS（BSD 3‑Clause）ほか
  - 詳細は `THIRD_PARTY_NOTICES.md` と `flearner/licenses/*` を参照

## 既知の制限

- 現状の wheel は Linux x86_64 のみ。同梱 OpenBLAS 前提です。
- macOS/Windows はソースからのビルドが必要（OpenBLAS と Fortran コンパイラを用意）。


---

Short English overview (for PyPI visitors):

FortLearner provides high‑performance machine learning kernels written in Fortran with thin Python bindings. Linux wheels bundle OpenBLAS so you can use it right after `pip install`. macOS/Windows support is planned; for now, build from source.
