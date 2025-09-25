# FortLearner ドキュメント索引

FortLearner は Fortran を中核とした機械学習ライブラリです。主要アルゴリズムは Fortran で実装され、次の2通りで利用できます。
- Fortran から直接利用（`use mod_*` ＋ 静的/共有ライブラリにリンク）
- Python から利用（薄いバインディング `flearner` が共有ライブラリをロード）

## クイックスタート

- Fortran 直利用
  1) ビルド: `make`（`libfortlearner.a` と `fortlearner/mod/*.mod` を生成）
  2) リンク例:
     `gfortran your.f90 -I fortlearner/mod -L . -lfortlearner -lopenblas -fopenmp`
- Python 経由
  1) 共有ライブラリ生成: `make lib`（`flearner/lib/libfortlearner.so`）
  2) インポート: `import flearner`（API は Fortran 実装を薄くラップ）

依存: gfortran, OpenBLAS（`-lopenblas`）, OpenMP（`-fopenmp`）

## 現状（未リリース）

- 安定版の PyPI 配布は準備中です（TestPyPI で検証中）。
- GitHub ではソースコードのみ公開し、生成物は CI でリリースアセットに添付します。
- manylinux wheel は OpenBLAS 同梱（Linux）。macOS/Windows は順次対応（当面はソースビルド）。

## リポジトリ構成（概要）
```
FortLearner/
├─ fortlearner/ # 中核の Fortran ソース（本体）。use で直接/共有ライブラリ経由の双方に対応
├─ flearner/ # 薄い Python バインディング＋補助。flearner/lib の共有ライブラリをロード
├─ docs/ # ドキュメント（このファイルを含む）
├─ tests/ # テスト一式（Python/Fortran）
├─ demos/ # 利用例（Fortran/Python）
│   └─ fortran/
│       ├─ bench_binary_search.f90         # make bench-search で利用する探索ベンチ
│       ├─ bench_weighted_sampling.f90     # make bench-weighted で利用する重み付きサンプリングベンチ
│       └─ verify_prefix_sum_selection.f90 # make verify-prefix-sum で利用する重み付きサンプリング検証
└─ README.md # プロジェクト概要
```

fortlearner 内の主な下位構成（詳細は下記リンク参照）:
- base_estimator: 抽象型と汎用ロジック
- kmeans: K-Means 本体（初期化・ループ・評価）
- math / stats: 数値演算・統計関数（例: clip, prefix sum）
- stat_transform: 前処理（例: MeanCenterer）
- utils: core（型/定数）, check / predicates / warn, distance, io（dump/load）, random, helpers, extra

## 設計ポリシーとガイド

以下は既存の設計メモ（実装方針諸々 / Predicates_Check_Warn 実装方針 / ハイパラチェック / モデル Dump/Load ガイドライン）の要点を統合したドキュメントです。別ファイルに依存せず、この節だけで判断できます。

### 実装方針（全体）

- 呼び出し設計: 例として `kmeans = new_kmeans()` → `kmeans%fit(X, ...)` の二段構成。
- 検証は二段階:
  - 生成時のハイパラチェック（ユーザが指定した値のみ対象）
  - `fit()` 時の入力・整合性チェック（行数や形状、値域など）
- モジュール分割: 機能ごとに細分化し、ビルド並列性と可読性を優先。
- 配列規約: 入出力は原則 2 次元。`X=(N, D)`, `y=(N, 1)` を基本とする。
- 型規約: 内部の基本型は `int64` / `real64`。一部アルゴリズムのみ例外（例: Bernoulli 系で `int16` 等）。
- 命名規約: ファイル名＝モジュール名（原則）。主要な公開手続きはモジュール名と対応づける（例外あり: 性質別に関数を束ねるユーティリティなど）。
- フォルダ構成（要旨）:
  ```
  fortlearner/
  ├── base_estimator      # 抽象型・共通ロジック
  ├── kmeans              # K-Means 本体（初期化・反復・評価）
  ├── math                # 数値演算ユーティリティ（例: clip）
  ├── stat_transform      # 前処理トランスフォーマ（例: MeanCenterer）
  ├── stats               # 統計関数（平均・分散など）
  └── utils
      ├── core            # 型・定数・設定
      ├── check           # `check_*`（不適なら error stop）
      ├── predicates      # `is_*`（pure elemental 判定）
      ├── warn            # `warn_*`（警告出力、停止しない）
      │   └── warn_cluster# クラスタ関連の警告
      ├── distance        # 距離計算
      ├── io              # dump/load（モデル・設定のI/O）
      ├── random          # 乱数・サンプリング
      ├── helpers         # 軽量ユーティリティ（文字列など）
      └── extra           # 付録・実験的コード
  ```

### Predicates / Check / Warn（命名・実装指針）

- 接頭辞の役割固定:
  - `is_...` → Predicate（真偽判定、原則 `pure elemental`）
  - `check_...` → Check（条件不成立でメッセージ出力＋`error stop`）
  - `warn_...` → Warn（条件不成立で警告出力、停止しない）
- 命名は肯定形・単一概念で統一。比較系は `ge/gt/le/lt` など略語を対称に使用。対象は `rows/cols/size` 等を明示。
- Predicate の原則: `pure elemental`、NaN/Inf は常に「望ましくない」→ `.false.`。実数同値判定は `is_close(x, y, atol, rtol)` を用意（既定例: `ATOL_R64=1e-10`, `RTOL_R64=1e-6`）。
- Check/Warn の出力形式（例）: `[file:class/value] must be <条件>, but got <実際値>.` を基本とし、可読性を統一。
- エラーコード: `ERR_` 前置で状態を表す名前（例: `ERR_NON_FINITE`, `ERR_TOO_FEW_ROWS`, `ERR_RELATION`）。比較系は共通コードを共有し、必要時のみ詳細化。
- ファイル構成ガイド: 規模が大きい場合は「1 ファイル 1 プロパティ」（例: `mod_is_positive.f90`, `mod_check_positive.f90`, `mod_warn_positive.f90`）。
- tol・閾値の扱い（KMeans 例）: 数値安定の観点で `tol > 0` を推奨。`tol = 0` を許容する場合は反復上限などの保険を必須化。
- Warn の一括制御: モジュール内 `logical, parameter :: ENABLE_WARNING` で早期 return を可能に。

## テストの実行

- Fortran ユニット/デモ実行（ビルド込み）
  - `make` または `make test`（`libfortlearner.a` を生成後、`tests/fortran` の実行ファイルを起動）
- Python ユニットテストのみ（共有ライブラリは Fake で差し替え）
  - `make ptest`
  - 例: 特定ファイルのみ実行 `make ptest PYTEST_ARGS="-q tests/python/unit/algorithms/kmeans/test_05_dataframe_inputs.py"`
  - 例: スキップ理由表示 `make ptest PYTEST_ARGS="-q -rs"`
- まとめて実行（Fortran→共有ライブラリ→Python→デモ）
  - `make allrun`
    - Fortran クリーン → Fortran テスト → 共有ライブラリビルド → ベンチマーク（bench-search / bench-weighted）→ Python テスト → Python デモ

備考:
- Python ユニットテスト（`tests/python/unit/...`）は `ctypes` のローダを Fake に差し替えて白箱検証を行います。
  実ライブラリは不要で、高速・安定に実行できます。
- 将来、実ライブラリを使う統合テストを追加する場合は、`tests/python/integration/...` に配置し、
  共有ライブラリが存在しない環境では `pytest.skip` する運用を推奨します。

### ハイパーパラメータ検証（設計）

- 最終的な検証は Fortran 側が担う（単一ソース・オブ・トゥルース）。Python 側は型/粗い範囲の軽量チェックに留める。
- 肯定形のプロパティに基づく検証関数を用意し、外れた場合は固有のエラーコードで `error stop`。
- 例:
  - `n_clusters` → `check_ge_i64(n_clusters, 2)`
  - `tol` → `check_non_negative(tol)`（または方針に応じて `check_positive_r64`）
- コンストラクタでは既定値は無検証、ユーザ指定値のみ検証対象。
 - Python 側ラッパ（例: FortKMeans）では Cerberus による基本スキーマ検証を実施（型・nullable など）。詳細は `docs/tests/python/unit/algorithms/kmeans/test_07_hparams_validation.md` 参照。

### モデル Dump/Load ガイドライン（後方互換）

- 目的: モデル内部状態（ハイパラ・学習済みパラメータ）を後方互換を保って保存/復元。
- ファイル形式:
  - `access='stream', form='unformatted'`
  - エンディアン: `convert='little_endian'`
  - 先頭に `int32 :: version = 1`（拡張時に +1）
- 継承と順序: 親 → 子の順で `dump`/`load`。新メンバー追加時はラッパーに追記して `version` を +1。旧版読み込みは `select case(version)` で分岐。
- 拡張例: `MiniBatchKMeans` はまず親（`KMeans`/`BaseKMeans`）を書き、その後に自身のメンバーを追記。
- テスト観点:
  - 同一バージョン互換: save → load → 等価性検証
  - 旧バージョン互換: 既存バイナリの読み込み確認
  - 大規模配列でもストリーム境界が正しいこと
  - Python 側から `np.fromfile(..., order='F')` で読めること
- 注意点:
  - 可変長データは長さ/形状を `int32/int64` で先出し
  - Fortran 列優先と Python の復元時に `order='F'`
  - 圧縮は未サポート（必要なら `compress=1` として version を +1）

## 次に読む

- Fortran からの利用例（`demos/fortran` と `tests/fortran`）
- Python からの利用例（`demos/python` と `tests/python`）
- KMeans の詳細（アルゴリズム要点・境界条件・計算量）: docs へ追補予定
- テスト仕様の索引: `docs/tests/index.md`

### 基盤関数ガイド（Fortran utils）

- 述語関数（`is_*`）: `docs/reference/述語関数ガイド.md`
- チェック関数（`check_*`）: `docs/reference/チェック関数ガイド.md`
- 警告関数（`warn_*`）: `docs/reference/警告ガイド.md`
 - 距離（distance）: `docs/reference/距離関数ガイド.md`
 - I/O（dump/load）: `docs/reference/I_Oガイド.md`
 - 乱数（random）: `docs/reference/乱数ガイド.md`
 - ヘルパー（helpers）: `docs/reference/ヘルパーガイド.md`

## エージェント向けクイックリンク

- Python API（人間向け要約）: `docs/python/api_index.md`
- Python API（機械可読 JSON）: `docs/python/api_index.json`
- Fortran シグネチャ（機械可読 JSON）: `docs/fortran/signatures.json`
- 依存マップ（概要）: `docs/ai/deps_map.md`
- 依存マップ（JSON）: `docs/ai/deps_map.json`
- 安定性マトリクス: `docs/ai/stability_matrix.md`
- 開発者向け 更新方針と手順: `docs/開発/更新方針と手順.md`

## 資料の区分（人間向け / AI 向け）

- 人間向け（Markdown）
  - `docs/index.md`（本ページ：入口）
  - `docs/開発/更新方針と手順.md`（リリース/運用ポリシー）
  - `docs/python/api_index.md`（Python API 概観）
  - `docs/python/flearner.FortKMeans.md`（API 詳細）
  - `docs/ai/deps_map.md`, `docs/ai/stability_matrix.md`（依存図/安定性）
- AI/自動化向け（機械可読）
  - `docs/python/api_index.json`（Python API 契約）
  - `docs/fortran/signatures.json`（Fortran バインディング署名）
  - `docs/ai/deps_map.json`（依存マップ JSON）

備考: 機械可読ファイルは `make gen-docs` で更新可能です（CI による整合性チェックを想定）。
