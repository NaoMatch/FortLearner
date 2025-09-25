# テスト仕様一覧（docs/tests）

本ディレクトリは `FortLearner/tests` のフォルダ構造を模倣し、実装済みテストの目的・確認内容を簡潔にまとめています。詳細は各サブディレクトリの index を参照してください。

- fortran
  - kmeans: Fortran 実装の KMeans に対する異常系/正常系テスト
- python
  - unit/common: 共有ユーティリティや基盤クラスの単体テスト
  - unit/algorithms/kmeans: Python 側ラッパ（ctypes 結線含む）のテスト

