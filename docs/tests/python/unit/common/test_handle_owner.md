# test_handle_owner

- 対象: `HandleOwner` ユーティリティ
- 目的: ハンドル解放の一貫性とスレッド安全性の検証
- 確認:
  - close/detach により `free` がちょうど1回だけ呼ばれる（冪等）
  - `free_fn` は callable 必須（不正なら `TypeError`）
  - `_free_safely` は例外を握りつぶす（伝播しない）
  - `handle`/`closed` の状態遷移が仕様どおり
  - 10 スレッド同時 close でも `free` 呼び出しは1回
  - detach されていない `finalizer` の明示実行で `free` が1回呼ばれる

