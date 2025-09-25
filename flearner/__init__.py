"""FortLearner Python bindings.

- Stable API: `FortKMeans` （トップレベルから遅延公開）
- Internal API: `fort_base_estimator`, `handle_owner` など

ネイティブ共有ライブラリのロードを伴うため、重いサブモジュールは
デフォルトではインポートしません（遅延ロード）。
"""

from __future__ import annotations
from typing import Any

__all__: list[str] = ["FortKMeans"]
__version__ = "0.1.0"

def __getattr__(name: str) -> Any:
    # Lazy export of stable API
    if name == "FortKMeans":
        from .FortKMeans import FortKMeans  # type: ignore
        return FortKMeans
    raise AttributeError(name)

def __dir__() -> list[str]:
    return sorted(list(globals().keys()) + __all__)
