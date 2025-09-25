# -*- coding: utf-8 -*-
"""
共通の import 補助関数
"""
import ctypes, importlib, importlib.util, pathlib, sys


def _patch_shared_loaders(monkeypatch, fake_lib):
    monkeypatch.setattr(ctypes, "CDLL", lambda path, *a, **k: fake_lib, raising=True)
    monkeypatch.setattr(ctypes.cdll, "LoadLibrary", lambda path, *a, **k: fake_lib, raising=True)
    monkeypatch.setattr(ctypes, "PyDLL", lambda path, *a, **k: fake_lib, raising=False)
    try:
        import numpy.ctypeslib as ncl
        monkeypatch.setattr(ncl, "load_library", lambda name, path=None: fake_lib, raising=True)
    except Exception:
        pass


def _ensure_importable(module_name: str):
    spec = importlib.util.find_spec(module_name)
    if spec: return
    here = pathlib.Path(__file__).resolve()
    for p in [here] + list(here.parents):
        if (p / "flearner").exists():
            if str(p) not in sys.path:
                sys.path.insert(0, str(p))
            break
    spec = importlib.util.find_spec(module_name)
    assert spec, f"Cannot find module: {module_name}"
