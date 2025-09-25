from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

from setuptools import setup
from setuptools.command.build_py import build_py as _build_py
from wheel.bdist_wheel import bdist_wheel as _bdist_wheel


def _platform_lib_name() -> str:
    if sys.platform.startswith("win"):
        return "libfortlearner.dll"
    if sys.platform == "darwin":
        return "libfortlearner.dylib"
    return "libfortlearner.so"


class build_py(_build_py):
    """Custom build step to compile the Fortran shared library via Makefile.

    This keeps source distributions usable and ensures the wheel ships with
    flearner/lib/libfortlearner.* bundled.
    """

    def run(self):
        root = Path(__file__).resolve().parent
        make = os.environ.get("MAKE", "make")
        target = root / "flearner" / "lib"
        target.mkdir(parents=True, exist_ok=True)

        try:
            subprocess.check_call([make, "lib"], cwd=str(root))
        except Exception as e:
            # Do not hard fail here to keep sdist creation possible, but log a hint
            print("[build_py] warn: building shared library failed:", e)
            print("[build_py] you may need OpenBLAS and gfortran installed.")
            if os.environ.get("CIBUILDWHEEL"):
                raise
        super().run()

        lib_path = target / _platform_lib_name()
        if not lib_path.exists():
            msg = f"[build_py] expected shared library missing: {lib_path}"
            print(msg)
            if os.environ.get("CIBUILDWHEEL"):
                raise RuntimeError(msg)


class bdist_wheel(_bdist_wheel):
    """Mark wheel as platform-specific so bundled libs are kept."""

    def finalize_options(self):
        super().finalize_options()
        self.root_is_pure = False


setup(cmdclass={"build_py": build_py, "bdist_wheel": bdist_wheel})
