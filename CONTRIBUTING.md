# Contributing to FortLearner

Thanks for your interest! We welcome issues and feedback.

## Development setup

Requirements (Linux):
- Python 3.9–3.12
- OpenBLAS, gfortran

Install:
```bash
python -m venv .venv && source .venv/bin/activate
pip install -U pip wheel build pytest
make lib
pip install -e .
```

Run tests:
```bash
pytest -q -m "not integration"    # unit (fast)
pytest -q -m integration           # integration (requires shared lib)
```

## Contribution style

- まずは Issue で提案/報告してください（仕様や設計の相談を優先します）
- 大きな変更や新機能は、必ず事前に Issue で合意形成してください
- メンテナンス体制の都合上、コードレビューのリソースは限られています（未事前相談の変更はレビューできない場合があります）

## Release

- Bump `setup.cfg: version` and `flearner/__init__.py: __version__`
- Build locally with `python -m build` and smoke test
- Publish via CI (TestPyPI → PyPI)
