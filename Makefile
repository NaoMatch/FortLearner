# ============================================================================ #
#  FortLearner – Makefile (build products → fortlearner/mod/)
#  * Build static library libfortlearner.a
#  * Unit tests        : test_*     (pass = stop 0)
#  * Expected-fail demos: fail_*    (must raise error/stop)
#  * Normal sample demo : demo_<algo>  (one per algorithm, reference only)
# ============================================================================ #

# ===== Fortran用変数 =====
SHELL    = /bin/bash

FC       = gfortran
SRCROOT  = fortlearner
BUILDDIR = $(SRCROOT)/mod
BINDIR   = build
FCFLAGS  = -cpp -O3 -fimplicit-none -fPIC -MMD -MP -J$(BUILDDIR)
# FCFLAGS  = -cpp -fimplicit-none -MMD -MP -J$(BUILDDIR) \
#            -ffpe-trap=zero,overflow,invalid -fbounds-check -fopenmp \
#            -Warray-temporaries -fdump-tree-original -fcheck=all -fbacktrace -g \
#            -fsanitize=address,undefined -fno-omit-frame-pointer -print-file-name=libasan.so \
#            -O2
LDFLAGS  = -L. -lfortlearner -lopenblas -fopenmp

# Verbose build control: make V=1 to show all commands and Fortran driver info
V ?= 0
ifeq ($(V),1)
  Q :=
  FCFLAGS += -v
else
  Q := @
endif

# Optional: specify OpenBLAS install dir to help the linker/loader
#   make lib OPENBLAS_DIR=/usr/lib/x86_64-linux-gnu
OPENBLAS_DIR ?=
ifneq ($(strip $(OPENBLAS_DIR)),)
  FCFLAGS += -I$(OPENBLAS_DIR)/include
  LDFLAGS += -L$(OPENBLAS_DIR)/lib -Wl,-rpath,$(OPENBLAS_DIR)/lib
endif

# Optional: statically link GCC/Fortran runtimes into the shared library
#   make lib STATIC_GCC=1
STATIC_GCC ?= 0
ifeq ($(STATIC_GCC),1)
  LDFLAGS += -static-libgfortran -static-libgcc
endif

# ===== Python用変数 =====
PYTHON      ?= python3
PYTEST      ?= $(PYTHON) -m pytest
TESTDIR     ?= tests/python
PYTEST_ARGS ?= -q

# マーカー（プロジェクト方針に合わせて変更可）
MARK_UNIT ?= not integration
MARK_INT  ?= integration


# -------------------- FortLearner Source --------------------
CORE_SRC :=  $(SRCROOT)/utils/core/mod_kinds.f90            \
             $(SRCROOT)/utils/core/mod_program_limits.f90   \
             $(SRCROOT)/utils/core/mod_config.f90           \
             $(SRCROOT)/utils/core/mod_c_to_f.f90           \
             $(SRCROOT)/utils/core/mod_openblas_threads.f90 \

IO_SRC    := $(SRCROOT)/utils/io/mod_dump_load.f90      \

EXTRA_SRC  := $(SRCROOT)/utils/extra/mod_data_summary.f90   \
              $(SRCROOT)/utils/extra/mod_datetime.f90       \
              $(SRCROOT)/utils/extra/mod_timer.f90       \

PREDICATES_SRC := $(SRCROOT)/utils/predicates/mod_is_close.f90                \
                  $(SRCROOT)/utils/predicates/mod_is_finite.f90               \
                  $(SRCROOT)/utils/predicates/mod_is_positive_sized_array.f90 \
                  $(SRCROOT)/utils/predicates/mod_is_allowed_option.f90       \
                  $(SRCROOT)/utils/predicates/mod_is_sign.f90                 \
                  $(SRCROOT)/utils/predicates/mod_is_in_range.f90             \
                  $(SRCROOT)/utils/predicates/mod_predicates.f90              \

HELPER_SRC := $(SRCROOT)/utils/helpers/mod_character_helpers.f90 \
              $(SRCROOT)/utils/helpers/mod_memory_helpers.f90    \
              $(SRCROOT)/utils/helpers/mod_bit_helpers.f90       \
              $(SRCROOT)/utils/helpers/mod_binary_search.f90     \

CHECK_SRC := $(SRCROOT)/utils/check/mod_error_codes.f90             \
             $(SRCROOT)/utils/check/mod_error_manager.f90           \
             $(SRCROOT)/utils/check/mod_check_positive.f90          \
             $(SRCROOT)/utils/check/mod_check_non_positive.f90      \
             $(SRCROOT)/utils/check/mod_check_negative.f90          \
             $(SRCROOT)/utils/check/mod_check_non_negative.f90      \
             $(SRCROOT)/utils/check/mod_check_in_range.f90          \
             $(SRCROOT)/utils/check/mod_check_allowed_option.f90    \
             $(SRCROOT)/utils/check/mod_check_finite.f90            \
             $(SRCROOT)/utils/check/mod_check_array_shape.f90       \
             $(SRCROOT)/utils/check/mod_check_relation.f90          \
             $(SRCROOT)/utils/check/mod_check_fitted_model.f90      \
             $(SRCROOT)/utils/check/mod_check_rows_distinct.f90     \
             $(SRCROOT)/utils/check/mod_check_format_version.f90    \
             $(SRCROOT)/utils/check/mod_check_file_exists.f90       \
             $(SRCROOT)/utils/check/mod_check.f90                   \
             $(SRCROOT)/utils/check/bindings/mod_error_bindings.f90 \

WARN_SRC := $(SRCROOT)/utils/warn/mod_warn_in_range.f90                   \
            $(SRCROOT)/utils/warn/mod_warn_array_size_within_limit.f90    \
            $(SRCROOT)/utils/warn/mod_warn_value_within_limit.f90         \
            $(SRCROOT)/utils/warn/mod_warn_non_zero_mean.f90              \
            $(SRCROOT)/utils/warn/warn_cluster/mod_warn_clusters_non_empty.f90 \

STATS_SRC := $(SRCROOT)/stats/mod_mean.f90 \

MATH_SRC  := $(SRCROOT)/math/mod_clip.f90 \
             $(SRCROOT)/math/mod_prefix_sum.f90 \

RANDOM_SRC := $(SRCROOT)/utils/random/mod_random_int.f90          \
              $(SRCROOT)/utils/random/mod_weighted_reservoir.f90  \
              $(SRCROOT)/utils/random/mod_random_seed.f90         \
              $(SRCROOT)/utils/random/mod_random_permutation.f90  \
              $(SRCROOT)/utils/random/mod_prefix_sum_selection.f90

DISTANCE_SRC := $(SRCROOT)/utils/distance/mod_euclidean_distance.f90

BASE_SRC := $(SRCROOT)/base_estimator/mod_base_estimator.f90

XFORM_SRC := $(SRCROOT)/stat_transform/mod_base_transformer.f90   \
             $(SRCROOT)/stat_transform/mod_mean_centerer.f90              

KMEANS_SRC := $(SRCROOT)/kmeans/mod_base_kmeans.f90                  \
              $(SRCROOT)/kmeans/mod_kmeans_fit_single_iter_dense.f90 \
              $(SRCROOT)/kmeans/mod_kmeans.f90                       \
              $(SRCROOT)/kmeans/bindings/mod_kmeans_registry.f90     \
              $(SRCROOT)/kmeans/bindings/mod_kmeans_binding.f90      \

LIB_SRC := \
    $(CORE_SRC) \
    $(IO_SRC) \
    $(EXTRA_SRC) \
    $(PREDICATES_SRC) \
    $(HELPER_SRC) \
    $(CHECK_SRC) \
    $(WARN_SRC) \
    $(STATS_SRC) \
    $(MATH_SRC) \
    $(RANDOM_SRC) \
    $(DISTANCE_SRC) \
    $(BASE_SRC) \
    $(XFORM_SRC) \
    $(KMEANS_SRC)

# ---------- object / dependency helpers ----------
o = $(BUILDDIR)/$(subst /,__,$(basename $(1))).o
d = $(o).d

LIB_OBJS := $(foreach s,$(LIB_SRC),$(call o,$(s)))
LIB_DEPS := $(LIB_OBJS:.o=.d)

LIB := libfortlearner.a

# ================================================================= targets ===
.PHONY: all
all: $(LIB)

$(LIB): $(LIB_OBJS)
	ar rcs $@ $^

# ---------- individual compile rule ----------
define compile_rule
$(call o,$(1)): $(1)
	$(Q)mkdir -p $(BUILDDIR)
	$(Q)$(FC) $(FCFLAGS) -c $$< -o $$@
endef
$(foreach s,$(LIB_SRC),$(eval $(call compile_rule,$(s))))

# ---------- auto-include dependency files ----------
-include $(LIB_DEPS)

# ── 1. Enumerate Fortran test/demo sources ───────────────────────────────
#   * invalid → expected-fail demos (fail_*)
#   * valid   → regular unit tests    (test_*)
#   * demo_*  → user sample programs  (demo_*)

FT_SRC_INVALID := $(wildcard tests/fortran/kmeans/ftest_*invalid*.f90)
FT_SRC_VALID   := $(wildcard tests/fortran/kmeans/ftest_*valid*.f90)

# 生成された実行ファイルをまとめるディレクトリ
BINDIR         := build/bin

# ---- executables ---------------------------------------------------------
FAIL_EXES := $(patsubst tests/fortran/kmeans/ftest_%.f90,$(BINDIR)/fail_%,$(FT_SRC_INVALID))
TEST_EXES := $(patsubst tests/fortran/kmeans/ftest_%.f90,$(BINDIR)/test_%,$(FT_SRC_INVALID) $(FT_SRC_VALID))

DEMO_SRC  := $(wildcard demos/fortran/samples/demo_*.f90)
DEMO_EXES := $(patsubst demos/fortran/demo_%.f90,$(BINDIR)/demo_%,$(DEMO_SRC))

# ---- pattern rules -------------------------------------------------------
# * expected-fail demo
$(BINDIR)/fail_%: tests/fortran/kmeans/ftest_%.f90 $(LIB)
	$(Q)mkdir -p $(BINDIR)
	$(Q)$(FC) $(FCFLAGS) -cpp -DFAIL $< -o $@ $(LDFLAGS)

# * unit-test executable
$(BINDIR)/test_%: tests/fortran/kmeans/ftest_%.f90 $(LIB)
	$(Q)mkdir -p $(BINDIR)
	$(Q)$(FC) $(FCFLAGS) -cpp -DTEST $< -o $@ $(LDFLAGS)

# * normal sample demo
$(BINDIR)/demo_%: demos/fortran/samples/demo_%.f90 $(LIB)
	$(Q)mkdir -p $(BINDIR)
	$(Q)$(FC) $(FCFLAGS) -cpp $< -o $@ $(LDFLAGS)

# ---- run tests -----------------------------------------------------------
.PHONY: test
test: $(FAIL_EXES) $(DEMO_EXES) $(TEST_EXES)
	@echo "Running tests..."
	@set -e; \
	cd $(BINDIR); \
	for t in $(notdir $(TEST_EXES)); do \
	    printf '\n== %s ==\n' "$$t" ; \
	    GFORTRAN_ERROR_BACKTRACE=0 ./$$t ; \
	done
	@echo "\033[32m✓ all tests passed\033[0m"

# ---- build shared library for Python binding ----------------------------
.PHONY: lib

LIBNAME = libfortlearner.so
LIBDIR  = flearner/lib

lib: $(LIBDIR)/$(LIBNAME)
	@echo "✓ built $(LIBDIR)/$(LIBNAME)"

$(LIBDIR)/$(LIBNAME): $(LIB_OBJS)
	$(Q)mkdir -p $(LIBDIR)
	$(Q)$(FC) $(FCFLAGS) -shared -fPIC $(LIB_OBJS) -o $(LIBDIR)/$(LIBNAME) $(filter-out -lfortlearner,$(LDFLAGS))

# ---- clean ---------------------------------------------------------------
.PHONY: clean
clean:
	@echo "Cleaning generated files..."
	$(Q)rm -rf $(BUILDDIR) $(BINDIR) $(LIBDIR)
	$(Q)rm -f $(LIB) $(FAIL_EXES) $(DEMO_EXES) $(TEST_EXES) $(LIBDIR)/$(LIBNAME) libfortlearner.a libfortlearner.so
	$(Q)find . -type f \( -name '*.o' -o -name '*.obj' -o -name '*.mod' -o -name '*.smod' -o -name '*.d' -o -name '*.gcno' -o -name '*.gcda' \) -print -delete 2>/dev/null || true



# ---- allrun --------------------------------------------------------------
DEMO   ?= demos/python/demo_kmeans.py
DEMO_ARGS ?=

.PHONY: demo allrun
demo: allrun
	@true

# 直列実行：どれかが失敗したら以降は実行されない

allrun:
	$(MAKE) clean
	$(MAKE) test
	$(MAKE) lib
	$(MAKE) bench
	$(PYTEST) $(TESTDIR) $(PYTEST_ARGS)
	$(MAKE) rundemo

.PHONY: bench-search
bench-search: $(LIB)
	@echo "[bench-search] building binary search benchmark"
	$(Q)mkdir -p $(BINDIR)
	$(FC) $(FCFLAGS) -I$(BUILDDIR) demos/fortran/benchmarks/bench_binary_search.f90 \
		-o $(BINDIR)/bench_binary_search $(LDFLAGS)
	@echo "[bench-search] running benchmark"
	$(BINDIR)/bench_binary_search

.PHONY: bench-weighted
bench-weighted: $(LIB)
	@echo "[bench-weighted] building weighted sampling benchmark"
	$(Q)mkdir -p $(BINDIR)
	$(FC) $(FCFLAGS) -I$(BUILDDIR) demos/fortran/benchmarks/bench_weighted_sampling.f90 \
		-o $(BINDIR)/bench_weighted_sampling $(LDFLAGS)
	@echo "[bench-weighted] running benchmark"
	$(BINDIR)/bench_weighted_sampling

.PHONY: bench
bench:
	$(MAKE) bench-search
	$(MAKE) bench-weighted

.PHONY: verify-prefix-sum
verify-prefix-sum: $(LIB)
	@echo "[verify-prefix-sum] building prefix-sum sampling verifier"
	$(Q)mkdir -p $(BINDIR)
	$(FC) $(FCFLAGS) -I$(BUILDDIR) demos/fortran/verification/verify_prefix_sum_selection.f90 \
		-o $(BINDIR)/verify_prefix_sum_selection $(LDFLAGS)
	@echo "[verify-prefix-sum] running verification"
	$(BINDIR)/verify_prefix_sum_selection

.PHONY: rundemo
rundemo:
	@# Run all Python demos under demos/python (simple execution only)
	@set -e; \
	DEMOS=$$(ls demos/python/*.py 2>/dev/null || true); \
	if [ -z "$$DEMOS" ]; then \
	  echo "[demo] No Python demos found under demos/python/"; \
	  exit 0; \
	fi; \
	for d in $$DEMOS; do \
	  echo "[demo] Running $$d"; \
	  $(PYTHON) $$d $(DEMO_ARGS); \
	done

# -------- diagnostics / listing --------------------------------------------
.PHONY: print-config list-src list-objs
print-config:
	@echo FC=$(FC)
	@echo FCFLAGS=$(FCFLAGS)
	@echo LDFLAGS=$(LDFLAGS)
	@echo OPENBLAS_DIR=$(OPENBLAS_DIR)
	@echo STATIC_GCC=$(STATIC_GCC)
	@echo V=$(V)

list-src:
	@echo "LIB_SRC:"; echo $(LIB_SRC) | tr ' ' '\n'

list-objs:
	@echo "LIB_OBJS:"; echo $(LIB_OBJS) | tr ' ' '\n'
# ---- python tests only ----------------------------------------------------
.PHONY: ptest
ptest:
	$(PYTEST) $(TESTDIR) $(PYTEST_ARGS)
# ============================================================================ #

# ---- docs/generation helpers ----------------------------------------------
.PHONY: gen-fortran-signatures gen-py-api gen-docs

gen-fortran-signatures:
	@echo "[gen] fortran bindings → docs/fortran/signatures.json"
	$(PYTHON) tools/gen_fortran_signatures.py > docs/fortran/signatures.json

gen-py-api:
	@echo "[gen] python api → docs/python/api_index.json"
	@# A dedicated generator script can be added; for now keep the curated JSON.
	@# $(PYTHON) tools/gen_py_api_json.py > docs/python/api_index.json

gen-docs: gen-fortran-signatures gen-py-api
	@echo "✓ generated docs artifacts"
