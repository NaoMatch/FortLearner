asm              := ./mod/inc_sum_up_vector_c.out \
					./mod/inc_sum_up_matrix_c.out \
                    ./mod/inc_sum_up_gt_c.out     \
					./mod/inc_get_minmax.out      \
					./mod/inc_variance_value_of_vector.out

compiler_fortran := gfortran
compiler_clang   := gcc
mod              := -J mod/
opt              := -O3

# Preprocessor
# -D_default: use Fortran only
# -D_x86_64:  use Fortran & inline assembler with x86_64
option := -cpp -D_x86_64 -fopenmp 


pre_f:
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_const.f90 -o ./mod/mod_const.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_common.f90 -o ./mod/mod_common.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_timer.f90 -o ./mod/mod_timer.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_random.f90 -o ./mod/mod_random.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_sort.f90 -o ./mod/mod_sort.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../math/mod_stats.f90 -o ./mod/mod_stats.out

pre_c:
	$(compiler_clang) $(mod) $(option) -c ../../math/include/stats/sum_up/inc_sum_up_vector.c                               $(opt) -o ./mod/inc_sum_up_vector_c.out
	$(compiler_clang) $(mod) $(option) -c ../../math/include/stats/sum_up/inc_sum_up_matrix.c                               $(opt) -o ./mod/inc_sum_up_matrix_c.out ./mod/inc_sum_up_vector_c.out
	$(compiler_clang) $(mod) $(option) -c ../../math/include/stats/sum_up_gt/inc_sum_up_gt.c                                $(opt) -o ./mod/inc_sum_up_gt_c.out
	$(compiler_clang) $(mod) $(option) -c ../../math/include/stats/get_minmax/inc_get_minmax.c                              $(opt) -o ./mod/inc_get_minmax.out
	$(compiler_clang) $(mod) $(option) -c ../../math/include/stats/variance_value_of_vector/inc_variance_value_of_vector.c  $(opt) -o ./mod/inc_variance_value_of_vector.out

main:
	# make pre_f
	# make pre_c
	$(compiler_fortran) -w -O2         \
	./mod/mod_const.out                   \
	./mod/mod_common.out                  \
	./mod/mod_timer.out                   \
	./mod/mod_random.out                  \
	./mod/mod_sort.out                    \
	./mod/mod_stats.out                   \
	$(asm) $(option) $(mod)               \
	main_mean_vector.f90 -o main_mean_vector.out
	./main_mean_vector.out
