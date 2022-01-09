compiler_fortran := gfortran
compiler_clang   := gcc
mod              := -J mod/
opt              := -O3 -funroll-loops -ffast-math -fprefetch-loop-arrays
option           := -cpp -D_x86_64 -fopenmp -fbounds-check
option_blas      := -lopenblas

common           := ./mod/mod_const.out \
					./mod/mod_common.out \
					./mod/mod_common_type.out \
					./mod/mod_timer.out \
					./mod/mod_error.out \
					./mod/mod_random.out \
					./mod/mod_hash.out \
					./mod/mod_sort.out \
					./mod/mod_math.out \
					./mod/mod_stats.out \
					./mod/mod_metric.out \
					./mod/mod_linalg.out \
					./mod/mod_hyperparameter.out \
					./mod/mod_hash_table.out \
					./mod/mod_nearest_neighbour.out \
					./mod/mod_exact_duplication_search.out \
					./mod/mod_brute_force_search.out \
					./mod/mod_kdtree.out \
					./mod/mod_balltree.out \
					./mod/mod_lsh.out \

asm              := ./mod/inc_sum_up_vector_c.out \
					./mod/inc_sum_up_matrix_c.out \
                    ./mod/inc_sum_up_gt_c.out     \
					./mod/inc_get_minmax.out      \
					./mod/inc_variance_value_of_vector.out \
					./mod/inc_get_matrix_minmax.out \
					./mod/inc_get_matrix_count_and_sum_up_gt.out \
					./mod/inc_ax_plus_y.out \
					./mod/inc_covariance_value_of_vectors.out \
					./mod/inc_get_minmax_vector2vector.out \
					./mod/inc_get_matrix_minmax_parallel.out \
					./mod/inc_get_count_and_sum_up_gt_vector2vector.out \
					./mod/inc_get_matrix_count_and_sum_up_gt_parallel.out \
					./mod/inc_multi_mat_vec.out \
					./mod/inc_matrix_sqsum_row.out \

build:
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_const.f90                                                                -o ./mod/mod_const.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_common.f90                                                               -o ./mod/mod_common.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_common_type.f90                                                          -o ./mod/mod_common_type.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_timer.f90                                                                -o ./mod/mod_timer.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../error/mod_error.f90                                                                 -o ./mod/mod_error.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_random.f90                                                               -o ./mod/mod_random.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_hash.f90                                                                 -o ./mod/mod_hash.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_sort.f90                                                                 -o ./mod/mod_sort.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../math/mod_math.f90                                                                   -o ./mod/mod_math.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../math/mod_stats.f90                                                                  -o ./mod/mod_stats.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../metric/mod_metric.f90                                                               -o ./mod/mod_metric.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_linalg.f90                                                               -o ./mod/mod_linalg.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../hyperparameter/mod_hyperparameter.f90                                               -o ./mod/mod_hyperparameter.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../hash_table/mod_hash_table.f90                                                       -o ./mod/mod_hash_table.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_nearest_neighbour.f90                                                 -o ./mod/mod_nearest_neighbour.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_brute_force_search.f90                                                -o ./mod/mod_brute_force_search.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_kdtree.f90                                                            -o ./mod/mod_kdtree.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_exact_duplication_search.f90                                          -o ./mod/mod_exact_duplication_search.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_balltree.f90                                                          -o ./mod/mod_balltree.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_lsh.f90                                                               -o ./mod/mod_lsh.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up/inc_sum_up_vector.c                                       -o ./mod/inc_sum_up_vector_c.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up/inc_sum_up_matrix.c                                       -o ./mod/inc_sum_up_matrix_c.out ./mod/inc_sum_up_vector_c.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up_gt/inc_sum_up_gt.c                                        -o ./mod/inc_sum_up_gt_c.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/get_minmax/inc_get_minmax.c                                      -o ./mod/inc_get_minmax.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/get_minmax/inc_get_matrix_minmax.c                               -o ./mod/inc_get_matrix_minmax.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/variance_value_of_vector/inc_variance_value_of_vector.c          -o ./mod/inc_variance_value_of_vector.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up_gt/inc_get_matrix_count_and_sum_up_gt.c                   -o ./mod/inc_get_matrix_count_and_sum_up_gt.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../linalg/include/linalg_ax_plus_y/inc_ax_plus_y.c                                     -o ./mod/inc_ax_plus_y.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.c    -o ./mod/inc_covariance_value_of_vectors.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/get_minmax_vector2vector/inc_get_minmax_vector2vector.c          -o ./mod/inc_get_minmax_vector2vector.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/get_minmax/inc_get_matrix_minmax_parallel.c                      -o ./mod/inc_get_matrix_minmax_parallel.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up_gt/inc_get_count_and_sum_up_gt_vector2vector.c            -o ./mod/inc_get_count_and_sum_up_gt_vector2vector.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up_gt/inc_get_matrix_count_and_sum_up_gt_parallel.c          -o ./mod/inc_get_matrix_count_and_sum_up_gt_parallel.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../linalg/include/linalg_multi_mat_vec/inc_multi_mat_vec.c                             -o ./mod/inc_multi_mat_vec.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../linalg/include/linalg_matrix_sqsum_row/inc_matrix_sqsum_row.c                       -o ./mod/inc_matrix_sqsum_row.out


main:
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_balltree.f90                                                          -o ./mod/mod_balltree.out
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main.f90 -o main.out \
	$(option_blas)
	./main.out
