compiler_fortran := gfortran
compiler_clang   := gcc
mod              := -J mod/
opt              := -O3
option           := -cpp -D_x86_64 -fopenmp -fbounds-check
option_blas      := -lopenblas

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

common           := ./mod/mod_const.out \
					./mod/mod_common.out \
					./mod/mod_timer.out \
					./mod/mod_error.out \
					./mod/mod_random.out \
					./mod/mod_sort.out \
					./mod/mod_math.out \
					./mod/mod_stats.out \
					./mod/mod_metric.out \
					./mod/mod_linalg.out \
					./mod/mod_optimization.out \
					\
					./mod/mod_hyperparameter.out \
					./mod/mod_discretizer.out \
					./mod/mod_scaler.out \
					./mod/mod_data_holder.out \
					\
					./mod/mod_kmeans.out \
					./mod/mod_breathing_kmeans.out \
					\
					./mod/mod_linear_regression.out \
					\
					./mod/mod_node.out \
					./mod/mod_splitter.out \
					./mod/mod_woodworking_tools.out \
					./mod/mod_base_tree.out \
					./mod/mod_decision_tree.out \
					./mod/mod_extra_tree.out \
					./mod/mod_clouds.out \
					./mod/mod_lawu.out \
					./mod/mod_sadt.out \
					./mod/mod_isolation_tree.out \
					\
					./mod/mod_isolation_forest.out \

build:
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_const.f90                                                                -o ./mod/mod_const.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_common.f90                                                               -o ./mod/mod_common.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_timer.f90                                                                -o ./mod/mod_timer.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../error/mod_error.f90                                                                 -o ./mod/mod_error.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_random.f90                                                               -o ./mod/mod_random.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_sort.f90                                                                 -o ./mod/mod_sort.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../math/mod_math.f90                                                                   -o ./mod/mod_math.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../math/mod_stats.f90                                                                  -o ./mod/mod_stats.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../metric/mod_metric.f90                                                               -o ./mod/mod_metric.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_linalg.f90                                                               -o ./mod/mod_linalg.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../optimization/mod_optimization.f90                                                   -o ./mod/mod_optimization.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../hyperparameter/mod_hyperparameter.f90                                               -o ./mod/mod_hyperparameter.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../preprocessing/mod_discretizer.f90                                                   -o ./mod/mod_discretizer.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../preprocessing/mod_scaler.f90                                                        -o ./mod/mod_scaler.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../data_holder/mod_data_holder.f90                                                     -o ./mod/mod_data_holder.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../clustering/mod_kmeans.f90                                                           -o ./mod/mod_kmeans.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_node.f90                                                          -o ./mod/mod_node.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_splitter.f90                                                      -o ./mod/mod_splitter.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_woodworking_tools.f90                                             -o ./mod/mod_woodworking_tools.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_base_tree.f90                                                     -o ./mod/mod_base_tree.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_node.f90                                                          -o ./mod/mod_node.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_decision_tree.f90                                                 -o ./mod/mod_decision_tree.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_extra_tree.f90                                                    -o ./mod/mod_extra_tree.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_clouds.f90                                                        -o ./mod/mod_clouds.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_lawu.f90                                                          -o ./mod/mod_lawu.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_sadt.f90                                                          -o ./mod/mod_sadt.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_isolation_tree.f90                                                -o ./mod/mod_isolation_tree.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../forest/mod_isolation_forest.f90                                                     -o ./mod/mod_isolation_forest.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linear_regression/mod_linear_regression.f90                                         -o ./mod/mod_linear_regression.out
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
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../linalg/include/linalg_matrix_sqsum_row/inc_matrix_sqsum_row.c                             -o ./mod/inc_matrix_sqsum_row.out

main:
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../clustering/mod_kmeans.f90 -o ./mod/mod_kmeans.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../clustering/mod_breathing_kmeans.f90 -o ./mod/mod_breathing_kmeans.out
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main.f90 -o main.out \
	$(option_blas)
	./main.out
