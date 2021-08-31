compiler_fortran := gfortran
compiler_clang   := gcc
mod              := -J mod/
opt              := -O3
option           := -cpp -D_x86_64 -fopenmp -fbounds-check

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

common           := 	./mod/mod_const.out \
						./mod/mod_common.out \
						./mod/mod_timer.out \
						./mod/mod_error.out \
						./mod/mod_random.out \
						./mod/mod_sort.out \
						./mod/mod_stats.out \
						./mod/mod_metric.out \
						./mod/mod_linalg.out \
						\
						./mod/mod_hyperparameter.out \
						./mod/mod_discretizer.out \
						./mod/mod_data_holder.out \
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

build:
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_const.f90                    -o ./mod/mod_const.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_common.f90                   -o ./mod/mod_common.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_timer.f90                    -o ./mod/mod_timer.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../error/mod_error.f90                     -o ./mod/mod_error.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_random.f90                   -o ./mod/mod_random.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_sort.f90                     -o ./mod/mod_sort.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../math/mod_stats.f90                      -o ./mod/mod_stats.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../metric/mod_metric.f90                   -o ./mod/mod_metric.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../linalg/mod_linalg.f90                   -o ./mod/mod_linalg.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../hyperparameter/mod_hyperparameter.f90   -o ./mod/mod_hyperparameter.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../preprocessing/mod_discretizer.f90       -o ./mod/mod_discretizer.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../data_holder/mod_data_holder.f90         -o ./mod/mod_data_holder.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_node.f90                -o ./mod/mod_node.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_splitter.f90            -o ./mod/mod_splitter.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_woodworking_tools.f90   -o ./mod/mod_woodworking_tools.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_base_tree.f90           -o ./mod/mod_base_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_node.f90                -o ./mod/mod_node.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_decision_tree.f90       -o ./mod/mod_decision_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_extra_tree.f90          -o ./mod/mod_extra_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_clouds.f90          -o ./mod/mod_clouds.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_lawu.f90          -o ./mod/mod_lawu.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../linear_regression/mod_linear_regression.f90          -o ./mod/mod_linear_regression.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up/inc_sum_up_vector.c                                       -o ./mod/inc_sum_up_vector_c.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up/inc_sum_up_matrix.c                                       -o ./mod/inc_sum_up_matrix_c.out ./mod/inc_sum_up_vector_c.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up_gt/inc_sum_up_gt.c                                        -o ./mod/inc_sum_up_gt_c.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/get_minmax/inc_get_minmax.c                                      -o ./mod/inc_get_minmax.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/get_minmax/inc_get_matrix_minmax.c                               -o ./mod/inc_get_matrix_minmax.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/variance_value_of_vector/inc_variance_value_of_vector.c          -o ./mod/inc_variance_value_of_vector.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up_gt/inc_get_matrix_count_and_sum_up_gt.c                   -o ./mod/inc_get_matrix_count_and_sum_up_gt.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../linalg/include/linalg_ax_plus_y/inc_ax_plus_y.c                                     -o ./mod/inc_ax_plus_y.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.c    -o ./mod/inc_covariance_value_of_vectors.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/get_minmax_vector2vector/inc_get_minmax_vector2vector.c    -o ./mod/inc_get_minmax_vector2vector.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/get_minmax/inc_get_matrix_minmax_parallel.c                      -o ./mod/inc_get_matrix_minmax_parallel.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up_gt/inc_get_count_and_sum_up_gt_vector2vector.c            -o ./mod/inc_get_count_and_sum_up_gt_vector2vector.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up_gt/inc_get_matrix_count_and_sum_up_gt_parallel.c          -o ./mod/inc_get_matrix_count_and_sum_up_gt_parallel.out

test:
	make main_decision_tree_clouds
	make main_decision_tree_decision_tree
	make main_decision_tree_extra_tree
	make main_decision_tree_lawu
	make main_linalg_ax_plus_y
	make main_linear_regression_lasso_regression
	make main_linear_regression_linear_regression
	make main_linear_regression_ridge_regression
	make main_stats_count_and_sum_up_gt
	make main_stats_count_and_sum_up_gt_matrix_with_index
	make main_stats_covariance_of_vectors
	make main_stats_get_minmax
	make main_stats_get_matrix_minmax_with_index
	make main_stats_get_matrix_minmax_with_index_parallel
	make main_stats_mean_matrix
	make main_stats_mean_vector
	make main_stats_sum_up_matrix
	make main_stats_sum_up_vector
	make main_stats_variance_vector
	rm main*.out
	rm *.bin

main_decision_tree_clouds:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_clouds.f90 -o main_decision_tree_clouds.out
	./main_decision_tree_clouds.out

main_decision_tree_decision_tree:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_decision_tree.f90 -o main_decision_tree_decision_tree.out
	./main_decision_tree_decision_tree.out

main_decision_tree_extra_tree:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_extra_tree.f90 -o main_decision_tree_extra_tree.out
	./main_decision_tree_extra_tree.out

main_decision_tree_lawu:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_lawu.f90 -o main_decision_tree_lawu.out
	./main_decision_tree_lawu.out

main_linalg_ax_plus_y:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_linalg_ax_plus_y.f90 -o main_linalg_ax_plus_y.out
	./main_linalg_ax_plus_y.out

main_linear_regression_lasso_regression:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_linear_regression_lasso_regression.f90 -o main_linear_regression_lasso_regression.out
	./main_linear_regression_lasso_regression.out

main_linear_regression_linear_regression:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_linear_regression_linear_regression.f90 -o main_linear_regression_linear_regression.out
	./main_linear_regression_linear_regression.out

main_linear_regression_ridge_regression:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_linear_regression_ridge_regression.f90 -o main_linear_regression_ridge_regression.out
	./main_linear_regression_ridge_regression.out

main_stats_count_and_sum_up_gt:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_count_and_sum_up_gt.f90 -o main_stats_count_and_sum_up_gt.out
	./main_stats_count_and_sum_up_gt.out

main_stats_covariance_of_vectors:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_covariance_of_vectors.f90 -o main_stats_covariance_of_vectors.out
	./main_stats_covariance_of_vectors.out

main_stats_get_minmax:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_get_minmax.f90 -o main_stats_get_minmax.out
	./main_stats_get_minmax.out

main_stats_get_matrix_minmax_with_index:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_get_matrix_minmax_with_index.f90 -o main_stats_get_matrix_minmax_with_index.out
	./main_stats_get_matrix_minmax_with_index.out

main_stats_get_matrix_minmax_with_index_parallel:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_get_matrix_minmax_with_index_parallel.f90 -o main_stats_get_matrix_minmax_with_index_parallel.out
	./main_stats_get_matrix_minmax_with_index_parallel.out

main_stats_count_and_sum_up_gt_matrix_with_index:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_count_and_sum_up_gt_matrix_with_index.f90 -o main_stats_count_and_sum_up_gt_matrix_with_index.out
	./main_stats_count_and_sum_up_gt_matrix_with_index.out

main_stats_mean_matrix:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_mean_matrix.f90 -o main_stats_mean_matrix.out
	./main_stats_mean_matrix.out

main_stats_mean_vector:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_mean_vector.f90 -o main_stats_mean_vector.out
	./main_stats_mean_vector.out

main_stats_sum_up_vector:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_sum_up_vector.f90 -o main_stats_sum_up_vector.out
	./main_stats_sum_up_vector.out

main_stats_sum_up_matrix:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_sum_up_matrix.f90 -o main_stats_sum_up_matrix.out
	./main_stats_sum_up_matrix.out

main_stats_variance_vector:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_variance_vector.f90 -o main_stats_variance_vector.out
	./main_stats_variance_vector.out
