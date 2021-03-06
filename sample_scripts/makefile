
asm              := ./inc_sum_up_c.out \
                    ./inc_sum_up_left_c.out  \
					./inc_get_minmax.out

compiler_fortran := gfortran

compiler_clang   := gcc

# Preprocessor
# -D_default: use Fortran only
# -D_x86_64:  use Fortran & inline assembler with x86_64
option := -cpp -D_x86_64

run:
	make run_utils
	make run_ml

build:
	$(compiler_clang) $(option) -c ../math/include/stats/sum_up/inc_sum_up.c           -O3 -o inc_sum_up_c.out
	$(compiler_clang) $(option) -c ../math/include/stats/sum_up_left/inc_sum_up_left.c -O3 -o inc_sum_up_left_c.out
	$(compiler_clang) $(option) -c ../math/include/stats/get_minmax/inc_get_minmax.c   -O3 -o inc_get_minmax.out

run_utils:
	make main_cholesky_decomposition
	make main_collect_unique_values
	make main_count_unique
	make main_discretizer
	make main_get_abs_maxloc
	make main_get_minmax
	make main_groupby
	make main_hyperparameter
	make main_identity
	make main_inner_product
	make main_inv_unit_lower_matrix
	make main_is_sorted
	make main_mattxmat
	make main_mean_variance_covariance
	make main_metric
	make main_minmax
	make main_most_left_bit_position
	make main_multi_mat_vec
	make main_optimize
	make main_progress_bar
	make main_rand_integer
	make main_rand_normal
	make main_random_rotation
	make main_read2bin
	make main_roulette_selection
	make main_scaler
	make main_search
	make main_sort
	make main_sum_up
	make main_sum_of_matrix
	make main_sum_up_left
	make main_vector2sum1


run_ml:
	make main_decision_tree
	# make main_deep_forest
	make main_gradient_boosting_tree
	make main_forest
	make main_linear_regression
	make main_logistic_regression
	make main_pca
	make main_sgd_regressor


main_cholesky_decomposition:
	$(compiler_fortran) $(option) -w -unroll -O3 -ftree-vectorize \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 ../linalg/mod_linalg.f90 \
	$(asm)  \
	main_cholesky_decomposition.f90 -o main_cholesky_decomposition.out
	./main_cholesky_decomposition.out

main_collect_unique_values:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 main_collect_unique_values.f90 -o main_collect_unique_values.out
	./main_collect_unique_values.out

main_count_unique:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 main_count_unique.f90 -o main_count_unique.out
	./main_count_unique.out

main_data_holder:
	$(compiler_fortran) -w -fbounds-check \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../linalg/mod_linalg.f90 \
	../math/mod_stats.f90 \
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	../data_holder/mod_data_holder.f90 \
	$(asm)  \
	main_data_holder.f90 \
	-o main_data_holder.out
	./main_data_holder.out


main_decision_tree:
	$(compiler_fortran) \
	-w -O3 -unroll \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../error/mod_error.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../metric/mod_metric.f90 \
	../linalg/mod_linalg.f90 \
	$(asm)  \
	\
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	../data_holder/mod_data_holder.f90 \
	\
	../decision_tree/mod_node.f90 \
	../decision_tree/mod_splitter.f90 \
	../decision_tree/mod_woodworking_tools.f90 \
	../decision_tree/mod_base_tree.f90 \
	../decision_tree/mod_decision_tree.f90 \
	../decision_tree/mod_extra_tree.f90 \
	../decision_tree/mod_clouds.f90 \
	../decision_tree/mod_lawu.f90 \
	../decision_tree/mod_weighted_oblique_decision_tree.f90 \
	\
	./main_decision_tree.f90 \
	-o main_decision_tree.out
	./main_decision_tree.out

main_deep_forest:
	$(compiler_fortran) \
	-w -O3 -unroll \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../error/mod_error.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../metric/mod_metric.f90 \
	../linalg/mod_linalg.f90 \
	$(asm)  \
	\
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	../data_holder/mod_data_holder.f90 \
	\
	../decision_tree/mod_node.f90 \
	../decision_tree/mod_splitter.f90 \
	../decision_tree/mod_woodworking_tools.f90 \
	../decision_tree/mod_base_tree.f90 \
	../decision_tree/mod_decision_tree.f90 \
	../decision_tree/mod_extra_tree.f90 \
	../decision_tree/mod_clouds.f90 \
	../decision_tree/mod_lawu.f90 \
	\
	../boosting_trees/mod_gradient_boosting_tree.f90 \
	\
	../forest/mod_forest.f90 \
	../forest/mod_deep_forest.f90 \
	\
	./main_deep_forest.f90 \
	-o main_deep_forest.out
	./main_deep_forest.out


main_discretizer:
	$(compiler_fortran) -w -O3 \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../linalg/mod_linalg.f90 \
	../math/mod_stats.f90 \
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	main_discretizer.f90 \
	$(asm)  \
	-o main_discretizer.out
	./main_discretizer.out

main_forest:
	$(compiler_fortran) \
	-w -O3 -unroll \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../error/mod_error.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../metric/mod_metric.f90 \
	../linalg/mod_linalg.f90 \
	$(asm)  \
	\
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	../data_holder/mod_data_holder.f90 \
	\
	../decision_tree/mod_node.f90 \
	../decision_tree/mod_splitter.f90 \
	../decision_tree/mod_woodworking_tools.f90 \
	../decision_tree/mod_base_tree.f90 \
	../decision_tree/mod_decision_tree.f90 \
	../decision_tree/mod_extra_tree.f90 \
	\
	../forest/mod_forest.f90 \
	\
	./main_forest.f90 \
	-o main_forest.out
	./main_forest.out

main_get_abs_maxloc:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 main_get_abs_maxloc.f90 -o main_get_abs_maxloc.out
	./main_get_abs_maxloc.out
	
main_get_minmax:
	$(compiler_fortran) -w -unroll -O3 -ftree-vectorize ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90  ../math/mod_stats.f90 main_get_minmax.f90 -o main_get_minmax.out \
	$(asm)
	./main_get_minmax.out

main_gradient_boosting_tree:
	$(compiler_fortran) \
	-w -O3 -unroll \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../error/mod_error.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../metric/mod_metric.f90 \
	../linalg/mod_linalg.f90 \
	$(asm)  \
	\
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	../data_holder/mod_data_holder.f90 \
	\
	../decision_tree/mod_node.f90 \
	../decision_tree/mod_splitter.f90 \
	../decision_tree/mod_woodworking_tools.f90 \
	../decision_tree/mod_base_tree.f90 \
	../decision_tree/mod_decision_tree.f90 \
	../decision_tree/mod_extra_tree.f90 \
	../decision_tree/mod_clouds.f90 \
	../decision_tree/mod_lawu.f90 \
	\
	../forest/mod_forest.f90 \
	../boosting_trees/mod_gradient_boosting_tree.f90 \
	\
	./main_gradient_boosting_tree.f90 \
	-o main_gradient_boosting_tree.out
	./main_gradient_boosting_tree.out

main_groupby:
	$(compiler_fortran) -w -fbounds-check -unroll -O3 -ftree-vectorize ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90  ../math/mod_stats.f90 main_groupby.f90 -o main_groupby.out \
	$(asm) 
	./main_groupby.out

main_hyperparameter:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 ../hyperparameter/mod_hyperparameter.f90 main_hyperparameter.f90 -o main_hyperparameter.out
	./main_hyperparameter.out

main_identity:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 main_identity.f90 -o main_identity.out
	./main_identity.out

main_inner_product:
	$(compiler_fortran) -w -unroll -O2 \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 ../linalg/mod_linalg.f90 \
	$(asm)  \
	main_inner_product.f90 -o main_inner_product.out
	./main_inner_product.out

main_inv_unit_lower_matrix:
	$(compiler_fortran) -w -unroll -O3 -ftree-vectorize \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 ../linalg/mod_linalg.f90 \
	$(asm)  \
	main_inv_unit_lower_matrix.f90 -o main_inv_unit_lower_matrix.out
	./main_inv_unit_lower_matrix.out

main_is_sorted:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 main_is_sorted.f90 -o main_is_sorted.out
	./main_is_sorted.out

main_linear_regression:
	$(compiler_fortran) -w -fbounds-check \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../error/mod_error.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../linalg/mod_linalg.f90 \
	../metric/mod_metric.f90 \
	$(asm)  \
	\
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	../data_holder/mod_data_holder.f90 \
	\
	../linear_regression/mod_linear_regression.f90 \
	main_linear_regression.f90 \
	-o main_linear_regression.out
	./main_linear_regression.out

main_logistic_regression:
	$(compiler_fortran) -w -O3 -ftree-vectorize \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../math/mod_math.f90 \
	../error/mod_error.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../linalg/mod_linalg.f90 \
	../metric/mod_metric.f90 \
	$(asm)  \
	\
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	../data_holder/mod_data_holder.f90 \
	\
	../optimization/mod_optimization.f90 \
	\
	../logistic_regression/mod_logistic_regression.f90 \
	main_logistic_regression.f90 \
	-o main_logistic_regression.out
	./main_logistic_regression.out

main_math:
	$(compiler_fortran) -w \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../math/mod_math.f90 main_math.f90 -o main_math.out
	./main_math.out

main_mattxmat:
	$(compiler_fortran) -w -unroll -O3 \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	$(asm)  \
	../linalg/mod_linalg.f90 main_mattxmat.f90 -o main_mattxmat.out
	./main_mattxmat.out

main_mean_variance_covariance:
	$(compiler_fortran) -w -unroll -O3 -ftree-vectorize -fbounds-check \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90 \
	$(asm)  \
	../math/mod_stats.f90 main_mean_variance_covariance.f90 -o main_mean_variance_covariance.out
	./main_mean_variance_covariance.out

main_metric:
	$(compiler_fortran) -w -w \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../metric/mod_metric.f90 \
	main_metric.f90 \
	$(asm)  \
	-o main_metric.out
	./main_metric.out

main_minmax:
	$(compiler_fortran) -w -unroll -O3 \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	$(asm) \
	main_minmax.f90 -o main_minmax.out
	./main_minmax.out

main_most_left_bit_position:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 main_most_left_bit_position.f90 -o main_most_left_bit_position.out
	./main_most_left_bit_position.out

main_multi_mat_vec:
	$(compiler_fortran) -w -unroll -O3 -ftree-vectorize \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	$(asm)  \
	../math/mod_stats.f90 ../linalg/mod_linalg.f90 main_multi_mat_vec.f90 -o main_multi_mat_vec.out
	./main_multi_mat_vec.out

main_optimize:
	$(compiler_fortran) -O2 -fbounds-check -w \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../math/mod_math.f90 \
	../error/mod_error.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../linalg/mod_linalg.f90 \
	../metric/mod_metric.f90 \
	$(asm)  \
	\
	../optimization/mod_optimization.f90 \
	\
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	../data_holder/mod_data_holder.f90 \
	\
	main_optimize.f90 \
	-o main_optimize.out
	./main_optimize.out

main_progress_bar:
	$(compiler_fortran) -w -fbounds-check ../common/mod_const.f90 ../common/mod_common.f90 main_progress_bar.f90 -o main_progress_bar.out
	./main_progress_bar.out

main_pca:
	$(compiler_fortran) -w -unroll -O3 -ftree-vectorize -fbounds-check \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../error/mod_error.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../linalg/mod_linalg.f90 ../linalg/mod_eig.f90 \
	../decomposition/mod_pca.f90 \
	$(asm)  \
	main_pca.f90 -o main_pca.out
	./main_pca.out

	
main_rand_integer:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_random.f90 main_rand_integer.f90 -o main_rand_integer.out
	./main_rand_integer.out

main_rand_normal:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_random.f90 main_rand_normal.f90 -o main_rand_normal.out
	./main_rand_normal.out

main_random_rotation:
	$(compiler_fortran) -w -unroll -O3 \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	$(asm)  \
	../linalg/mod_linalg.f90 main_random_rotation.f90 -o main_random_rotation.out
	./main_random_rotation.out

main_read2bin:
	$(compiler_fortran) -w -fbounds-check ../common/mod_const.f90 ../common/mod_common.f90 main_read2bin.f90 -o main_read2bin.out
	./main_read2bin.out

main_roulette_selection:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_random.f90 main_roulette_selection.f90 -o main_roulette_selection.out
	./main_roulette_selection.out

main_scaler:
	$(compiler_fortran) -w -w -fbounds-check \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../error/mod_error.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	\
	../linalg/mod_linalg.f90 \
	../math/mod_stats.f90 \
	$(asm)  \
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	../preprocessing/mod_scaler.f90 \
	main_scaler.f90 \
	-o main_scaler.out
	./main_scaler.out


main_search:
	$(compiler_fortran) -w ../common/mod_const.f90 ../common/mod_common.f90 main_search.f90 -o main_search.out
	./main_search.out

main_sgd_regressor:
	$(compiler_fortran) \
	-w -unroll -O3 \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../error/mod_error.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../metric/mod_metric.f90 \
	../linalg/mod_linalg.f90 \
	\
	../hyperparameter/mod_hyperparameter.f90 \
	../preprocessing/mod_discretizer.f90 \
	../preprocessing/mod_scaler.f90 \
	../data_holder/mod_data_holder.f90 \
	\
	../linear_regression/mod_linear_regression.f90 \
	../sgd_estimator/mod_sgd_estimator.f90 \
	$(asm)  \
	main_sgd_regressor.f90 -o main_sgd_regressor.out
	./main_sgd_regressor.out

main_sort:
	$(compiler_fortran) -w -O3 -unroll ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../math/mod_math.f90 ../common/mod_sort.f90 main_sort.f90 -o main_sort.out
	./main_sort.out

main_sum_up:
	$(compiler_fortran) -w -unroll -O3 \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	$(asm)  $(option) \
	main_sum_up.f90 -o main_sum_up.out
	./main_sum_up.out

main_sum_of_matrix:
	$(compiler_fortran) -w -unroll -O3 \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	$(asm)  \
	main_sum_of_matrix.f90 -o main_sum_of_matrix.out
	./main_sum_of_matrix.out

main_sum_up_left:
	$(compiler_fortran) -w -unroll -O3 -ftree-vectorize \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	$(asm) \
	main_sum_up_left.f90 -o main_sum_up_left.out
	./main_sum_up_left.out

main_vector2sum1:
	$(compiler_fortran) -w -unroll -O3 -ftree-vectorize \
	../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 \
	../common/mod_random.f90 ../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../linalg/mod_linalg.f90 \
	$(asm)  \
	main_vector2sum1.f90 -o main_vector2sum1.out
	./main_vector2sum1.out

