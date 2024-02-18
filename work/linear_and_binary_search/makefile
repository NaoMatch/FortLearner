compiler_fortran := gfortran
compiler_clang   := gcc-8
mod              := -J mod/
opt              := -O3
option           := -cpp -D_x86_64 -fopenmp
option_degug     := 
option_blas      := -lopenblas -ldl

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

common           := 	./mod/mod_const.out \
						./mod/mod_common.out \
						./mod/mod_common_type.out \
						./mod/mod_heap.out \
						./mod/mod_timer.out \
						./mod/mod_error.out \
						./mod/mod_random.out \
						\
						./mod/mod_simple_tree.out \
						\
						./mod/mod_hash.out \
						./mod/mod_hash_table.out \
						./mod/mod_hash_map.out \
						./mod/mod_sort.out \
						./mod/mod_math.out \
						./mod/mod_stats.out \
						./mod/mod_metric.out \
						./mod/mod_linalg.out \
						./mod/mod_eig.out \
						./mod/mod_optimization.out \
						./mod/mod_union_find.out \
						\
						./mod/mod_hyperparameter.out \
						./mod/mod_discretizer.out \
						./mod/mod_data_holder.out \
						./mod/mod_scaler.out \
						./mod/mod_encoder.out \
						\
						./mod/mod_base_kmeans.out \
						./mod/mod_kmeans.out \
						./mod/mod_minibatch_kmeans.out \
						./mod/mod_breathing_kmeans.out \
						./mod/mod_dbscan.out \
						\
						./mod/mod_linear_regression.out \
						./mod/mod_logistic_regression.out \
						./mod/mod_sgd_estimator.out \
						\
						./mod/mod_node.out \
						./mod/mod_splitter.out \
						./mod/mod_woodworking_tools.out \
						./mod/mod_base_tree.out \
						./mod/mod_clouds.out \
						./mod/mod_decision_tree.out \
						./mod/mod_extra_tree.out \
						./mod/mod_lawu.out \
						./mod/mod_isolation_tree.out \
						./mod/mod_extended_isolation_tree.out \
						./mod/mod_random_rotation_decision_tree.out \
						./mod/mod_sadt.out \
						./mod/mod_sliq.out \
						./mod/mod_oblivious_tree.out \
						./mod/mod_threshold_tree.out \
						\
						./mod/mod_gradient_boosting_clouds.out \
						./mod/mod_gradient_boosting_extra_tree.out \
						./mod/mod_gradient_boosting_lawu.out \
						./mod/mod_gradient_boosting_tree.out \
						./mod/mod_gradient_boosting_adaboost.out \
						\
						./mod/mod_forest_jitter.out \
						./mod/mod_forest_packing_.out \
						./mod/mod_extra_trees.out \
						./mod/mod_random_forest.out \
						./mod/mod_isolation_forest.out \
						\
						./mod/mod_nearest_neighbour.out \
						./mod/mod_balltree.out \
						./mod/mod_exact_duplication_search.out \
						./mod/mod_brute_force_search.out \
						./mod/mod_kdtree.out \
						./mod/mod_lsh.out \
						./mod/mod_product_quantization.out \
						./mod/mod_k_nearest_neighbor_regressor.out \
						./mod/mod_k_nearest_neighbor_classifier.out \
						./mod/mod_local_outlier_factor.out \
						\
						./mod/mod_pca.out \
						./mod/mod_nipals.out \
						\
						./mod/mod_variable_in_variable.out \
						./mod/mod_wengert_list.out \
						./mod/mod_activation_function.out \
						./mod/mod_absolute_value.out \
						./mod/mod_power.out \
						./mod/mod_summation.out \
						./mod/mod_spread.out \
						./mod/mod_addition.out \
						./mod/mod_substraction.out \
						./mod/mod_multiplication.out \
						./mod/mod_division.out \
						./mod/mod_square.out \
						./mod/mod_square_root.out \
						./mod/mod_sinusoid.out \
						./mod/mod_cosine.out \
						./mod/mod_tangent.out \
						./mod/mod_arcsinusoid.out \
						./mod/mod_arccosine.out \
						./mod/mod_arctangent.out \
						./mod/mod_exponential.out \
						./mod/mod_log_natural.out \
						./mod/mod_sigmoid.out \
						./mod/mod_matmul.out \
						./mod/mod_relu.out \
						./mod/mod_dense.out \
						./mod/mod_softmax.out \
						./mod/mod_loss_function.out \
						./mod/mod_backwards.out \
						./mod/mod_my_mlp.out \
						./mod/mod_mnist_classifier.out \
						\
						./mod/mod_kernel.out \
						./mod/mod_svm_cache.out \
						./mod/mod_linear_svm.out \
						./mod/mod_kernel_svm.out \

clean:
	rm ./mod/*.mod
	rm ./mod/*.out
	rm ./*.out
	rm ./*.mod

build_all:
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_const.f90 -o ./mod/mod_const.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_common.f90 -o ./mod/mod_common.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_common_type.f90 -o ./mod/mod_common_type.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_heap.f90 -o ./mod/mod_heap.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_timer.f90 -o ./mod/mod_timer.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../error/mod_error.f90 -o ./mod/mod_error.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_random.f90 -o ./mod/mod_random.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_hash.f90 -o ./mod/mod_hash.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_sort.f90 -o ./mod/mod_sort.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_simple_tree.f90 -o ./mod/mod_simple_tree.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../math/mod_math.f90 -o ./mod/mod_math.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../math/mod_stats.f90 -o ./mod/mod_stats.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../metric/mod_metric.f90 -o ./mod/mod_metric.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_linalg.f90 -o ./mod/mod_linalg.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_eig.f90 -o ./mod/mod_eig.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../optimization/mod_optimization.f90 -o ./mod/mod_optimization.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../data_structure/mod_union_find.f90 -o ./mod/mod_union_find.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../hyperparameter/mod_hyperparameter.f90 -o ./mod/mod_hyperparameter.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../hash_table/mod_hash_table.f90 -o ./mod/mod_hash_table.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../hash_table/mod_hash_map.f90 -o ./mod/mod_hash_map.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../preprocessing/mod_discretizer.f90 -o ./mod/mod_discretizer.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../preprocessing/mod_scaler.f90 -o ./mod/mod_scaler.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../preprocessing/mod_encoder.f90 -o ./mod/mod_encoder.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../data_holder/mod_data_holder.f90 -o ./mod/mod_data_holder.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decomposition/mod_pca.f90 -o ./mod/mod_pca.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decomposition/mod_nipals.f90 -o ./mod/mod_nipals.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_nearest_neighbour.f90 -o ./mod/mod_nearest_neighbour.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_brute_force_search.f90 -o ./mod/mod_brute_force_search.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_kdtree.f90 -o ./mod/mod_kdtree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_balltree.f90 -o ./mod/mod_balltree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_k_nearest_neighbor_classifier.f90 -o ./mod/mod_k_nearest_neighbor_classifier.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_k_nearest_neighbor_regressor.f90 -o ./mod/mod_k_nearest_neighbor_regressor.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_local_outlier_factor.f90 -o ./mod/mod_local_outlier_factor.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../clustering/mod_base_kmeans.f90 -o ./mod/mod_base_kmeans.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../clustering/mod_kmeans.f90 -o ./mod/mod_kmeans.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../clustering/mod_minibatch_kmeans.f90 -o ./mod/mod_minibatch_kmeans.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../clustering/mod_breathing_kmeans.f90 -o ./mod/mod_breathing_kmeans.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../clustering/mod_dbscan.f90 -o ./mod/mod_dbscan.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_node.f90 -o ./mod/mod_node.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_splitter.f90 -o ./mod/mod_splitter.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_woodworking_tools.f90 -o ./mod/mod_woodworking_tools.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_base_tree.f90 -o ./mod/mod_base_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_decision_tree.f90 -o ./mod/mod_decision_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_extra_tree.f90 -o ./mod/mod_extra_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_clouds.f90 -o ./mod/mod_clouds.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_lawu.f90 -o ./mod/mod_lawu.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_sadt.f90 -o ./mod/mod_sadt.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_isolation_tree.f90 -o ./mod/mod_isolation_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_extended_isolation_tree.f90 -o ./mod/mod_extended_isolation_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_random_rotation_decision_tree.f90 -o ./mod/mod_random_rotation_decision_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_sliq.f90 -o ./mod/mod_sliq.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_oblivious_tree.f90 -o ./mod/mod_oblivious_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../decision_tree/mod_threshold_tree.f90 -o ./mod/mod_threshold_tree.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_exact_duplication_search.f90 -o ./mod/mod_exact_duplication_search.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_lsh.f90 -o ./mod/mod_lsh.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neighbour/mod_product_quantization.f90 -o ./mod/mod_product_quantization.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../boosting_trees/mod_gradient_boosting_clouds.f90 -o ./mod/mod_gradient_boosting_clouds.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../boosting_trees/mod_gradient_boosting_extra_tree.f90 -o ./mod/mod_gradient_boosting_extra_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../boosting_trees/mod_gradient_boosting_lawu.f90 -o ./mod/mod_gradient_boosting_lawu.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../boosting_trees/mod_gradient_boosting_tree.f90 -o ./mod/mod_gradient_boosting_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../boosting_trees/mod_gradient_boosting_adaboost.f90 -o ./mod/mod_gradient_boosting_adaboost.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../forest/mod_forest_jitter.f90 -o ./mod/mod_forest_jitter.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../forest/mod_forest_packing_.f90 -o ./mod/mod_forest_packing_.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../forest/mod_random_forest.f90 -o ./mod/mod_random_forest.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../forest/mod_extra_trees.f90 -o ./mod/mod_extra_trees.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../forest/mod_isolation_forest.f90 -o ./mod/mod_isolation_forest.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linear_regression/mod_linear_regression.f90 -o ./mod/mod_linear_regression.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../logistic_regression/mod_logistic_regression.f90 -o ./mod/mod_logistic_regression.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../sgd_estimator/mod_sgd_estimator.f90 -o ./mod/mod_sgd_estimator.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/mod_variable_in_variable.f90 -o ./mod/mod_variable_in_variable.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/mod_wengert_list.f90 -o ./mod/mod_wengert_list.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/mod_activation_function.f90 -o ./mod/mod_activation_function.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_operators/mod_power.f90 -o ./mod/mod_power.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_operators/mod_addition.f90 -o ./mod/mod_addition.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_operators/mod_multiplication.f90 -o ./mod/mod_multiplication.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_operators/mod_substraction.f90 -o ./mod/mod_substraction.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_operators/mod_division.f90 -o ./mod/mod_division.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_summation.f90 -o ./mod/mod_summation.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_spread.f90 -o ./mod/mod_spread.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_absolute_value.f90 -o ./mod/mod_absolute_value.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_square_root.f90 -o ./mod/mod_square_root.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_sinusoid.f90 -o ./mod/mod_sinusoid.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_cosine.f90 -o ./mod/mod_cosine.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_tangent.f90 -o ./mod/mod_tangent.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_arcsinusoid.f90 -o ./mod/mod_arcsinusoid.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_arccosine.f90 -o ./mod/mod_arccosine.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_arctangent.f90 -o ./mod/mod_arctangent.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_exponential.f90 -o ./mod/mod_exponential.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_log_natural.f90 -o ./mod/mod_log_natural.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/intrinsic_functions/mod_matmul.f90 -o ./mod/mod_matmul.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/activation_function/mod_sigmoid.f90 -o ./mod/mod_sigmoid.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/activation_function/mod_square.f90 -o ./mod/mod_square.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/activation_function/mod_relu.f90 -o ./mod/mod_relu.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/activation_function/mod_softmax.f90 -o ./mod/mod_softmax.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/layer/mod_dense.f90 -o ./mod/mod_dense.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/mod_loss_function.f90 -o ./mod/mod_loss_function.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/mod_backwards.f90 -o ./mod/mod_backwards.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/mod_my_mlp.f90 -o ./mod/mod_my_mlp.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../multi_layer_perceptron/mod_mnist_classifier.f90 -o ./mod/mod_mnist_classifier.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../svm/mod_kernel.f90 -o ./mod/mod_kernel.out	
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../svm/mod_svm_cache.f90 -o ./mod/mod_svm_cache.out	
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../svm/mod_linear_svm.f90 -o ./mod/mod_linear_svm.out	
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../svm/mod_kernel_svm.f90 -o ./mod/mod_kernel_svm.out	
	\
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up/inc_sum_up_vector.c -o ./mod/inc_sum_up_vector_c.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up/inc_sum_up_matrix.c -o ./mod/inc_sum_up_matrix_c.out ./mod/inc_sum_up_vector_c.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up_gt/inc_sum_up_gt.c -o ./mod/inc_sum_up_gt_c.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/get_minmax/inc_get_minmax.c -o ./mod/inc_get_minmax.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/get_minmax/inc_get_matrix_minmax.c -o ./mod/inc_get_matrix_minmax.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/variance_value_of_vector/inc_variance_value_of_vector.c -o ./mod/inc_variance_value_of_vector.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up_gt/inc_get_matrix_count_and_sum_up_gt.c -o ./mod/inc_get_matrix_count_and_sum_up_gt.out
	\
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.c -o ./mod/inc_covariance_value_of_vectors.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/get_minmax_vector2vector/inc_get_minmax_vector2vector.c -o ./mod/inc_get_minmax_vector2vector.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/get_minmax/inc_get_matrix_minmax_parallel.c -o ./mod/inc_get_matrix_minmax_parallel.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up_gt/inc_get_count_and_sum_up_gt_vector2vector.c -o ./mod/inc_get_count_and_sum_up_gt_vector2vector.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../math/include/stats/sum_up_gt/inc_get_matrix_count_and_sum_up_gt_parallel.c -o ./mod/inc_get_matrix_count_and_sum_up_gt_parallel.out
	\
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../linalg/include/linalg_ax_plus_y/inc_ax_plus_y.c -o ./mod/inc_ax_plus_y.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../linalg/include/linalg_multi_mat_vec/inc_multi_mat_vec.c -o ./mod/inc_multi_mat_vec.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../linalg/include/linalg_matrix_sqsum_row/inc_matrix_sqsum_row.c -o ./mod/inc_matrix_sqsum_row.out

build:
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_common.f90 -o ./mod/mod_common.out

main:
	# make build_all
	make build

	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main.f90 -o main.out \
	$(option_blas)
	./main.out
