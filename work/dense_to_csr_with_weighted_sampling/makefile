compiler_fortran := gfortran
compiler_clang   := gcc-8
mod              := -J mod/
opt              := -O3
option           := -cpp -D_x86_64 -fopenmp  -march=native
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
					./mod/inc_prefix_sum.out \
					./mod/mod_sparse_dgemm_c.out \

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
						./mod/mod_linalg.out \
						./mod/mod_eig.out \
						./mod/mod_sparse_dgemm.out \
						./mod/mod_csr.out \
						./mod/mod_optimization.out \
						./mod/mod_union_find.out \
						\
						./mod/mod_metric.out \
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
						./mod/mod_kernel.out \
						./mod/mod_svm_cache.out \
						./mod/mod_linear_svm.out \
						./mod/mod_kernel_svm.out \
						\
						./mod/mod_config.out \
						./mod/mod_variable.out \
						\
						./mod/mod_addition.out \
						./mod/mod_multiply.out \
						./mod/mod_power.out \
						./mod/mod_substraction.out \
						./mod/mod_division.out \
						./mod/mod_absolute_value.out \
						./mod/mod_square_root.out \
						./mod/mod_sinusoidal.out \
						./mod/mod_cosinusoidal.out \
						./mod/mod_tangent.out \
						./mod/mod_tangent_hyperbolic.out \
						./mod/mod_exponential.out \
						./mod/mod_summation.out \
						./mod/mod_reshaping.out \
						./mod/mod_transposing.out \
						./mod/mod_spreading.out \
						./mod/mod_concatination.out \
						./mod/mod_matrix_multiplication.out \
						./mod/mod_get_item.out \
						./mod/mod_log_natural.out \
						./mod/mod_intrinsics.out \
						\
						./mod/mod_visualize.out \
						\
						./mod/mod_sigmoid.out \
						./mod/mod_relu.out \
						./mod/mod_dropout.out \
						./mod/mod_softmax.out \
						./mod/mod_functions.out \
						\
						./mod/mod_linear.out \
						./mod/mod_simple_residual_block.out \
						./mod/mod_layers.out \
						\
						./mod/mod_mean_squared_error.out \
						./mod/mod_binary_cross_entropy.out \
						./mod/mod_cross_entropy.out \
						./mod/mod_sigmoid_with_loss.out \
						./mod/mod_softmax_with_loss.out \
						./mod/mod_losses.out \
						\
						./mod/mod_sgd.out \
						./mod/mod_momentum_sgd.out \
						./mod/mod_optimizers.out \


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
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_linalg.f90 -o ./mod/mod_linalg.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_eig.f90 -o ./mod/mod_eig.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_sparse_dgemm.f90 -o ./mod/mod_sparse_dgemm.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../linalg/mod_sparse_dgemm.c -o ./mod/mod_sparse_dgemm_c.out 
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_csr.f90 -o ./mod/mod_csr.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../optimization/mod_optimization.f90 -o ./mod/mod_optimization.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../data_structure/mod_union_find.f90 -o ./mod/mod_union_find.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../metric/mod_metric.f90 -o ./mod/mod_metric.out
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
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../svm/mod_kernel.f90 -o ./mod/mod_kernel.out	
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../svm/mod_svm_cache.f90 -o ./mod/mod_svm_cache.out	
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../svm/mod_linear_svm.f90 -o ./mod/mod_linear_svm.out	
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../svm/mod_kernel_svm.f90 -o ./mod/mod_kernel_svm.out	
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/mod_config.f90                -o ./mod/mod_config.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/mod_variable.f90              -o ./mod/mod_variable.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/mod_visualize.f90             -o ./mod/mod_visualize.out

	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_addition.f90              -o ./mod/mod_addition.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_multiply.f90              -o ./mod/mod_multiply.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_power.f90                 -o ./mod/mod_power.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_substraction.f90          -o ./mod/mod_substraction.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_division.f90              -o ./mod/mod_division.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_absolute_value.f90        -o ./mod/mod_absolute_value.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_square_root.f90           -o ./mod/mod_square_root.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_sinusoidal.f90            -o ./mod/mod_sinusoidal.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_cosinusoidal.f90          -o ./mod/mod_cosinusoidal.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_tangent.f90               -o ./mod/mod_tangent.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_tangent_hyperbolic.f90    -o ./mod/mod_tangent_hyperbolic.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_exponential.f90           -o ./mod/mod_exponential.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_summation.f90             -o ./mod/mod_summation.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_reshaping.f90             -o ./mod/mod_reshaping.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_transposing.f90           -o ./mod/mod_transposing.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_spreading.f90             -o ./mod/mod_spreading.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_concatination.f90         -o ./mod/mod_concatination.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_matrix_multiplication.f90 -o ./mod/mod_matrix_multiplication.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_get_item.f90              -o ./mod/mod_get_item.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_log_natural.f90           -o ./mod/mod_log_natural.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/intrinsics/mod_intrinsics.f90            -o ./mod/mod_intrinsics.out

	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/functions/mod_sigmoid.f90               -o ./mod/mod_sigmoid.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/functions/mod_relu.f90                  -o ./mod/mod_relu.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/functions/mod_dropout.f90               -o ./mod/mod_dropout.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/functions/mod_softmax.f90               -o ./mod/mod_softmax.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/functions/mod_functions.f90             -o ./mod/mod_functions.out

	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/layers/mod_linear.f90                -o ./mod/mod_linear.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/layers/mod_simple_residual_block.f90 -o ./mod/mod_simple_residual_block.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/layers/mod_layers.f90                -o ./mod/mod_layers.out

	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/losses/mod_mean_squared_error.f90    -o ./mod/mod_mean_squared_error.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/losses/mod_binary_cross_entropy.f90    -o ./mod/mod_binary_cross_entropy.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/losses/mod_cross_entropy.f90    -o ./mod/mod_cross_entropy.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/losses/mod_sigmoid_with_loss.f90    -o ./mod/mod_sigmoid_with_loss.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/losses/mod_softmax_with_loss.f90    -o ./mod/mod_softmax_with_loss.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/losses/mod_losses.f90                -o ./mod/mod_losses.out

	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/optimizers/mod_sgd.f90                   -o ./mod/mod_sgd.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/optimizers/mod_momentum_sgd.f90          -o ./mod/mod_momentum_sgd.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../neural_network/optimizers/mod_optimizers.f90            -o ./mod/mod_optimizers.out
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
	\
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../../common/include/common/prefix_sum/inc_prefix_sum.c -o ./mod/inc_prefix_sum.out 

build:
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_linalg.f90 -o ./mod/mod_linalg.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_eig.f90 -o ./mod/mod_eig.out
	# $(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_sparse_dgemm.f90 -o ./mod/mod_sparse_dgemm.out
	# $(compiler_clang)   $(mod) $(opt) $(option) -c    ../../linalg/mod_sparse_dgemm.c -o ./mod/mod_sparse_dgemm_c.out 
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../linalg/mod_csr.f90 -o ./mod/mod_csr.out


main:
	# make build_all
	make build

	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main.f90 -o main.out \
	$(option_blas)
	./main.out
