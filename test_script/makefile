compiler_fortran := gfortran
compiler_clang   := gcc-8
mod              := -J mod/
opt              := -O2
option           := -cpp -D_x86_64 -fopenmp -march=native
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
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_const.f90 -o ./mod/mod_const.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_common.f90 -o ./mod/mod_common.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_common_type.f90 -o ./mod/mod_common_type.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_heap.f90 -o ./mod/mod_heap.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_timer.f90 -o ./mod/mod_timer.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../error/mod_error.f90 -o ./mod/mod_error.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_random.f90 -o ./mod/mod_random.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_hash.f90 -o ./mod/mod_hash.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../common/mod_sort.f90 -o ./mod/mod_sort.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_simple_tree.f90 -o ./mod/mod_simple_tree.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../math/mod_math.f90 -o ./mod/mod_math.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../math/mod_stats.f90 -o ./mod/mod_stats.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../metric/mod_metric.f90 -o ./mod/mod_metric.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../linalg/mod_linalg.f90 -o ./mod/mod_linalg.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../linalg/mod_eig.f90 -o ./mod/mod_eig.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../optimization/mod_optimization.f90 -o ./mod/mod_optimization.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../data_structure/mod_union_find.f90 -o ./mod/mod_union_find.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../hyperparameter/mod_hyperparameter.f90 -o ./mod/mod_hyperparameter.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../hash_table/mod_hash_table.f90 -o ./mod/mod_hash_table.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../hash_table/mod_hash_map.f90 -o ./mod/mod_hash_map.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../preprocessing/mod_discretizer.f90 -o ./mod/mod_discretizer.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../preprocessing/mod_scaler.f90 -o ./mod/mod_scaler.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../preprocessing/mod_encoder.f90 -o ./mod/mod_encoder.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../data_holder/mod_data_holder.f90 -o ./mod/mod_data_holder.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../neighbour/mod_nearest_neighbour.f90 -o ./mod/mod_nearest_neighbour.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../neighbour/mod_brute_force_search.f90 -o ./mod/mod_brute_force_search.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../neighbour/mod_kdtree.f90 -o ./mod/mod_kdtree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../neighbour/mod_balltree.f90 -o ./mod/mod_balltree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../neighbour/mod_k_nearest_neighbor_classifier.f90 -o ./mod/mod_k_nearest_neighbor_classifier.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../neighbour/mod_k_nearest_neighbor_regressor.f90 -o ./mod/mod_k_nearest_neighbor_regressor.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../neighbour/mod_local_outlier_factor.f90 -o ./mod/mod_local_outlier_factor.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../clustering/mod_base_kmeans.f90 -o ./mod/mod_base_kmeans.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../clustering/mod_kmeans.f90 -o ./mod/mod_kmeans.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../clustering/mod_minibatch_kmeans.f90 -o ./mod/mod_minibatch_kmeans.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../clustering/mod_breathing_kmeans.f90 -o ./mod/mod_breathing_kmeans.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../clustering/mod_dbscan.f90 -o ./mod/mod_dbscan.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decomposition/mod_pca.f90 -o ./mod/mod_pca.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decomposition/mod_nipals.f90 -o ./mod/mod_nipals.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_node.f90 -o ./mod/mod_node.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_splitter.f90 -o ./mod/mod_splitter.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_woodworking_tools.f90 -o ./mod/mod_woodworking_tools.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_base_tree.f90 -o ./mod/mod_base_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_decision_tree.f90 -o ./mod/mod_decision_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_extra_tree.f90 -o ./mod/mod_extra_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_clouds.f90 -o ./mod/mod_clouds.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_lawu.f90 -o ./mod/mod_lawu.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_sadt.f90 -o ./mod/mod_sadt.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_isolation_tree.f90 -o ./mod/mod_isolation_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_extended_isolation_tree.f90 -o ./mod/mod_extended_isolation_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_random_rotation_decision_tree.f90 -o ./mod/mod_random_rotation_decision_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_sliq.f90 -o ./mod/mod_sliq.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_oblivious_tree.f90 -o ./mod/mod_oblivious_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../decision_tree/mod_threshold_tree.f90 -o ./mod/mod_threshold_tree.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../neighbour/mod_exact_duplication_search.f90 -o ./mod/mod_exact_duplication_search.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../neighbour/mod_lsh.f90 -o ./mod/mod_lsh.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../neighbour/mod_product_quantization.f90 -o ./mod/mod_product_quantization.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../boosting_trees/mod_gradient_boosting_clouds.f90 -o ./mod/mod_gradient_boosting_clouds.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../boosting_trees/mod_gradient_boosting_extra_tree.f90 -o ./mod/mod_gradient_boosting_extra_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../boosting_trees/mod_gradient_boosting_lawu.f90 -o ./mod/mod_gradient_boosting_lawu.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../boosting_trees/mod_gradient_boosting_tree.f90 -o ./mod/mod_gradient_boosting_tree.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../boosting_trees/mod_gradient_boosting_adaboost.f90 -o ./mod/mod_gradient_boosting_adaboost.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../forest/mod_forest_jitter.f90 -o ./mod/mod_forest_jitter.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../forest/mod_forest_packing_.f90 -o ./mod/mod_forest_packing_.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../forest/mod_random_forest.f90 -o ./mod/mod_random_forest.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../forest/mod_extra_trees.f90 -o ./mod/mod_extra_trees.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../forest/mod_isolation_forest.f90 -o ./mod/mod_isolation_forest.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../linear_regression/mod_linear_regression.f90 -o ./mod/mod_linear_regression.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../logistic_regression/mod_logistic_regression.f90 -o ./mod/mod_logistic_regression.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../sgd_estimator/mod_sgd_estimator.f90 -o ./mod/mod_sgd_estimator.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/mod_variable_in_variable.f90 -o ./mod/mod_variable_in_variable.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/mod_wengert_list.f90 -o ./mod/mod_wengert_list.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/mod_activation_function.f90 -o ./mod/mod_activation_function.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_operators/mod_power.f90 -o ./mod/mod_power.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_operators/mod_addition.f90 -o ./mod/mod_addition.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_operators/mod_multiplication.f90 -o ./mod/mod_multiplication.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_operators/mod_substraction.f90 -o ./mod/mod_substraction.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_operators/mod_division.f90 -o ./mod/mod_division.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_summation.f90 -o ./mod/mod_summation.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_spread.f90 -o ./mod/mod_spread.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_absolute_value.f90 -o ./mod/mod_absolute_value.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_square_root.f90 -o ./mod/mod_square_root.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_sinusoid.f90 -o ./mod/mod_sinusoid.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_cosine.f90 -o ./mod/mod_cosine.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_tangent.f90 -o ./mod/mod_tangent.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_arcsinusoid.f90 -o ./mod/mod_arcsinusoid.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_arccosine.f90 -o ./mod/mod_arccosine.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_arctangent.f90 -o ./mod/mod_arctangent.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_exponential.f90 -o ./mod/mod_exponential.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_log_natural.f90 -o ./mod/mod_log_natural.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/intrinsic_functions/mod_matmul.f90 -o ./mod/mod_matmul.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/activation_function/mod_sigmoid.f90 -o ./mod/mod_sigmoid.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/activation_function/mod_square.f90 -o ./mod/mod_square.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/activation_function/mod_relu.f90 -o ./mod/mod_relu.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/activation_function/mod_softmax.f90 -o ./mod/mod_softmax.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/layer/mod_dense.f90 -o ./mod/mod_dense.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/mod_loss_function.f90 -o ./mod/mod_loss_function.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/mod_backwards.f90 -o ./mod/mod_backwards.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/mod_my_mlp.f90 -o ./mod/mod_my_mlp.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../multi_layer_perceptron/mod_mnist_classifier.f90 -o ./mod/mod_mnist_classifier.out
	\
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../svm/mod_kernel.f90 -o ./mod/mod_kernel.out	
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../svm/mod_svm_cache.f90 -o ./mod/mod_svm_cache.out	
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../svm/mod_linear_svm.f90 -o ./mod/mod_linear_svm.out	
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../svm/mod_kernel_svm.f90 -o ./mod/mod_kernel_svm.out	
	\
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up/inc_sum_up_vector.c -o ./mod/inc_sum_up_vector_c.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up/inc_sum_up_matrix.c -o ./mod/inc_sum_up_matrix_c.out ./mod/inc_sum_up_vector_c.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up_gt/inc_sum_up_gt.c -o ./mod/inc_sum_up_gt_c.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/get_minmax/inc_get_minmax.c -o ./mod/inc_get_minmax.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/get_minmax/inc_get_matrix_minmax.c -o ./mod/inc_get_matrix_minmax.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/variance_value_of_vector/inc_variance_value_of_vector.c -o ./mod/inc_variance_value_of_vector.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up_gt/inc_get_matrix_count_and_sum_up_gt.c -o ./mod/inc_get_matrix_count_and_sum_up_gt.out
	\
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.c -o ./mod/inc_covariance_value_of_vectors.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/get_minmax_vector2vector/inc_get_minmax_vector2vector.c -o ./mod/inc_get_minmax_vector2vector.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/get_minmax/inc_get_matrix_minmax_parallel.c -o ./mod/inc_get_matrix_minmax_parallel.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up_gt/inc_get_count_and_sum_up_gt_vector2vector.c -o ./mod/inc_get_count_and_sum_up_gt_vector2vector.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../math/include/stats/sum_up_gt/inc_get_matrix_count_and_sum_up_gt_parallel.c -o ./mod/inc_get_matrix_count_and_sum_up_gt_parallel.out
	\
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../linalg/include/linalg_ax_plus_y/inc_ax_plus_y.c -o ./mod/inc_ax_plus_y.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../linalg/include/linalg_multi_mat_vec/inc_multi_mat_vec.c -o ./mod/inc_multi_mat_vec.out
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../linalg/include/linalg_matrix_sqsum_row/inc_matrix_sqsum_row.c -o ./mod/inc_matrix_sqsum_row.out
	\
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ../common/include/common/prefix_sum/inc_prefix_sum.c -o ./mod/inc_prefix_sum.out 

clean:
	-rm ./*.mod
	-rm ./*.out

test_all:
	# make clean
	# make build_all
	# make main_csv2bin
	python create_datasets.py
	make main_common_hash_one_at_a_time_hash
	make main_common_prefix_sum
	make main_boosting_tree_gradient_boosting_clouds_regressor
	make main_boosting_tree_gradient_boosting_extra_tree_regressor
	make main_boosting_tree_gradient_boosting_lawu_regressor
	make main_boosting_tree_gradient_boosting_tree_regressor
	make main_boosting_tree_gradient_boosting_adaboost_regressor
	make main_clustering_breathing_kmeans
	make main_clustering_kmeans
	make main_clustering_minibatch_kmeans
	make main_clustering_dbscan
	make main_common_random_weighted_sampling
	make main_decision_tree_clouds
	make main_decision_tree_decision_tree
	make main_decision_tree_extra_tree
	make main_decision_tree_isolation_tree
	make main_decision_tree_lawu
	make main_decision_tree_random_rotation_decision_tree
	make main_decision_tree_sliq
	make main_decision_tree_oblivious_tree
	make main_decomposition_nipals
	make main_encoder
	make main_forest_random_forest
	make main_forest_extra_trees
	make main_forest_isolation_forest
	make main_hash_table
	make main_linalg_ax_plus_y
	make main_linear_regression_lasso_regression
	make main_linear_regression_linear_regression
	make main_linear_regression_ridge_regression
	make main_multi_layer_perceptron
	make main_neighbour_exact_duplication_search
	make main_neighbour_brute_force_search
	make main_neighbour_balltree
	make main_neighbour_kdtree
	make main_neighbour_k_nearest_neighbor_classifier
	make main_neighbour_k_nearest_neighbor_regressor
	make main_neighbour_product_quantization
	make main_neighbor_local_outlier_factor
	make main_stats_count_and_sum_up_gt
	make main_stats_count_and_sum_up_gt_matrix_with_index
	make main_stats_covariance_of_vectors
	make main_stats_get_minmax
	make main_stats_get_matrix_minmax_with_index
	make main_stats_get_matrix_minmax_with_index_parallel
	make main_stats_mean_matrix
	make main_stats_mean_vector
	make main_stats_median
	make main_stats_weighted_median
	make main_stats_sum_up_matrix
	make main_stats_sum_up_vector
	make main_stats_variance_vector
	make main_svm_linear_svm
	make main_svm_kernel_svm
	make main_hash_hash_map
	make main_data_structure_union_find

main_common_hash_one_at_a_time_hash:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_common_hash_one_at_a_time_hash.f90 -o main_common_hash_one_at_a_time_hash.out \
	$(option_blas)
	./main_common_hash_one_at_a_time_hash.out

main_common_prefix_sum:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_common_prefix_sum.f90 -o main_common_prefix_sum.out \
	$(option_blas)
	./main_common_prefix_sum.out

main_sgd_estimator_sgd_regressor:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_sgd_estimator_sgd_regressor.f90 -o main_sgd_estimator_sgd_regressor.out \
	$(option_blas)
	./main_sgd_estimator_sgd_regressor.out

main_logistic_regression_logistic_regression:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_logistic_regression_logistic_regression.f90 -o main_logistic_regression_logistic_regression.out \
	$(option_blas)
	./main_logistic_regression_logistic_regression.out

main_data_structure_union_find:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_data_structure_union_find.f90 -o main_data_structure_union_find.out \
	$(option_blas)
	./main_data_structure_union_find.out

main_hash_hash_map:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_hash_hash_map.f90 -o main_hash_hash_map.out \
	$(option_blas)
	./main_hash_hash_map.out

main_svm_kernel_svm_rbf:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_svm_kernel_svm_rbf.f90 -o main_svm_kernel_svm_rbf.out \
	$(option_blas)
	./main_svm_kernel_svm_rbf.out

main_svm_kernel_svm:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_svm_kernel_svm.f90 -o main_svm_kernel_svm.out \
	$(option_blas)
	./main_svm_kernel_svm.out

main_svm_linear_svm:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_svm_linear_svm.f90 -o main_svm_linear_svm.out \
	$(option_blas)
	./main_svm_linear_svm.out

main_csv2bin:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_csv2bin.f90 -o main_csv2bin.out \
	$(option_blas)

main_boosting_tree_gradient_boosting_clouds_regressor:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_boosting_tree_gradient_boosting_clouds_regressor.f90 -o main_boosting_tree_gradient_boosting_clouds_regressor.out \
	$(option_blas)
	./main_boosting_tree_gradient_boosting_clouds_regressor.out

main_boosting_tree_gradient_boosting_extra_tree_regressor:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_boosting_tree_gradient_boosting_extra_tree_regressor.f90 -o main_boosting_tree_gradient_boosting_extra_tree_regressor.out \
	$(option_blas)
	./main_boosting_tree_gradient_boosting_extra_tree_regressor.out

main_boosting_tree_gradient_boosting_lawu_regressor:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_boosting_tree_gradient_boosting_lawu_regressor.f90 -o main_boosting_tree_gradient_boosting_lawu_regressor.out \
	$(option_blas)
	./main_boosting_tree_gradient_boosting_lawu_regressor.out

main_boosting_tree_gradient_boosting_tree_regressor:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_boosting_tree_gradient_boosting_tree_regressor.f90 -o main_boosting_tree_gradient_boosting_tree_regressor.out \
	$(option_blas)
	./main_boosting_tree_gradient_boosting_tree_regressor.out

main_boosting_tree_gradient_boosting_adaboost_regressor:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_boosting_tree_gradient_boosting_adaboost_regressor.f90 -o main_boosting_tree_gradient_boosting_adaboost_regressor.out \
	$(option_blas)
	./main_boosting_tree_gradient_boosting_adaboost_regressor.out

main_clustering_breathing_kmeans:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_clustering_breathing_kmeans.f90 -o main_clustering_breathing_kmeans.out \
	$(option_blas)
	./main_clustering_breathing_kmeans.out

main_clustering_kmeans:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_clustering_kmeans.f90 -o main_clustering_kmeans.out \
	$(option_blas)
	./main_clustering_kmeans.out

main_clustering_minibatch_kmeans:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_clustering_minibatch_kmeans.f90 -o main_clustering_minibatch_kmeans.out \
	$(option_blas)
	./main_clustering_minibatch_kmeans.out

main_clustering_dbscan:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_clustering_dbscan.f90 -o main_clustering_dbscan.out \
	$(option_blas)
	./main_clustering_dbscan.out

main_common_random_weighted_sampling:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_common_random_weighted_sampling.f90 -o main_common_random_weighted_sampling.out \
	$(option_blas)
	./main_common_random_weighted_sampling.out

main_decision_tree_clouds:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_clouds.f90 -o main_decision_tree_clouds.out \
	$(option_blas)
	# ./main_decision_tree_clouds.out

main_decision_tree_decision_tree:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_decision_tree.f90 -o main_decision_tree_decision_tree.out \
	$(option_blas)
	./main_decision_tree_decision_tree.out

main_decision_tree_extra_tree:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_extra_tree.f90 -o main_decision_tree_extra_tree.out \
	$(option_blas)
	./main_decision_tree_extra_tree.out

main_decision_tree_lawu:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_lawu.f90 -o main_decision_tree_lawu.out \
	$(option_blas)
	./main_decision_tree_lawu.out

main_decision_tree_isolation_tree:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_isolation_tree.f90 -o main_decision_tree_isolation_tree.out \
	$(option_blas)
	./main_decision_tree_isolation_tree.out

main_decision_tree_sadt:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_sadt.f90 -o main_decision_tree_sadt.out \
	$(option_blas)
	./main_decision_tree_sadt.out

main_decision_tree_sliq:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_sliq.f90 -o main_decision_tree_sliq.out \
	$(option_blas)
	./main_decision_tree_sliq.out

main_decision_tree_oblivious_tree:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_oblivious_tree.f90 -o main_decision_tree_oblivious_tree.out \
	$(option_blas)
	./main_decision_tree_oblivious_tree.out

main_decomposition_nipals:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decomposition_nipals.f90 -o main_decomposition_nipals.out \
	$(option_blas)
	./main_decomposition_nipals.out

main_encoder:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_encoder.f90 -o main_encoder.out \
	$(option_blas)
	./main_encoder.out

main_forest_random_forest:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_forest_random_forest.f90 -o main_forest_random_forest.out \
	$(option_blas)
	./main_forest_random_forest.out

main_forest_extra_trees:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_forest_extra_trees.f90 -o main_forest_extra_trees.out \
	$(option_blas)
	./main_forest_extra_trees.out

main_forest_isolation_forest:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_forest_isolation_forest.f90 -o main_forest_isolation_forest.out \
	$(option_blas)
	./main_forest_isolation_forest.out

main_hash_table:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_hash_table.f90 -o main_hash_table.out \
	$(option_blas)
	./main_hash_table.out

main_decision_tree_random_rotation_decision_tree:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_decision_tree_random_rotation_decision_tree.f90 -o main_decision_tree_random_rotation_decision_tree.out \
	$(option_blas)
	./main_decision_tree_random_rotation_decision_tree.out

main_linalg_ax_plus_y:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_linalg_ax_plus_y.f90 -o main_linalg_ax_plus_y.out \
	$(option_blas)
	./main_linalg_ax_plus_y.out

main_linear_regression_lasso_regression:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_linear_regression_lasso_regression.f90 -o main_linear_regression_lasso_regression.out \
	$(option_blas)
	./main_linear_regression_lasso_regression.out

main_linear_regression_linear_regression:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_linear_regression_linear_regression.f90 -o main_linear_regression_linear_regression.out \
	$(option_blas)
	./main_linear_regression_linear_regression.out

main_linear_regression_ridge_regression:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_linear_regression_ridge_regression.f90 -o main_linear_regression_ridge_regression.out \
	$(option_blas)
	./main_linear_regression_ridge_regression.out

main_neighbour_balltree:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_neighbour_balltree.f90 -o main_neighbour_balltree.out \
	$(option_blas)
	./main_neighbour_balltree.out

main_neighbour_exact_duplication_search:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_neighbour_exact_duplication_search.f90 -o main_neighbour_exact_duplication_search.out \
	$(option_blas)
	./main_neighbour_exact_duplication_search.out

main_neighbour_brute_force_search:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_neighbour_brute_force_search.f90 -o main_neighbour_brute_force_search.out \
	$(option_blas)
	./main_neighbour_brute_force_search.out

main_neighbour_kdtree:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_neighbour_kdtree.f90 -o main_neighbour_kdtree.out \
	$(option_blas)
	./main_neighbour_kdtree.out

main_neighbour_k_nearest_neighbor_classifier:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_neighbour_k_nearest_neighbor_classifier.f90 -o main_neighbour_k_nearest_neighbor_classifier.out \
	$(option_blas)
	./main_neighbour_k_nearest_neighbor_classifier.out

main_neighbour_k_nearest_neighbor_regressor:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_neighbour_k_nearest_neighbor_regressor.f90 -o main_neighbour_k_nearest_neighbor_regressor.out \
	$(option_blas)
	./main_neighbour_k_nearest_neighbor_regressor.out

main_neighbour_lsh:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_neighbour_lsh.f90 -o main_neighbour_lsh.out \
	$(option_blas)
	./main_neighbour_lsh.out

main_neighbour_product_quantization:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_neighbour_product_quantization.f90 -o main_neighbour_product_quantization.out \
	$(option_blas)
	./main_neighbour_product_quantization.out

main_neighbor_local_outlier_factor:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_neighbor_local_outlier_factor.f90 -o main_neighbor_local_outlier_factor.out \
	$(option_blas)
	./main_neighbor_local_outlier_factor.out

main_stats_count_and_sum_up_gt:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_count_and_sum_up_gt.f90 -o main_stats_count_and_sum_up_gt.out \
	$(option_blas)
	./main_stats_count_and_sum_up_gt.out

main_stats_covariance_of_vectors:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_covariance_of_vectors.f90 -o main_stats_covariance_of_vectors.out \
	$(option_blas)
	./main_stats_covariance_of_vectors.out

main_stats_get_minmax:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_get_minmax.f90 -o main_stats_get_minmax.out \
	$(option_blas)
	./main_stats_get_minmax.out

main_stats_get_matrix_minmax_with_index:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_get_matrix_minmax_with_index.f90 -o main_stats_get_matrix_minmax_with_index.out \
	$(option_blas)
	./main_stats_get_matrix_minmax_with_index.out

main_stats_get_matrix_minmax_with_index_parallel:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_get_matrix_minmax_with_index_parallel.f90 -o main_stats_get_matrix_minmax_with_index_parallel.out \
	$(option_blas)
	./main_stats_get_matrix_minmax_with_index_parallel.out

main_stats_count_and_sum_up_gt_matrix_with_index:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_count_and_sum_up_gt_matrix_with_index.f90 -o main_stats_count_and_sum_up_gt_matrix_with_index.out \
	$(option_blas)
	./main_stats_count_and_sum_up_gt_matrix_with_index.out

main_stats_mean_matrix:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_mean_matrix.f90 -o main_stats_mean_matrix.out \
	$(option_blas)
	./main_stats_mean_matrix.out

main_stats_mean_vector:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_mean_vector.f90 -o main_stats_mean_vector.out \
	$(option_blas)
	./main_stats_mean_vector.out

main_stats_median:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_median.f90 -o main_stats_median.out \
	$(option_blas)
	./main_stats_median.out

main_stats_weighted_median:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_weighted_median.f90 -o main_stats_weighted_median.out \
	$(option_blas)
	./main_stats_weighted_median.out

main_stats_sum_up_vector:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_sum_up_vector.f90 -o main_stats_sum_up_vector.out \
	$(option_blas)
	./main_stats_sum_up_vector.out

main_stats_sum_up_matrix:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_sum_up_matrix.f90 -o main_stats_sum_up_matrix.out \
	$(option_blas)
	./main_stats_sum_up_matrix.out

main_stats_variance_vector:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_stats_variance_vector.f90 -o main_stats_variance_vector.out \
	$(option_blas)
	./main_stats_variance_vector.out

main_multi_layer_perceptron:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(asm) $(option) $(mod) $(opt) \
	main_multi_layer_perceptron.f90 -o main_multi_layer_perceptron.out \
	$(option_blas)
	./main_multi_layer_perceptron.out
