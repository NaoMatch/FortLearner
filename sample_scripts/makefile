run:
	make main_cholesky_decomposition
	make main_collect_unique_values
	make main_count_unique
	make main_get_abs_maxloc
	make main_get_minmax
	make main_groupby
	make main_hyperparameter
	make main_identity
	make main_inner_product
	make main_inv_unit_lower_matrix
	make main_is_sorted
	make main_linear_regression
	make main_mattxmat
	make main_mean_variance_covariance
	make main_metric
	make main_most_left_bit_position
	make main_multi_mat_vec
	make main_vector2sum1
	make main_progress_bar
	make main_rand_integer
	make main_rand_normal
	make main_random_rotation
	make main_read2bin
	make main_roulette_selection
	make main_search
	make main_sort


main_cholesky_decomposition:
	gfortran -unroll -O3 -ftree-vectorize ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../linalg/mod_linalg.f90 main_cholesky_decomposition.f90 -o main_cholesky_decomposition.out
	./main_cholesky_decomposition.out

main_collect_unique_values:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_collect_unique_values.f90 -o main_collect_unique_values.out
	./main_collect_unique_values.out

main_count_unique:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_count_unique.f90 -o main_count_unique.out
	./main_count_unique.out

main_get_abs_maxloc:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_get_abs_maxloc.f90 -o main_get_abs_maxloc.out
	./main_get_abs_maxloc.out
	
main_get_minmax:
	gfortran -unroll -O3 -ftree-vectorize ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90  ../math/mod_stats.f90 main_get_minmax.f90 -o main_get_minmax.out
	./main_get_minmax.out

main_groupby:
	gfortran -fbounds-check -unroll -O3 -ftree-vectorize ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90  ../math/mod_stats.f90 main_groupby.f90 -o main_groupby.out
	./main_groupby.out

main_hyperparameter:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../hyperparameter/mod_hyperparameter.f90 main_hyperparameter.f90 -o main_hyperparameter.out
	./main_hyperparameter.out

main_identity:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_identity.f90 -o main_identity.out
	./main_identity.out

main_inner_product:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../linalg/mod_linalg.f90 main_inner_product.f90 -o main_inner_product.out
	./main_inner_product.out

main_inv_unit_lower_matrix:
	gfortran -unroll -O3 -ftree-vectorize ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../linalg/mod_linalg.f90 main_inv_unit_lower_matrix.f90 -o main_inv_unit_lower_matrix.out
	./main_inv_unit_lower_matrix.out

main_is_sorted:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_is_sorted.f90 -o main_is_sorted.out
	./main_is_sorted.out

main_linear_regression:
	gfortran -w \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../linalg/mod_linalg.f90 \
	../math/mod_stats.f90 \
	../metric/mod_metric.f90 \
	\
	../hyperparameter/mod_hyperparameter.f90 \
	\
	../linear_regression/mod_linear_regression.f90 \
	main_linear_regression.f90 \
	-o main_linear_regression.out
	./main_linear_regression.out

main_math:
	gfortran ../math/mod_math.f90 main_math.f90 -o main_math.out
	./main_math.out

main_mattxmat:
	gfortran -unroll -O3 -ftree-vectorize ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../linalg/mod_linalg.f90 main_mattxmat.f90 -o main_mattxmat.out
	./main_mattxmat.out

main_mean_variance_covariance:
	gfortran -fbounds-check -unroll -O3 -ftree-vectorize ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90  ../math/mod_stats.f90 main_mean_variance_covariance.f90 -o main_mean_variance_covariance.out
	./main_mean_variance_covariance.out

main_metric:
	gfortran -w \
	../common/mod_const.f90 \
	../common/mod_common.f90 \
	../common/mod_timer.f90 \
	../common/mod_random.f90 \
	../common/mod_sort.f90 \
	../math/mod_stats.f90 \
	../metric/mod_metric.f90 \
	main_metric.f90 \
	-o main_metric.out
	./main_metric.out

main_most_left_bit_position:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_most_left_bit_position.f90 -o main_most_left_bit_position.out
	./main_most_left_bit_position.out

main_multi_mat_vec:
	gfortran -unroll -O3 -ftree-vectorize ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../linalg/mod_linalg.f90 main_multi_mat_vec.f90 -o main_multi_mat_vec.out
	./main_multi_mat_vec.out

main_vector2sum1:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../linalg/mod_linalg.f90 main_vector2sum1.f90 -o main_vector2sum1.out
	./main_vector2sum1.out

main_progress_bar:
	gfortran -fbounds-check ../common/mod_const.f90 ../common/mod_common.f90 main_progress_bar.f90 -o main_progress_bar.out
	./main_progress_bar.out

main_rand_integer:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_random.f90 main_rand_integer.f90 -o main_rand_integer.out
	./main_rand_integer.out

main_rand_normal:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_random.f90 main_rand_normal.f90 -o main_rand_normal.out
	./main_rand_normal.out

main_random_rotation:
	gfortran -unroll -O3 -ftree-vectorize ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../linalg/mod_linalg.f90 main_random_rotation.f90 -o main_random_rotation.out
	./main_random_rotation.out

main_read2bin:
	gfortran -fbounds-check ../common/mod_const.f90 ../common/mod_common.f90 main_read2bin.f90 -o main_read2bin.out
	./main_read2bin.out

main_roulette_selection:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_random.f90 main_roulette_selection.f90 -o main_roulette_selection.out
	./main_roulette_selection.out

main_search:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_search.f90 -o main_search.out
	./main_search.out

main_sort:
	gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90 main_sort.f90 -o main_sort.out
	./main_sort.out
