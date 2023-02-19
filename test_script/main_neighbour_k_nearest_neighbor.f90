program main_neighbour_k_nearest_neighbor
    use mod_const
    use mod_timer
    use mod_common
    use mod_k_nearest_neighbor
    use mod_metric
    implicit none

    type(k_nearest_neighbor_regressor) :: knn_reg

    integer(kind=8)        :: date_value1(8), date_value2(8)
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:), x_test(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE :: pred_train(:,:), pred_test(:,:)
    type(metrics) :: metric

    file_name_x_train_bin = "../sample_data/make_regression_X_train_0000010000x00010.bin"
    file_name_y_train_bin = "../sample_data/make_regression_y_train_0000010000x00010.bin"
    call read_bin_2d(file_name_x_train_bin, x_train, print_log=f_)
    call read_bin_2d(file_name_y_train_bin, y_train, print_log=f_)

    file_name_x_test_bin = "../sample_data/make_regression_X_test_0000010000x00010.bin"
    file_name_y_test_bin = "../sample_data/make_regression_y_test_0000010000x00010.bin"
    call read_bin_2d(file_name_x_test_bin, x_test, print_log=f_)
    call read_bin_2d(file_name_y_test_bin, y_test, print_log=f_)

    knn_reg = k_nearest_neighbor_regressor(algorithm="brute_force")

    print*, shape(x_train), shape(y_train)
    call knn_reg%fit(x_train, y_train)

    pred_train = knn_reg%predict(x_train)
    pred_test = knn_reg%predict(x_test)

    print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))

end program main_neighbour_k_nearest_neighbor