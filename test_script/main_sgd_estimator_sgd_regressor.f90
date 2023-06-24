program main_sgd_estimator_sgd_regressor
    use mod_common
    use mod_sgd_estimator
    use mod_metric
    use mod_data_holder
    implicit none

    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:)
    real(kind=8), ALLOCATABLE :: y_pred(:,:)

    type(metrics) :: metric

    type(sgd_regressor) :: sgd_reg
    type(data_holder), target :: dholder
    type(data_holder), pointer :: dholder_ptr

    file_name_x_train_bin = "../sample_data/make_regression_X_train_0000100000x00100.bin"
    file_name_y_train_bin = "../sample_data/make_regression_y_train_0000100000x00100.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)

    dholder = data_holder(x_train, y_train, is_trans_x=f_)
    dholder_ptr => dholder

    sgd_reg = sgd_regressor()
    
    call sgd_reg%fit(dholder)
    y_pred = sgd_reg%predict(x_train)
    print*, metric%mean_square_error(y_train(:,1), y_pred(:,1))
end program main_sgd_estimator_sgd_regressor