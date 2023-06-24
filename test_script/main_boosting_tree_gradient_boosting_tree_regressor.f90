program main_boosting_tree_gradient_boosting_tree_regressor
    use mod_gradient_boosting_tree
    use mod_metric
    use mod_data_holder
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8), time_dt, time_et, time_cl, time_lw

    integer(kind=8)    :: n_samples_trains(5), n_columns_trains(5)
    integer(kind=8)    :: n_samples_train, n_columns_train 
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)

    type(metrics) :: metric
    type(gradient_boosting_tree_regressor) :: gbdt_reg, gbdt_reg2
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr

    file_name_x_train_bin = "../sample_data/make_regression_X_train_0000100000x00100.bin"
    file_name_y_train_bin = "../sample_data/make_regression_y_train_0000100000x00100.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)
    dholder = data_holder(x_train, y_train, is_trans_x=f_)
    dholder_ptr => dholder

    print*, '============================================================='
    print*, "Train: "
    gbdt_reg = gradient_boosting_tree_regressor(n_estimators=100_8, max_depth=6_8)
    call gbdt_reg%fit(dholder)
    y_train_pred = gbdt_reg%predict(x_train)
    print*, metric%mean_square_error(y_train(:,1), y_train_pred(:,1))
    call gbdt_reg%dump(file_name="cl_gbdt.bin")

    ! Load, Test ------------------------------------------------------------------------
    print*, "Load Trained Model, Test"
    call gbdt_reg2%load(file_name="cl_gbdt.bin")
    y_train_pred = gbdt_reg2%predict(x_train)
    print*, metric%mean_square_error(y_train(:,1), y_train_pred(:,1))

end program main_boosting_tree_gradient_boosting_tree_regressor