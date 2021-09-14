program main_decision_tree_clouds
    use mod_timer
    use mod_clouds, only: clouds_regressor
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

    type(clouds_regressor) :: et, et2, et3
    type(metrics)                 :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr

    file_name_x_train_csv = "../sample_data/make_regression_X_0000100000x00100.csv"
    file_name_y_train_csv = "../sample_data/make_regression_y_0000100000x00100.csv"
    file_name_x_train_bin = "../sample_data/make_regression_X_0000100000x00100.bin"
    file_name_y_train_bin = "../sample_data/make_regression_y_0000100000x00100.bin"
    n_samples_train = 100000
    n_columns_train = 100
    skip_header = t_
    dtype_in  = "r"
    dtype_out = "r"

    print*, '============================================================='
    print*, "CSV to Binary"
    print*, "    x_train"
    call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
        n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)
    print*, "    y_train"
    call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
        n_samples_train, 1_8, skip_header, dtype_in, dtype_out)

    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train"
    call read_bin_2d(file_name_x_train_bin, x_train)
    print*, "    y_train"
    call read_bin_2d(file_name_y_train_bin, y_train)

    dholder = data_holder(x_train, y_train, is_trans_x=f_)
    dholder_ptr => dholder

    ! Train, Test, Dump -----------------------------------------------------------------
    print*, "Train, Test, Dump Trained Model"
    et = clouds_regressor(max_depth=8_8)
    call et%fit(dholder_ptr)
    y_train_pred = et%predict(x_train)
    print*, metric%mean_square_error(y_train(:,1), y_train_pred(:,1))
    call et%dump(file_name="cl.bin")

    ! Load, Test ------------------------------------------------------------------------
    print*, "Load Trained Model, Test"
    et2 = clouds_regressor(max_depth=8_8)
    call et2%load(file_name="cl.bin")
    y_train_pred = et2%predict(x_train)
    print*, metric%mean_square_error(y_train(:,1), y_train_pred(:,1))

    ! Dump (Error) ----------------------------------------------------------------------
    ! et3 = clouds_regressor()
    ! call et3%dump(file_name="et_fail.bin")

    ! Load (Error) ----------------------------------------------------------------------
    ! et3 = clouds_regressor()
    ! call et3%load(file_name="et_fail.bin")

end program main_decision_tree_clouds
