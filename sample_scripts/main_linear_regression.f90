program main_linear_regression
    use mod_timer
    use mod_const, only: t_, f_
    use mod_common, only: read2bin_1d, read2bin_2d, read_bin_1d, read_bin_2d
    use mod_metric, only: metrics
    use mod_linear_regression, only: linear_regression, lasso_regression, ridge_regression
    use mod_data_holder
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(kind=8)    :: n_samples_train, n_columns_train
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:), x_test(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred(:,:), y_test_pred(:,:)

    type(metrics)              :: metric
    type(data_holder), target  :: dholder_train, dholder_test
    type(data_holder), pointer :: dholder_train_ptr, dholder_test_ptr
    type(linear_regression)    :: lr_reg
    type(lasso_regression)     :: ls_reg
    type(ridge_regression)     :: rd_reg

    print*, '============================================================='
    print*, "Input Data Shape: "
    n_samples_train  = 824_8
    n_samples_test   = 103_8
    n_columns_train  = 8_8
    n_columns_test   = 8_8
    skip_header = t_
    dtype_in = "r"
    dtype_out = "r"
    print*, "    train (row x col): ", n_samples_train, n_columns_train
    print*, "    test  (row x col): ", n_samples_test, n_columns_test
    print*, "    skip header:       ", skip_header
    print*, "    data type input:   ", dtype_in
    print*, "    data type output:  ", dtype_out

    print*, '============================================================='
    print*, "File Names: "
    file_name_x_train_csv = "./input/Concrete_x_train.csv"
    file_name_y_train_csv = "./input/Concrete_y_train.csv"
    file_name_x_train_bin = "./input/Concrete_x_train.bin"
    file_name_y_train_bin = "./input/Concrete_y_train.bin"
    file_name_x_test_csv  = "./input/Concrete_x_test.csv"
    file_name_y_test_csv  = "./input/Concrete_y_test.csv"
    file_name_x_test_bin  = "./input/Concrete_x_test.bin"
    file_name_y_test_bin  = "./input/Concrete_y_test.bin"
    print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)

    print*, '============================================================='
    print*, "CSV to Binary"
    print*, "    x_train"
    call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
        n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)
    print*, "    y_train"
    call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
        n_samples_train, 1_8, skip_header, dtype_in, dtype_out)
    print*, "    x_test"
    call read2bin_2d(file_name_x_test_csv, file_name_x_test_bin, &
        n_samples_test, n_columns_test, skip_header, dtype_in, dtype_out)
    print*, "    y_test"
    call read2bin_2d(file_name_y_test_csv, file_name_y_test_bin, &
        n_samples_test, 1_8, skip_header, dtype_in, dtype_out)

    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train"
    call read_bin_2d(file_name_x_train_bin, x_train)
    print*, "    y_train"
    call read_bin_2d(file_name_y_train_bin, y_train)
    print*, "    x_test"
    call read_bin_2d(file_name_x_test_bin, x_test)
    print*, "    y_test"
    call read_bin_2d(file_name_y_test_bin, y_test)

    x_train = x_train(:,1:1)
    x_test  = x_test(:,1:1)
    print*, '============================================================='
    print*, "data_holder"
    dholder_train = data_holder(x_train, y_train)
    dholder_train_ptr => dholder_train
    dholder_test = data_holder(x_test, y_test)
    dholder_test_ptr => dholder_test

    print*, '============================================================='
    print*, "LinearRegression"
    print*, "    train"
    lr_reg = linear_regression(fit_intercept=t_)
    call date_and_time(values=date_value1)
    call lr_reg%fit(dholder_train_ptr)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    if (allocated(lr_reg%coefs_)) then
        print*, "    Coefficients: ", lr_reg%coefs_
    else
        print*, "    Coefficient: ", lr_reg%coef_
    end if
    print*, "    Intercept: ",  lr_reg%intercept_
    print*, "    predict train"
    y_train_pred = lr_reg%predict(x_train)
    print*, "    predict test"
    y_test_pred = lr_reg%predict(x_test)
    print*, "    mean_square_error"
    print*, "        train: ", metric%mean_square_error(y_train(:,1), y_train_pred(:,1))
    print*, "        test:  ", metric%mean_square_error(y_test(:,1), y_test_pred(:,1))
    print*, "    root_mean_square_error"
    print*, "        train: ", metric%root_mean_square_error(y_train(:,1), y_train_pred(:,1))
    print*, "        test:  ", metric%root_mean_square_error(y_test(:,1), y_test_pred(:,1))


    print*, '============================================================='
    print*, "Ridge"
    print*, "    train"
    rd_reg = ridge_regression(lambda=1000d0, fit_intercept=t_)
    call date_and_time(values=date_value1)
    call rd_reg%fit(dholder_train_ptr)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    if (allocated(rd_reg%coefs_)) then
        print*, "    Coefficients: ", rd_reg%coefs_
    else
        print*, "    Coefficient: ", rd_reg%coef_
    end if
    print*, "    Intercept: ",  rd_reg%intercept_
    print*, "    predict train"
    y_train_pred = rd_reg%predict(x_train)
    print*, "    predict test"
    y_test_pred = rd_reg%predict(x_test)
    print*, "    mean_square_error"
    print*, "        train: ", metric%mean_square_error(y_train(:,1), y_train_pred(:,1))
    print*, "        test:  ", metric%mean_square_error(y_test(:,1), y_test_pred(:,1))
    print*, "    root_mean_square_error"
    print*, "        train: ", metric%root_mean_square_error(y_train(:,1), y_train_pred(:,1))
    print*, "        test:  ", metric%root_mean_square_error(y_test(:,1), y_test_pred(:,1))


    print*, '============================================================='
    print*, "Lasso"
    print*, "    train"
    ls_reg = lasso_regression(lambda=100d0, fit_intercept=t_, max_iter=100000_8)
    call date_and_time(values=date_value1)
    call ls_reg%fit(dholder_train_ptr)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    if (allocated(ls_reg%coefs_)) then
        print*, "    Coefficients: ", ls_reg%coefs_
    else
        print*, "    Coefficient: ", ls_reg%coef_
    end if
    print*, "    Intercept: ",  ls_reg%intercept_
    print*, "    predict train"
    y_train_pred = ls_reg%predict(x_train)
    print*, "    predict test"
    y_test_pred = ls_reg%predict(x_test)
    print*, "    mean_square_error"
    print*, "        train: ", metric%mean_square_error(y_train(:,1), y_train_pred(:,1))
    print*, "        test:  ", metric%mean_square_error(y_test(:,1), y_test_pred(:,1))
    print*, "    root_mean_square_error"
    print*, "        train: ", metric%root_mean_square_error(y_train(:,1), y_train_pred(:,1))
    print*, "        test:  ", metric%root_mean_square_error(y_test(:,1), y_test_pred(:,1))



end program main_linear_regression
