program main_linear_regression
    use mod_const, only: t_, f_
    use mod_common, only: read2bin_1d, read2bin_2d, read_bin_1d, read_bin_2d
    use mod_metric, only: root_mean_square_error, mean_square_error
    use mod_linear_regression, only: linear_regression, lasso_regression, ridge_regression
    implicit none

    integer(kind=8)    :: n_samples_train, n_columns_train
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:), x_test(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:), y_test(:)
    real(kind=8), ALLOCATABLE :: y_train_pred(:), y_test_pred(:)

    type(linear_regression) :: lr_reg
    type(lasso_regression)  :: ls_reg
    type(ridge_regression)  :: rd_reg

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
    call read2bin_1d(file_name_y_train_csv, file_name_y_train_bin, &
        n_samples_train, skip_header, dtype_in, dtype_out)
    print*, "    x_test"
    call read2bin_2d(file_name_x_test_csv, file_name_x_test_bin, &
        n_samples_test, n_columns_test, skip_header, dtype_in, dtype_out)
    print*, "    y_test"
    call read2bin_1d(file_name_y_test_csv, file_name_y_test_bin, &
        n_samples_test, skip_header, dtype_in, dtype_out)

    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train"
    call read_bin_2d(file_name_x_train_bin, x_train)
    print*, "    y_train"
    call read_bin_1d(file_name_y_train_bin, y_train)
    print*, "    x_test"
    call read_bin_2d(file_name_x_test_bin, x_test)
    print*, "    y_test"
    call read_bin_1d(file_name_y_test_bin, y_test)

    print*, '============================================================='
    print*, "LinearRegression"
    print*, "    train"
    lr_reg = linear_regression(fit_intercept=t_)
    call lr_reg%fit(x_train, y_train)
    print*, "    Coefficient: ", lr_reg%coefs_
    print*, "    Intercept: ",  lr_reg%intercept_
    print*, "    predict train"
    y_train_pred = lr_reg%predict(x_train)
    print*, "    predict test"
    y_test_pred = lr_reg%predict(x_test)
    print*, "    mean_square_error"
    print*, "        train: ", mean_square_error(y_train, y_train_pred)
    print*, "        test:  ", mean_square_error(y_test, y_test_pred)
    print*, "    root_mean_square_error"
    print*, "        train: ", root_mean_square_error(y_train, y_train_pred)
    print*, "        test:  ", root_mean_square_error(y_test, y_test_pred)


    print*, '============================================================='
    print*, "Lasso"
    print*, "    train"
    ls_reg = lasso_regression(lambda=100d0, fit_intercept=t_, max_iter=100000_8)
    call ls_reg%fit(x_train, y_train)
    print*, "    Coefficient: ", ls_reg%coefs_
    print*, "    Intercept: ",  ls_reg%intercept_
    print*, "    predict train"
    y_train_pred = ls_reg%predict(x_train)
    print*, "    predict test"
    y_test_pred = ls_reg%predict(x_test)
    print*, "    mean_square_error"
    print*, "        train: ", mean_square_error(y_train, y_train_pred)
    print*, "        test:  ", mean_square_error(y_test, y_test_pred)
    print*, "    root_mean_square_error"
    print*, "        train: ", root_mean_square_error(y_train, y_train_pred)
    print*, "        test:  ", root_mean_square_error(y_test, y_test_pred)


    print*, '============================================================='
    print*, "Ridge"
    print*, "    train"
    rd_reg = ridge_regression(lambda=100d0, fit_intercept=t_)
    call rd_reg%fit(x_train, y_train)
    print*, "    Coefficient: ", rd_reg%coefs_
    print*, "    Intercept: ",  rd_reg%intercept_
    print*, "    predict train"
    y_train_pred = rd_reg%predict(x_train)
    print*, "    predict test"
    y_test_pred = rd_reg%predict(x_test)
    print*, "    mean_square_error"
    print*, "        train: ", mean_square_error(y_train, y_train_pred)
    print*, "        test:  ", mean_square_error(y_test, y_test_pred)
    print*, "    root_mean_square_error"
    print*, "        train: ", root_mean_square_error(y_train, y_train_pred)
    print*, "        test:  ", root_mean_square_error(y_test, y_test_pred)

end program main_linear_regression
