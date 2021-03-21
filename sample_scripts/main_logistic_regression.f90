program main_logistic_regression
    use mod_const
    use mod_random
    use mod_timer
    use mod_metric
    use mod_data_holder

    use mod_logistic_regression
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8), time_dt, time_et, time_cl, time_lw
    integer(kind=8)    :: iter, max_iter

    integer(kind=8)    :: n_samples_train, n_columns_train
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE, target :: x_train(:,:), x_test(:,:)
    integer(kind=8), ALLOCATABLE, target :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE, target :: y_train_pred(:,:), y_test_pred(:,:)
    type(metrics)     :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr
    type(logistic_regression) :: lr, lr_new, lr_bfgs

    print*, '============================================================='
    print*, "Input Data Shape: "
    n_samples_train  = 9000000_8
    n_samples_test   = 1000000_8
    n_columns_train  = 28_8
    n_columns_test   = 28_8
    skip_header = f_
    dtype_in = "r"
    dtype_out = "r"
    print*, "    train (row x col): ", n_samples_train, n_columns_train
    print*, "    test  (row x col): ", n_samples_test, n_columns_test
    print*, "    skip header:       ", skip_header
    print*, "    data type input:   ", dtype_in
    print*, "    data type output:  ", dtype_out

    print*, '============================================================='
    print*, "File Names: "
    file_name_x_train_csv = "../../uci_data/98_Higgs/HIGGS_train_x.csv"
    file_name_y_train_csv = "../../uci_data/98_Higgs/HIGGS_train_y.csv"
    file_name_x_train_bin = "../../uci_data/98_Higgs/HIGGS_train_x.bin"
    file_name_y_train_bin = "../../uci_data/98_Higgs/HIGGS_train_y.bin"
    file_name_x_test_csv  = "../../uci_data/98_Higgs/HIGGS_valid_x.csv"
    file_name_y_test_csv  = "../../uci_data/98_Higgs/HIGGS_valid_y.csv"
    file_name_x_test_bin  = "../../uci_data/98_Higgs/HIGGS_valid_x.bin"
    file_name_y_test_bin  = "../../uci_data/98_Higgs/HIGGS_valid_y.bin"
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
        n_samples_train, 1_8, skip_header, dtype_in, "i")
    print*, "    x_test"
    call read2bin_2d(file_name_x_test_csv, file_name_x_test_bin, &
        n_samples_test, n_columns_test, skip_header, dtype_in, dtype_out)
    print*, "    y_test"
    call read2bin_2d(file_name_y_test_csv, file_name_y_test_bin, &
        n_samples_test, 1_8, skip_header, dtype_in, "i")

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


    dholder = data_holder(x_train, y_train)
    dholder_ptr => dholder

!     lr = logistic_regression(penalty="l2", lambda=1d-10, tolerance=1d-4)
!     call date_and_time(values=date_value1)  
!     call lr%fit(dholder_ptr)
!     call date_and_time(values=date_value2)
!     y_train_pred = lr%predict(x_train)
!     y_test_pred = lr%predict(x_test)

!     print*, time_diff(date_value1, date_value2), &
!             metric%auc_i8(y_train(:,1), y_train_pred(:,1)), &
!             metric%auc_i8(y_test(:,1), y_test_pred(:,1))


    lr_new = logistic_regression(penalty="l2", lambda=1d-10, tolerance=1d-4)
    call date_and_time(values=date_value1)  
    call lr_new%fit_new(dholder_ptr)
    call date_and_time(values=date_value2)
    y_train_pred = lr_new%predict(x_train)
    y_test_pred = lr_new%predict(x_test)

    print*, time_diff(date_value1, date_value2), &
            metric%auc_i8(y_train(:,1), y_train_pred(:,1)), &
            metric%auc_i8(y_test(:,1), y_test_pred(:,1))


    lr_bfgs = logistic_regression(penalty="l2", lambda=1d-10, tolerance=1d-4)
    call date_and_time(values=date_value1)  
    call lr_bfgs%fit_bfgs(dholder_ptr)
    call date_and_time(values=date_value2)
    y_train_pred = lr_bfgs%predict(x_train)
    y_test_pred = lr_bfgs%predict(x_test)

    print*, time_diff(date_value1, date_value2), &
            metric%auc_i8(y_train(:,1), y_train_pred(:,1)), &
            metric%auc_i8(y_test(:,1), y_test_pred(:,1))

end program main_logistic_regression
