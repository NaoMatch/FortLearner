program main_sgd_regressor
    use mod_const
    use mod_common
    use mod_timer

    use mod_metric
    use mod_scaler
    use mod_sgd_estimator
    use mod_linear_regression
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
    real(kind=8), ALLOCATABLE :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred_sgd(:,:), y_test_pred_sgd(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred_et(:,:), y_test_pred_et(:,:)
    
    type(metrics)                 :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr
    type(standard_scaler)         :: stdscaler
    type(linear_regression)       :: lr_reg
    type(sgd_regressor)           :: sgd_reg

    print*, '============================================================='
    print*, "Input Data Shape: "
    n_samples_train  = 17010_8
    n_samples_test   = 2127_8
    n_columns_train  = 81_8
    n_columns_test   = 81_8
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
    file_name_x_train_csv = "./input/SuperConductivity_x_train.csv"
    file_name_y_train_csv = "./input/SuperConductivity_y_train.csv"
    file_name_x_train_bin = "./input/SuperConductivity_x_train.bin"
    file_name_y_train_bin = "./input/SuperConductivity_y_train.bin"
    file_name_x_test_csv  = "./input/SuperConductivity_x_test.csv"
    file_name_y_test_csv  = "./input/SuperConductivity_y_test.csv"
    file_name_x_test_bin  = "./input/SuperConductivity_x_test.bin"
    file_name_y_test_bin  = "./input/SuperConductivity_y_test.bin"
    print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)

    ! print*, '============================================================='
    ! print*, "Input Data Shape: "
    ! n_samples_train  = 7654_8
    ! n_samples_test   = 957_8
    ! n_columns_train  = 4_8
    ! n_columns_test   = 4_8
    ! skip_header = t_
    ! dtype_in = "r"
    ! dtype_out = "r"
    ! print*, "    train (row x col): ", n_samples_train, n_columns_train
    ! print*, "    test  (row x col): ", n_samples_test, n_columns_test
    ! print*, "    skip header:       ", skip_header
    ! print*, "    data type input:   ", dtype_in
    ! print*, "    data type output:  ", dtype_out

    ! print*, '============================================================='
    ! print*, "File Names: "
    ! file_name_x_train_csv = "./input/Combined_Cycle_x_train.csv"
    ! file_name_y_train_csv = "./input/Combined_Cycle_y_train.csv"
    ! file_name_x_train_bin = "./input/Combined_Cycle_x_train.bin"
    ! file_name_y_train_bin = "./input/Combined_Cycle_y_train.bin"
    ! file_name_x_test_csv  = "./input/Combined_Cycle_x_test.csv"
    ! file_name_y_test_csv  = "./input/Combined_Cycle_y_test.csv"
    ! file_name_x_test_bin  = "./input/Combined_Cycle_x_test.bin"
    ! file_name_y_test_bin  = "./input/Combined_Cycle_y_test.bin"
    ! print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    ! print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    ! print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    ! print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)


    ! print*, '============================================================='
    ! print*, "Input Data Shape: "
    ! n_samples_train  = 824_8
    ! n_samples_test   = 103_8
    ! n_columns_train  = 8_8
    ! n_columns_test   = 8_8
    ! skip_header = t_
    ! dtype_in = "r"
    ! dtype_out = "r"
    ! print*, "    train (row x col): ", n_samples_train, n_columns_train
    ! print*, "    test  (row x col): ", n_samples_test, n_columns_test
    ! print*, "    skip header:       ", skip_header
    ! print*, "    data type input:   ", dtype_in
    ! print*, "    data type output:  ", dtype_out

    ! print*, '============================================================='
    ! print*, "File Names: "
    ! file_name_x_train_csv = "./input/Concrete_x_train.csv"
    ! file_name_y_train_csv = "./input/Concrete_y_train.csv"
    ! file_name_x_train_bin = "./input/Concrete_x_train.bin"
    ! file_name_y_train_bin = "./input/Concrete_y_train.bin"
    ! file_name_x_test_csv  = "./input/Concrete_x_test.csv"
    ! file_name_y_test_csv  = "./input/Concrete_y_test.csv"
    ! file_name_x_test_bin  = "./input/Concrete_x_test.bin"
    ! file_name_y_test_bin  = "./input/Concrete_y_test.bin"
    ! print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    ! print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    ! print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    ! print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)

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

    print*, '============================================================='
    print*, "STANDARDIZARION"
    stdscaler = standard_scaler()
    call stdscaler%fit(x_train)
    x_train = stdscaler%transform(x_train)
    x_test = stdscaler%transform(x_test)

    print*, '============================================================='
    print*, "data_holder"
    dholder = data_holder(x_train, y_train)
    dholder_ptr => dholder


    print*, '============================================================='
    print*, "Fit: SGDRegressor"
    sgd_reg = sgd_regressor(&
        max_iter=10000_8, &
        learning_rate="invscaling", &
        learning_rate_initial=0.0001d0, &
        alpha=0.01d0, & 
        penalty="elasticnet", &
        l1_ratio=0.9d0, &
        verbose=t_, fit_intercept=f_)
    call sgd_reg%fit(dholder_ptr)

    print*, '============================================================='
    print*, "Predict: SGDRegressor"
    y_train_pred_sgd = sgd_reg%predict(x_train)
    y_test_pred_sgd  = sgd_reg%predict(x_test)
    print*, "Train: ", metric%mean_square_error(y_train(:,1), y_train_pred_sgd(:,1))
    print*, "Test:  ", metric%mean_square_error(y_test(:,1), y_test_pred_sgd(:,1))

    print*, '============================================================='
    print*, "Fit: LinearRegression"
    lr_reg = linear_regression()
    call lr_reg%fit(dholder_ptr)

    print*, '============================================================='
    print*, "Predict: LinearRegression"
    y_train_pred_sgd = lr_reg%predict(x_train)
    y_test_pred_sgd  = lr_reg%predict(x_test)
    print*, "Train: ", metric%mean_square_error(y_train(:,1), y_train_pred_sgd(:,1))
    print*, "Test:  ", metric%mean_square_error(y_test(:,1), y_test_pred_sgd(:,1))
end program main_sgd_regressor
