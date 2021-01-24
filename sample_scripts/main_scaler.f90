program main_scaler
    use mod_const
    use mod_common
    use mod_timer

    use mod_scaler
    use mod_stats
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
    real(kind=8), ALLOCATABLE :: x_train_trans(:,:)

    type(minmax_scaler) :: mmscaler
    type(maxabs_scaler) :: mascaler
    type(standard_scaler) :: stdscaler
    type(robust_scaler) :: rbstscaler


    print*, '============================================================='
    print*, "Input Data Shape: "
    n_samples_train  = 7654_8
    n_samples_test   = 957_8
    n_columns_train  = 4_8
    n_columns_test   = 4_8
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
    file_name_x_train_csv = "./input/Combined_Cycle_x_train.csv"
    file_name_y_train_csv = "./input/Combined_Cycle_y_train.csv"
    file_name_x_train_bin = "./input/Combined_Cycle_x_train.bin"
    file_name_y_train_bin = "./input/Combined_Cycle_y_train.bin"
    file_name_x_test_csv  = "./input/Combined_Cycle_x_test.csv"
    file_name_y_test_csv  = "./input/Combined_Cycle_y_test.csv"
    file_name_x_test_bin  = "./input/Combined_Cycle_x_test.bin"
    file_name_y_test_bin  = "./input/Combined_Cycle_y_test.bin"
    print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)


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
    print*, "Scaler"
    print*, minval(x_train, dim=1)
    print*, maxval(x_train, dim=1)

    print*, '============================================================='
    print*, "MinMaxScaler"
    mmscaler = minmax_scaler()
    call mmscaler%fit(x_train)
    x_train_trans = mmscaler%transform(x_train)
    print*, minval(x_train_trans, dim=1)
    print*, maxval(x_train_trans, dim=1)

    print*, '============================================================='
    print*, "MaxAbsScaler"
    mascaler = maxabs_scaler()
    call mascaler%fit(x_train)
    x_train_trans = mascaler%transform(x_train)
    print*, minval(x_train_trans, dim=1)
    print*, maxval(x_train_trans, dim=1)

    print*, '============================================================='
    print*, "StandardScaler"
    stdscaler = standard_scaler(with_mean=t_, with_std=t_)
    call stdscaler%fit(x_train)
    x_train_trans = stdscaler%transform(x_train)
    print*, mean(x_train_trans, n_samples_train, n_columns_train)
    print*, variance(x_train_trans, n_samples_train, n_columns_train)

    print*, '============================================================='
    print*, "RobustScaler"
    rbstscaler = robust_scaler()
    call rbstscaler%fit(x_train)
    x_train_trans = rbstscaler%transform(x_train)
    print*, minval(x_train_trans, dim=1)
    print*, maxval(x_train_trans, dim=1)

end program main_scaler
