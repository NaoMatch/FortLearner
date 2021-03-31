program main_dicretizer
    use mod_discretizer
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8), time_fit, time_transform
    integer(kind=8) :: n_samples, n_columns
    real(kind=8), ALLOCATABLE    :: vector(:)
    integer(kind=8), ALLOCATABLE :: vector_disc(:)
    real(kind=8), ALLOCATABLE    :: matrix(:,:)
    integer(kind=8), ALLOCATABLE :: matrix_disc(:,:)
    integer(kind=8), ALLOCATABLE :: x_train_disc(:,:)
    integer(kind=8) :: i

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

    type(column_discretizer) :: col_disc
    type(discretizer) :: disc
    


    print*, '============================================================='
    print*, "Input Data Shape: "
    n_samples_train  = 900000_8
    n_samples_test   = 100000_8
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


    ! print*, '============================================================='
    ! print*, "CSV to Binary"
    ! print*, "    x_train"
    ! call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
    !     n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)
    ! print*, "    y_train"
    ! call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
    !     n_samples_train, 1_8, skip_header, dtype_in, "i")
    ! print*, "    x_test"
    ! call read2bin_2d(file_name_x_test_csv, file_name_x_test_bin, &
    !     n_samples_test, n_columns_test, skip_header, dtype_in, dtype_out)
    ! print*, "    y_test"
    ! call read2bin_2d(file_name_y_test_csv, file_name_y_test_bin, &
    !     n_samples_test, 1_8, skip_header, dtype_in, "i")

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

    ! print*, '============================================================='
    ! print*, "MATRIX, STRATEGY: uniform"
    ! disc = discretizer(max_bins=5_8, strategy="uniform")
    ! call date_and_time(values=date_value1)
    ! call disc%fit(x_train)
    ! call date_and_time(values=date_value2)
    ! print*, "Fit: ", time_diff(date_value1, date_value2)

    ! call date_and_time(values=date_value1)
    ! x_train_disc = disc%transform(x_train)
    ! call date_and_time(values=date_value2)
    ! print*, "Transform: ", time_diff(date_value1, date_value2)
    ! do i=1, n_samples, 1
    !     print*, x_train(i,:), " : ",  x_train_disc(i,:)
    ! end do

    ! print*, '============================================================='
    ! print*, "MATRIX, STRATEGY: quantile"
    ! disc = discretizer(max_bins=5_8, strategy="quantile")
    ! call date_and_time(values=date_value1)
    ! call disc%fit(x_train)
    ! call date_and_time(values=date_value2)
    ! print*, "Fit: ", time_diff(date_value1, date_value2)
    
    ! call date_and_time(values=date_value1)
    ! x_train_disc = disc%transform(x_train)
    ! call date_and_time(values=date_value2)
    ! print*, "Transform: ", time_diff(date_value1, date_value2)
    ! do i=1, n_samples, 1
    !     print*, x_train(i,:), " : ",  x_train_disc(i,:)
    ! end do

    print*, '============================================================='
    print*, "MATRIX, STRATEGY: kmeans"
    disc = discretizer(max_bins=255_8, strategy="kmeans_naive")
    call date_and_time(values=date_value1)
    call disc%fit(x_train(:,1:1))
    call date_and_time(values=date_value2)
    print*, "Fit: ", time_diff(date_value1, date_value2)
    stop
    
    call date_and_time(values=date_value1)
    x_train_disc = disc%transform(x_train)
    call date_and_time(values=date_value2)
    print*, "Transform: ", time_diff(date_value1, date_value2)
    ! do i=1, n_samples, 1
    !     print*, x_train(i,:), " : ",  x_train_disc(i,:)
    ! end do


























    stop 
    n_samples = 10
    n_columns = 5

    print*, '============================================================='
    print*, "MATRIX SHAPE: ", n_samples, n_columns
    print*, "VECTOR SHAPE: ", n_samples
    allocate(vector(n_samples))
    allocate(matrix(n_samples, n_columns))
    allocate(vector_disc(n_samples))
    allocate(matrix_disc(n_samples, n_columns))

    print*, '============================================================='
    print*, "Randomization"
    call RANDOM_NUMBER(vector)
    call RANDOM_NUMBER(matrix)


    print*, '============================================================='
    print*, "VECTOR, STRATEGY: uniform"
    col_disc = column_discretizer(max_bins=5_8, strategy="uniform")
    call col_disc%fit(vector)
    vector_disc = col_disc%transform(vector)
    do i=1, n_samples, 1
        print*, vector(i), vector_disc(i)
    end do

    print*, '============================================================='
    print*, "VECTOR, STRATEGY: quantile"
    col_disc = column_discretizer(max_bins=5_8, strategy="quantile")
    call col_disc%fit(vector)
    vector_disc = col_disc%transform(vector)
    do i=1, n_samples, 1
        print*, vector(i), vector_disc(i)
    end do

    print*, '============================================================='
    print*, "VECTOR, STRATEGY: kmeans"
    col_disc = column_discretizer(max_bins=5_8, strategy="kmeans")
    call col_disc%fit(vector)
    vector_disc = col_disc%transform(vector)
    do i=1, n_samples, 1
        print*, vector(i), vector_disc(i)
    end do


    print*, '============================================================='
    print*, "MATRIX, STRATEGY: uniform"
    disc = discretizer(max_bins=5_8, strategy="uniform")
    call disc%fit(matrix)
    matrix_disc = disc%transform(matrix)
    do i=1, n_samples, 1
        print*, matrix(i,:), " : ",  matrix_disc(i,:)
    end do

    print*, '============================================================='
    print*, "MATRIX, STRATEGY: quantile"
    disc = discretizer(max_bins=5_8, strategy="quantile")
    call disc%fit(matrix)
    matrix_disc = disc%transform(matrix)
    do i=1, n_samples, 1
        print*, matrix(i,:), " : ",  matrix_disc(i,:)
    end do

    print*, '============================================================='
    print*, "MATRIX, STRATEGY: kmeans"
    disc = discretizer(max_bins=5_8, strategy="kmeans")
    call disc%fit(matrix)
    matrix_disc = disc%transform(matrix)
    do i=1, n_samples, 1
        print*, matrix(i,:), " : ",  matrix_disc(i,:)
    end do

end program main_dicretizer
