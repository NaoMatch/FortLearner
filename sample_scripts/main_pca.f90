program main_pca
    use mod_const
    use mod_common
    use mod_timer

    use mod_pca
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

    real(kind=8), allocatable :: x_train_trans(:,:), x_test_trans(:,:)

    type(pca) :: pca_
    integer(kind=8)    :: n, n_columns_print

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
    print*, "Fit PCA"
    pca_ = pca()
    call pca_%fit(x_train)

    print*, '============================================================='
    print*, "Eigen Values and Vectors"
    do n=1, pca_%n_columns, 1
        print*, pca_%eig%eigen_values_r8(n), " : ", pca_%eig%eigen_vectors_r8(n,:)
    end do
    x_train_trans = pca_%transform(x_train)

    print*, '============================================================='
    print*, "Transformed Matrix"
    n_columns_print = minval((/size(x_train_trans, dim=1), 10/))
    do n=1, n_columns_print, 1
        print*, x_train_trans(n,:)
    end do

end program main_pca
