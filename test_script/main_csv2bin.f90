program main_csv2bin
    use mod_common
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

    print*, '============================================================='
    print*, '============================================================='
    file_name_x_train_csv = "../sample_data/make_regression_X_0000001000x00005.csv"
    file_name_y_train_csv = "../sample_data/make_regression_y_0000001000x00005.csv"
    file_name_x_train_bin = "../sample_data/make_regression_X_0000001000x00005.bin"
    file_name_y_train_bin = "../sample_data/make_regression_y_0000001000x00005.bin"
    n_samples_train = 1000
    n_columns_train = 5
    skip_header = t_
    dtype_in  = "r"
    dtype_out = "r"

    print*, "CSV to Binary"
    print*, "    x_train"
    call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
        n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)
    print*, "    y_train"
    call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
        n_samples_train, 1_8, skip_header, dtype_in, dtype_out)

        print*, '============================================================='
    print*, '============================================================='
    file_name_x_train_csv = "../sample_data/make_brobs_X_750x2.csv"
    file_name_y_train_csv = "../sample_data/make_brobs_DBSCAN_750x2.csv"
    file_name_x_train_bin = "../sample_data/make_brobs_X_750x2.bin"
    file_name_y_train_bin = "../sample_data/make_brobs_DBSCAN_750x2.bin"
    n_samples_train = 750
    n_columns_train = 2
    skip_header = t_
    dtype_in  = "r"
    dtype_out = "r"

    print*, "CSV to Binary"
    print*, "    x_train"
    call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
        n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)
    print*, "    y_train"
    call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
        n_samples_train, 1_8, skip_header, "i", "i")
    
end program main_csv2bin