program main
    use mod_timer
    use mod_common
    use mod_kmeans
    implicit none
    integer(kind=8)              :: date_value1(8), date_value2(8), time_sadt
    integer(kind=8), allocatable :: n_samples_trains(:), n_columns_trains(:), n_iters(:)
    integer(kind=8)              :: n_samples_train, n_columns_train 
    integer(kind=8)              :: n_samples_test, n_columns_test
    integer(kind=8)              :: iii, jjj, n_iter, iter
    logical(kind=4)              :: skip_header
    CHARACTER(len=1)             :: dtype_in, dtype_out
    CHARACTER(len=256)           :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256)           :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256)           :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256)           :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE    :: x_train(:,:), x_test(:,:)
    real(kind=8), ALLOCATABLE    :: x_train_t(:,:), x_test_t(:,:)
    real(kind=8), ALLOCATABLE    :: y_train(:), y_test(:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_dt(:,:), y_test_pred_dt(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_et(:,:), y_test_pred_et(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_cl(:,:), y_test_pred_cl(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_lw(:,:), y_test_pred_lw(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)

    integer(kind=8) :: n_cluster, min_n_cluster, max_n_cluster
    type(kmeans) :: km, km2

    file_name_x_train_csv = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_x_train.txt"
    file_name_x_train_bin = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_x_train.bin"
    n_samples_train = 412206
    n_columns_train = 90

    file_name_x_train_csv = "../../../uci_data/97_make_regression/make_regression_x_0001000000x00050.csv"
    file_name_x_train_bin = "../../../uci_data/97_make_regression/make_regression_x_0001000000x00050.bin"
    n_samples_train = 1000000
    n_columns_train = 50
    skip_header = f_
    dtype_in  = "r"
    dtype_out = "r"

    ! print*, '============================================================='
    ! print*, "CSV to Binary"
    ! print*, "    x_train"
    ! call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
    !     n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)

    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train"
    call read_bin_2d(file_name_x_train_bin, x_train)

    print*, "Start"
    n_iter = 10
    min_n_cluster = 2
    max_n_cluster = 10
    do n_cluster=min_n_cluster, max_n_cluster, 1
        km = kmeans(max_iter=n_cluster)
        print*, '============================================================='
        print*, '============================================================='
        print*, '============================================================='
        call date_and_time(values=date_value1)
        do iter=1, n_iter, 1
            call km%fit(x_train, iter, n_iter)
        end do
        call date_and_time(values=date_value2)
        print*, n_cluster, & 
            time_diff(date_value1, date_value2), & 
            time_diff(date_value1, date_value2)/dble(n_iter)

        print*, '============================================================='
        print*, '============================================================='
        print*, '============================================================='
        call date_and_time(values=date_value1)
        do iter=1, n_iter, 1
            call km%fit_faster(x_train, iter, n_iter)
        end do
        call date_and_time(values=date_value2)
        print*, n_cluster, & 
            time_diff(date_value1, date_value2), & 
            time_diff(date_value1, date_value2)/dble(n_iter)
    end do

    stop "done"

    y_train = km%predict(x_train)

    call km%dump("test.bin")

    ! call km2%load("test.bin")

    y_test = km2%predict(x_train)

    print*, count(y_train .eq. y_test)


end program main
