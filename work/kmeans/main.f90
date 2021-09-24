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

    type(kmeans) :: km, km2

    file_name_x_train_csv = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_x_train.txt"
    file_name_x_train_bin = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_x_train.bin"
    n_samples_train = 412206
    n_columns_train = 90

    print*, '============================================================='
    print*, "CSV to Binary"
    print*, "    x_train"
    call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
        n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)

    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train"
    call read_bin_2d(file_name_x_train_bin, x_train)
    ! x_train_t = transpose(x_train)
    ! do iii=4, n_samples_train, 1
    !     x_train(iii, :) = 0d0
    ! end do

    print*, "Start"
    call date_and_time(values=date_value1)
    km = kmeans(max_iter=2_8)
    call km%fit(x_train)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

    y_train = km%predict(x_train)

    call km%dump("test.bin")

    ! call km2%load("test.bin")

    y_test = km2%predict(x_train)

    print*, count(y_train .eq. y_test)


end program main
