program main
    use mod_timer
    use mod_common
    use mod_kmeans
    use mod_scaler
    use mod_sort
    use mod_stats
    implicit none

    integer(kind=8)              :: date_value1(8), date_value2(8), time_sadt
    integer(kind=8), allocatable :: times(:)
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
    integer(kind=8), ALLOCATABLE :: cluster_indices(:)
    real(kind=8), ALLOCATABLE    :: tmp_r(:)
    integer(kind=8), ALLOCATABLE :: tmp_i(:)

    integer(kind=8) :: n_cluster, min_n_cluster, max_n_cluster, c
    real(kind=8)    :: score
    type(minmax_scaler) :: mm_scaler
    type(kmeans) :: km, km2

    file_name_x_train_csv = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_x_train.txt"
    file_name_x_train_bin = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_x_train.bin"
    n_samples_train = 412206
    n_columns_train = 90

    ! file_name_x_train_csv = "../../../uci_data/97_make_regression/make_regression_x_0000010000x00400.csv"
    ! file_name_x_train_bin = "../../../uci_data/97_make_regression/make_regression_x_0000010000x00400.bin"
    ! n_samples_train = 100000
    ! n_columns_train = 400
    ! skip_header = f_
    ! dtype_in  = "r"
    ! dtype_out = "r"

    ! print*, '============================================================='
    ! print*, "CSV to Binary"
    ! print*, "    x_train"
    ! call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
    !     n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)

    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train"
    call read_bin_2d(file_name_x_train_bin, x_train)
    mm_scaler = minmax_scaler(min_val=-1d0, max_val=1d0)
    call mm_scaler%fit(x_train)
    x_train = mm_scaler%transform(x_train)

    ! print*, minval(x_train, dim=1)
    ! print*, maxval(x_train, dim=1)


    n_cluster=35
    n_iter = 100
    allocate(tmp_i(n_cluster))
    allocate(tmp_r(n_cluster))
    

    do iter=1, n_iter, 1
        print*, '============================================================='
        print*, '============================================================='
        print*, '============================================================='
        km = kmeans(n_clusters=n_cluster)
        call date_and_time(values=date_value1)
        call km%fit(x_train)
        call date_and_time(values=date_value2)
        score = km%score(x_train)
        print*, "Lloyd:           ", time_diff(date_value1, date_value2), score

        ! km = kmeans(n_clusters=n_cluster)
        ! call date_and_time(values=date_value1)
        ! call km%fit_slow(x_train)
        ! call date_and_time(values=date_value2)
        ! score = km%score(x_train)
        ! print*, "Lloyd_slow:      ", time_diff(date_value1, date_value2), score

        km = kmeans(n_clusters=n_cluster)
        call date_and_time(values=date_value1)
        call km%fit_elkan(x_train)
        call date_and_time(values=date_value2)
        score = km%score(x_train)
        print*, "Elkan's Method2: ", time_diff(date_value1, date_value2), score

        km = kmeans(n_clusters=n_cluster)
        call date_and_time(values=date_value1)
        call km%fit_dgemm(x_train)
        call date_and_time(values=date_value2)
        score = km%score(x_train)
        print*, "Lloyd with dgemm: ", time_diff(date_value1, date_value2), score

        km = kmeans(n_clusters=n_cluster)
        call date_and_time(values=date_value1)
        call km%fit_dgemv(x_train)
        call date_and_time(values=date_value2)
        score = km%score(x_train)
        print*, "Lloyd with dgemv: ", time_diff(date_value1, date_value2), score
        ! print*, '============================================================='
        !     tmp_r = km%cluster_centers(1,:)
        !     do c=1, n_cluster, 1
        !         tmp_i(c) = c
        !     end do
        !     call quick_argsort(tmp_r, tmp_i, n_cluster)
        !     km%cluster_centers(:,:) = km%cluster_centers(:,tmp_i)
        !     do c=1, n_cluster, 1
        !         print*, iter, c, real(km%cluster_centers(1:10,c))
        !     end do
    end do
    stop





















    print*, "Start"
    n_iter = 40
    min_n_cluster = 2
    max_n_cluster = 40
    open(100, file="check_runnning_time.csv")

    allocate(times(n_iter))
    do n_cluster=2, 3, 1
        km = kmeans(n_clusters=n_cluster)
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, '============================================================='
        print*, '============================================================='
        print*, '============================================================='
        print*, "OLD"
        do iter=1, n_iter, 1
            call date_and_time(values=date_value1)
            call km%fit_slow(x_train)
            call date_and_time(values=date_value2)
            times(iter) = time_diff(date_value1, date_value2)
        end do
        print*, "old", n_cluster, mean(times, n_iter), sqrt(variance(times, n_iter))
        write(100, *) "old", n_cluster, mean(times, n_iter), sqrt(variance(times, n_iter))

        tmp_r = km%cluster_centers(1,:)
        tmp_i = tmp_r
        do c=1, n_cluster, 1
            tmp_i(c) = c
        end do
        call quick_argsort(tmp_r, tmp_i, n_cluster)
        km%cluster_centers(:,:) = km%cluster_centers(:,tmp_i)
        do c=1, n_cluster, 1
            print*, c, real(km%cluster_centers(1:10,c))
        end do

        km = kmeans(n_clusters=n_cluster)
        print*, '============================================================='
        print*, '============================================================='
        print*, '============================================================='
        print*, "Single Threading"
        do iter=1, n_iter, 1
            call date_and_time(values=date_value1)
            call km%fit(x_train)
            call date_and_time(values=date_value2)
            times(iter) = time_diff(date_value1, date_value2)
        end do
        print*, "new", n_cluster, mean(times, n_iter), sqrt(variance(times, n_iter))
        write(100, *) "new", n_cluster, mean(times, n_iter), sqrt(variance(times, n_iter))

        tmp_r = km%cluster_centers(1,:)
        tmp_i = tmp_r
        do c=1, n_cluster, 1
            tmp_i(c) = c
        end do
        call quick_argsort(tmp_r, tmp_i, n_cluster)
        km%cluster_centers(:,:) = km%cluster_centers(:,tmp_i)
        do c=1, n_cluster, 1
            print*, c, real(km%cluster_centers(1:10,c))
        end do
    end do
    stop

    do n_cluster=10, 40, 5
        km = kmeans(n_clusters=n_cluster)
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, ""
        print*, '============================================================='
        print*, '============================================================='
        print*, '============================================================='
        print*, "OLD"
        do iter=1, n_iter, 1
            call date_and_time(values=date_value1)
            call km%fit_slow(x_train)
            call date_and_time(values=date_value2)
            times(iter) = time_diff(date_value1, date_value2)
        end do
        print*, "old", n_cluster, mean(times, n_iter), sqrt(variance(times, n_iter))
        write(100, *) "old", n_cluster, mean(times, n_iter), sqrt(variance(times, n_iter))

        tmp_r = km%cluster_centers(1,:)
        tmp_i = tmp_r
        do c=1, n_cluster, 1
            tmp_i(c) = c
        end do
        call quick_argsort(tmp_r, tmp_i, n_cluster)
        km%cluster_centers(:,:) = km%cluster_centers(:,tmp_i)
        do c=1, n_cluster, 1
            print*, c, real(km%cluster_centers(1:10,c))
        end do

        km = kmeans(n_clusters=n_cluster)
        print*, '============================================================='
        print*, '============================================================='
        print*, '============================================================='
        print*, "Single Threading"
        do iter=1, n_iter, 1
            call date_and_time(values=date_value1)
            call km%fit(x_train)
            call date_and_time(values=date_value2)
            times(iter) = time_diff(date_value1, date_value2)
        end do
        print*, "new", n_cluster, mean(times, n_iter), sqrt(variance(times, n_iter))
        write(100, *) "new", n_cluster, mean(times, n_iter), sqrt(variance(times, n_iter))

        tmp_r = km%cluster_centers(1,:)
        tmp_i = tmp_r
        do c=1, n_cluster, 1
            tmp_i(c) = c
        end do
        call quick_argsort(tmp_r, tmp_i, n_cluster)
        km%cluster_centers(:,:) = km%cluster_centers(:,tmp_i)
        do c=1, n_cluster, 1
            print*, c, real(km%cluster_centers(1:10,c))
        end do
    end do
    close(100)
end program main
