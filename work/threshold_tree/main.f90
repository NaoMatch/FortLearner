program main
    use mod_const
    use mod_common
    use mod_threshold_tree
    use mod_kmeans
    use mod_timer
    implicit none
    integer(kind=8)        :: date_value1(8), date_value2(8)
    type(threshold_tree) :: tt
    type(kmeans) :: km

    character(len=256) :: fn_in, fn_out
    integer(kind=8)       :: n_samples, n_columns, i, count_miss
    real(kind=8), allocatable :: x(:,:)
    integer(kind=8), allocatable :: y_pred(:,:)
    integer(kind=8), allocatable :: y_pred_km(:)

    ! print*, '*********************************************************************************************'
    ! n_samples = 30
    ! n_columns = 2
    ! allocate(x(n_samples, n_columns))
    ! fn_in  = "../../../sample_2dim.csv"
    ! fn_out = "../../../sample_2dim.bin"
    ! call read2bin_2d(fn_in, fn_out, n_samples, n_columns, f_, "r", "r")
    ! call read_bin_2d(fn_out, x)

    ! print*, '*********************************************************************************************'
    ! n_samples = 10000
    ! n_columns = 20
    ! allocate(x(n_samples, n_columns))
    ! fn_in  = "../../../sample_2dim_10000x20.csv"
    ! fn_out = "../../../sample_2dim_10000x20.bin"
    ! call read2bin_2d(fn_in, fn_out, n_samples, n_columns, f_, "r", "r")
    ! call read_bin_2d(fn_out, x)

    ! print*, '*********************************************************************************************'
    ! n_samples = 1000
    ! n_columns = 128
    ! allocate(x(n_samples, n_columns))
    ! fn_in  = "../../../sample_2dim_1000x128x255.csv"
    ! fn_out = "../../../sample_2dim_1000x128x255.bin"
    ! call read2bin_2d(fn_in, fn_out, n_samples, n_columns, f_, "r", "r")
    ! call read_bin_2d(fn_out, x)

    print*, '*********************************************************************************************'
    n_samples = 1000
    n_columns = 128
    allocate(x(n_samples, n_columns))
    fn_in  = "../../../sample_2dim_1000x128x20.csv"
    fn_out = "../../../sample_2dim_1000x128x20.bin"
    call read2bin_2d(fn_in, fn_out, n_samples, n_columns, f_, "r", "r")
    call read_bin_2d(fn_out, x)

    tt = threshold_tree(n_clusters=20_8)
    call tt%fit(x)
        
    call date_and_time(values=date_value1)
    y_pred_km = tt%km%predict(x)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    y_pred = tt%predict(x)

    count_miss = 0_8
    do i=1, n_samples, 1
        if (y_pred(i,1) .ne. y_pred_km(i)) count_miss = count_miss + 1
    end do

    print*, count_miss
    print*, tt%km%score(x)

end program main
