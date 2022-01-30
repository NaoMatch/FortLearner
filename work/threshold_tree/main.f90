program main
    use mod_const
    use mod_common
    use mod_threshold_tree
    use mod_kmeans
    implicit none

    type(threshold_tree) :: tt
    type(kmeans) :: km

    character(len=256) :: fn_in, fn_out
    integer(kind=8)       :: n_samples, n_columns, i, count_miss
    real(kind=8), allocatable :: x(:,:)
    integer(kind=8), allocatable :: y_pred(:,:), y_pred_km(:), y_pred_km_2(:)
    n_samples = 100
    n_columns = 2
    allocate(x(n_samples, n_columns))

    fn_in  = "../../test_script/sample_data.csv"
    fn_out = "../../test_script/sample_data.bin"
    call read2bin_2d(fn_in, fn_out, n_samples, n_columns, f_, "r", "r")
    call read_bin_2d(fn_out, x)


    tt = threshold_tree(n_clusters=2_8)
    call tt%fit(x)
    y_pred = tt%predict(x)


    km = kmeans(n_clusters=2_8, tolerance=1d-6)
    call km%fit(x)
    y_pred_km_2 = km%predict(x)
    
    y_pred_km = tt%km%predict(x)

    count_miss = 0
    do i=1, n_samples, 1
        print*, i, " :: ", y_pred(i,1), y_pred_km(i), y_pred(i,1) .ne. y_pred_km(i)
        if (y_pred(i,1) .ne. y_pred_km(i)) then
            count_miss = count_miss + 1
        end if
    end do

    print*, "count_miss: ", count_miss


end program main
