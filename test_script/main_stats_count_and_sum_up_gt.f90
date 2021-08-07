program main_stats_count_and_sum_up_gt
    use mod_timer
    use mod_stats, only: count_and_sum_up_gt
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    integer(kind=8)           :: n_samples, vec_size(5), i
    real(kind=8)              :: sum_val_fast, sum_val_naive, thr_val
    integer(kind=8)           :: cnt_val_fast, cnt_val_naive
    real(kind=8), allocatable :: vec0(:), vec1(:)


    vec_size = (/100, 1000, 10000, 100000, 1000000/)
    thr_val = .5d0

    print*, "           n_samples                 fast                     naive       diff"
    do i=1, 5, 1
        n_samples = vec_size(i)
        allocate(vec0(n_samples))
        allocate(vec1(n_samples))
        call random_number(vec0)
        call random_number(vec1)

        call count_and_sum_up_gt(sum_val_fast, cnt_val_fast, vec0, vec1, thr_val, n_samples)
        sum_val_naive = sum(pack(vec0, mask=vec1>thr_val))
        cnt_val_naive = size(pack(vec0, mask=vec1>thr_val))

        print*, n_samples, sum_val_fast, sum_val_naive, sum_val_fast-sum_val_naive
        print*, n_samples, cnt_val_fast, cnt_val_naive, cnt_val_fast-cnt_val_naive

        deallocate(vec0, vec1)
    end do


end program main_stats_count_and_sum_up_gt
