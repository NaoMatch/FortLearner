program main_stats_weighted_median
    use mod_sort
    use mod_stats
    use mod_timer
    implicit none

    integer(kind=8) :: n, min_loc, i
    real(kind=8), allocatable :: x(:), x_w(:)
    real(kind=8) :: med_val_1, med_val_2, med_val_3, med_val_4
    real(kind=8) :: w_sum

    integer(kind=8)        :: date_value1(8), date_value2(8), time1, time2, time3, time4

    n = 10
    allocate(x(n), x_w(n))

    call random_number(x)
    call random_number(x_w)
    ! x_w = 1d0

    call date_and_time(values=date_value1)
    do i=1, 1000, 1
        med_val_1 = weighted_median(x, x_w, n)
    end do
    call date_and_time(values=date_value2)
    time1 = time_diff(date_value1, date_value2)
    
    call date_and_time(values=date_value1)
    do i=1, 1000, 1
        med_val_2 = median(x, n)
    end do
    call date_and_time(values=date_value2)
    time2 = time_diff(date_value1, date_value2)
    
    call date_and_time(values=date_value1)
    med_val_3 = weighted_median_brute_force(x, x_w, n)
    call date_and_time(values=date_value2)
    time3 = time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    w_sum = sum(x_w)
    do i=1, 1, 1
        med_val_4 = weighted_median_value_of_vector_fast_r8(x, x_w, w_sum, n)
    end do
    call date_and_time(values=date_value2)
    time4 = time_diff(date_value1, date_value2)

    print*, "Median w/  weight, sort   : ", med_val_1, time1
    ! print*, "Median w/o weight, sort   : ", med_val_2, time2
    print*, "Median w/  weight, brute  : ", med_val_3, time3
    print*, "Median w/  weight, FAST?  : ", med_val_4, time4



contains



    recursive function weighted_median_value_of_vector_fast_r8(x, w, w_tot, n) result(res)
        implicit none
        real(kind=8) :: res
        real(kind=8), intent(in) :: x(n)
        real(kind=8), intent(in) :: w(n)
        real(kind=8), intent(in) :: w_tot
        integer(kind=8), intent(in) :: n

        integer(kind=8) :: i, position, factor, lt_count, idx, le_count, gt_count
        real(kind=8) :: w_half, w_sum, min_w, max_w, w_sum_gt, w_sum_lt, w_sum_le, current_med, tmp_x, tmp_w
        real(kind=8), allocatable :: x_copy(:), w_copy(:)
        real(kind=8), allocatable :: x_lt(:), w_lt(:), x_gt(:), w_gt(:)

        if (n == 1_8) then
            res = x(1)
            return
        end if

        call get_minmax(min_w, max_w, w, n)
        if (min_w .eq. max_w) then
            res = median(x, n)
            return
        end if

        position = int (dble(n*0.5d0)+.5d0 )
        allocate(x_copy(n))
        allocate(w_copy(n))
        x_copy(:) = x(:)
        w_copy(:) = w(:)

        w_half = w_tot * .5d0

        current_med = median(x_copy, n)
        ! Comute less than
        w_sum_lt = 0d0
        lt_count = 0
        do i=1, n, 1
            factor = x_copy(i) < current_med
            lt_count = lt_count + factor
            w_sum_lt = w_sum_lt + w_copy(i)*factor
        end do
     
        w_sum_le = 0d0
        le_count = 0
        do i=1, n, 1
            factor = x_copy(i) <= current_med
            le_count = le_count + factor
            w_sum_le = w_sum_le + w_copy(i)*factor
        end do

        ! print*, "    ", n, w_tot, w_sum_lt, w_sum_le, w_sum_lt > w_tot, w_sum_le < w_tot

        if (w_sum_lt > w_tot) then
            allocate(x_lt(lt_count), w_lt(lt_count))
            idx = 1
            do i=1, n, 1
                tmp_x = x_copy(i)
                if (tmp_x < current_med) then
                    x_lt(idx) = tmp_x
                    w_lt(idx) = w_copy(i)
                    idx = idx + 1
                end if
            end do
            res = weighted_median_value_of_vector_fast_r8(x_lt, w_lt, w_tot-w_sum_lt, lt_count)

        elseif (w_sum_le < w_tot) then
            gt_count = n - le_count
            allocate(x_gt(gt_count), w_gt(gt_count))
            idx = 1
            do i=1, n, 1
                tmp_x = x_copy(i)
                if (tmp_x > current_med) then
                    x_gt(idx) = tmp_x
                    w_gt(idx) = w_copy(i)
                    idx = idx + 1
                end if
            end do
            w_sum_gt = sum(w_gt)
            res = weighted_median_value_of_vector_fast_r8(x_gt, w_gt, w_tot-w_sum_gt, gt_count)

        else
            res = current_med
        end if
    end function weighted_median_value_of_vector_fast_r8


    function weighted_median_brute_force(x, w, n)
        implicit none
        real(kind=8) :: weighted_median_brute_force
        real(kind=8), intent(in) :: x(n)
        real(kind=8), intent(in) :: w(n)
        integer(kind=8), intent(in) :: n

        integer(kind=8) :: i
        real(kind=8) :: tmp_val, obj_new, obj

        obj = huge(0d0)

        do i=1, n, 1
            tmp_val = x(i)
            obj_new = sum( w(:) * abs(x(:) - tmp_val) )
            if (obj > obj_new) then
                obj = obj_new
                weighted_median_brute_force = tmp_val
            end if
        end do
    end function weighted_median_brute_force


end program main_stats_weighted_median