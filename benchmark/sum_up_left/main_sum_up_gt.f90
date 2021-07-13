program main_sum_up_gt
    use mod_stats
    use mod_timer
    use mod_sort
    use mod_random
    implicit none
    
    integer(kind=8)           :: date_value1(8), date_value2(8)
    integer(kind=8)           :: s_value(8), e_value(8)
    integer(kind=8)           :: cnt_iter, k_s, k_e, k_remain, cnt_k, tool_time
    real(kind=8), allocatable :: times(:)
    real(kind=8) :: avg_time

    character(len=45), allocatable :: types(:)
    character(len=45) :: hoge

    real(c_double),     target, allocatable :: x_r8(:), y_r8(:)
    integer(c_int64_t), target, allocatable :: x_i8(:), y_i8(:)
    integer(c_int64_t), target, allocatable :: i_i8(:), i_full_i8(:)
    real(c_double)                          :: res_r8, min_r8, max_r8, thre_y_r8
    integer(c_int64_t)                      :: res_i8, min_i8, max_i8, thre_y_i8

    type(c_ptr) :: x_r8_ptr, x_i8_ptr
    type(c_ptr) :: v_r8_ptr, v_i8_ptr
    type(c_ptr) :: i_i8_ptr

    real(kind=8), ALLOCATABLE    :: col_ids_r(:)
    integer(kind=8), ALLOCATABLE :: col_ids_i(:)
    integer(kind=8) :: j, k, n_i8, c_i8, n_iter, iter, n_types, iter_types, iii, col_id, num_select
    integer(kind=8) :: cnt_i8
    character(len=128) :: filename_r8, filename_i8
    logical(kind=4) :: is_stop

    is_stop = .false.
    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)   = "count_and_sum_up_gt_naive                :"
    types(2)   = "count_and_sum_up_gt_loop_naive_F         :"
    types(3)   = "count_and_sum_up_gt_loop_02_F            :"
    types(4)   = "count_and_sum_up_gt_loop_04_F            :"
    types(5)   = "count_and_sum_up_gt_loop_08_F            :"
    types(6)   = "count_and_sum_up_gt_loop_16_F            :"
    types(7)   = "count_and_sum_up_gt_loop_branchless_F    :"
    types(8)   = "count_and_sum_up_gt_loop_branchless_02_F :"
    types(9)   = "count_and_sum_up_gt_loop_branchless_04_F :"
    types(10)  = "count_and_sum_up_gt_loop_branchless_08_F :"
    types(11)  = "count_and_sum_up_gt_loop_branchless_16_F :"
    types(12)  = "count_and_sum_up_gt_loop_naive_C         :"
    types(13)  = "count_and_sum_up_gt_loop_02_C            :"
    types(14)  = "count_and_sum_up_gt_loop_04_C            :"
    types(15)  = "count_and_sum_up_gt_loop_08_C            :"
    types(16)  = "count_and_sum_up_gt_loop_16_C            :"
    types(17)  = "count_and_sum_up_gt_loop_branchless_C    :"
    types(18)  = "count_and_sum_up_gt_loop_branchless_02_C :"
    types(19)  = "count_and_sum_up_gt_loop_branchless_04_C :"
    types(20)  = "count_and_sum_up_gt_loop_branchless_08_C :"
    types(21)  = "count_and_sum_up_gt_loop_branchless_16_C :"
    types(22)  = "count_and_sum_up_gt_loop_02_A            :"
    types(23)  = "count_and_sum_up_gt_loop_04_A            :"
    types(24)  = "count_and_sum_up_gt_loop_08_A            :"
    types(25)  = "count_and_sum_up_gt_loop_16_A            :"
    types(26)  = "count_and_sum_up_gt_loop_32_A            :"

    filename_r8 = "time_sum_up_gt_r8.txt"
    filename_i8 = "time_sum_up_gt_i8.txt"

    open(10, file=filename_r8)
    open(20, file=filename_i8)

    k_s = 4
    k_e = 48
    cnt_iter = k_e - k_s
    k_remain = cnt_iter
    cnt_k = 1
    num_select = 10
    thre_y_r8 = +0d0
    thre_y_i8 = +0_8

    call date_and_time(values=s_value)
    do k=k_s, k_e, 1
        n_i8 = minval((/2d0**(k/2d0), 10000000d0/))+1
        
        allocate(x_r8(n_i8), x_i8(n_i8))
        allocate(y_r8(n_i8), y_i8(n_i8))
        x_r8_ptr = c_loc(x_r8)
        x_i8_ptr = c_loc(x_i8)
        call random_number(x_r8)
        call random_number(y_r8)
        x_r8 = 100 * x_r8 - 50d0
        x_i8 = x_r8
        y_r8 = 100 * y_r8 - 50d0
        y_i8 = y_r8

        ! n_iter=maxval((/10000000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/5000000000_8/n_i8, 1_8/))
        n_iter=maxval((/2000000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/500000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/50000000_8/n_i8, 1_8/))
        ! n_iter=1; is_stop=.true.

        print*, '============================================================='
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            res_r8=0d0; cnt_i8=0_8
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1);  res_r8 = sum(pack(x_r8, mask=(y_r8>thre_y_r8))); cnt_i8 = size(pack(x_r8, mask=(y_r8>thre_y_r8)))
                    case  (2); call count_and_sum_up_gt_loop_if_F_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case  (3); call count_and_sum_up_gt_loop_02_if_F_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case  (4); call count_and_sum_up_gt_loop_04_if_F_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case  (5); call count_and_sum_up_gt_loop_08_if_F_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case  (6); call count_and_sum_up_gt_loop_16_if_F_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case  (7); call count_and_sum_up_gt_loop_branchless_F_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case  (8); call count_and_sum_up_gt_loop_branchless_02_F_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case  (9); call count_and_sum_up_gt_loop_branchless_04_F_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (10); call count_and_sum_up_gt_loop_branchless_08_F_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (11); call count_and_sum_up_gt_loop_branchless_16_F_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (12); call count_and_sum_up_gt_loop_C_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (13); call count_and_sum_up_gt_loop_02_C_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (14); call count_and_sum_up_gt_loop_04_C_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (15); call count_and_sum_up_gt_loop_08_C_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (16); call count_and_sum_up_gt_loop_16_C_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (17); call count_and_sum_up_gt_loop_branchless_C_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (18); call count_and_sum_up_gt_loop_branchless_02_C_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (19); call count_and_sum_up_gt_loop_branchless_04_C_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (20); call count_and_sum_up_gt_loop_branchless_08_C_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (21); call count_and_sum_up_gt_loop_branchless_16_C_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (22); call count_and_sum_up_gt_loop_02_A_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (23); call count_and_sum_up_gt_loop_04_A_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (24); call count_and_sum_up_gt_loop_08_A_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (25); call count_and_sum_up_gt_loop_16_A_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                    case (26); call count_and_sum_up_gt_loop_32_A_r8(res_r8, cnt_i8, x_r8, y_r8, thre_y_r8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, f15.7, i15)", types(iter_types), n_i8, real(times(iter_types)), real(res_r8), int(cnt_i8)
        end do
        write(10,*) k, n_i8, times

        print*, '============================================================='
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            res_i8=0d0; cnt_i8=0_8
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1);  res_i8 = sum(pack(x_i8, mask=(y_i8>thre_y_i8))); cnt_i8 = size(pack(x_i8, mask=(y_i8>thre_y_i8)))
                    case  (2); call count_and_sum_up_gt_loop_if_F_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case  (3); call count_and_sum_up_gt_loop_02_if_F_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case  (4); call count_and_sum_up_gt_loop_04_if_F_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case  (5); call count_and_sum_up_gt_loop_08_if_F_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case  (6); call count_and_sum_up_gt_loop_16_if_F_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case  (7); call count_and_sum_up_gt_loop_branchless_F_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case  (8); call count_and_sum_up_gt_loop_branchless_02_F_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case  (9); call count_and_sum_up_gt_loop_branchless_04_F_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (10); call count_and_sum_up_gt_loop_branchless_08_F_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (11); call count_and_sum_up_gt_loop_branchless_16_F_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (12); call count_and_sum_up_gt_loop_C_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (13); call count_and_sum_up_gt_loop_02_C_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (14); call count_and_sum_up_gt_loop_04_C_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (15); call count_and_sum_up_gt_loop_08_C_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (16); call count_and_sum_up_gt_loop_16_C_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (17); call count_and_sum_up_gt_loop_branchless_C_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (18); call count_and_sum_up_gt_loop_branchless_02_C_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (19); call count_and_sum_up_gt_loop_branchless_04_C_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (20); call count_and_sum_up_gt_loop_branchless_08_C_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (21); call count_and_sum_up_gt_loop_branchless_16_C_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (22); call count_and_sum_up_gt_loop_02_A_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (23); call count_and_sum_up_gt_loop_04_A_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (24); call count_and_sum_up_gt_loop_08_A_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (25); call count_and_sum_up_gt_loop_16_A_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                    case (26); call count_and_sum_up_gt_loop_32_A_i8(res_i8, cnt_i8, x_i8, y_i8, thre_y_i8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, i15, i15)", types(iter_types), n_i8, real(times(iter_types)), & 
                int(res_i8), int(cnt_i8)
        end do
        write(20,*) k, n_i8, times
        deallocate(x_r8, x_i8)
        deallocate(y_r8, y_i8)
        if (is_stop) stop
        call date_and_time(values=e_value)
        avg_time  = time_diff(e_value, s_value) / cnt_k
        tool_time = time_diff(e_value, s_value)
        print*, "Remain: ", int(avg_time * k_remain/1000d0), "[sec]    ", "Took: ", int(tool_time/1000d0), "[sec]"
        k_remain = k_remain - 1
        cnt_k = cnt_k + 1
    end do
    close(10)
    close(20)
end program main_sum_up_gt
