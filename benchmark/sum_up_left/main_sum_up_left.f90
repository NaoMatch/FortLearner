program main_sum_up_left
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

    character(len=35), allocatable :: types(:)
    character(len=35) :: hoge

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
    character(len=128) :: filename_r8, filename_i8

    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)   = "sum_up_left_naive                :"
    types(2)   = "sum_up_left_loop_naive_F         :"
    types(3)   = "sum_up_left_loop_02_F            :"
    types(4)   = "sum_up_left_loop_04_F            :"
    types(5)   = "sum_up_left_loop_08_F            :"
    types(6)   = "sum_up_left_loop_16_F            :"
    types(7)   = "sum_up_left_loop_branchless_F    :"
    types(8)   = "sum_up_left_loop_branchless_02_F :"
    types(9)   = "sum_up_left_loop_branchless_04_F :"
    types(10)  = "sum_up_left_loop_branchless_08_F :"
    types(11)  = "sum_up_left_loop_branchless_16_F :"

    filename_r8 = "time_sum_up_left_r8.txt"
    filename_i8 = "time_sum_up_left_i8.txt"

    open(10, file=filename_r8)
    open(20, file=filename_i8)

    k_s = 40
    k_e = 48
    cnt_iter = k_e - k_s
    k_remain = cnt_iter
    cnt_k = 1
    num_select = 10
    thre_y_r8 = -0d0
    thre_y_i8 = -0_8

    call date_and_time(values=s_value)
    do k=k_s, k_e, 1
        n_i8 = minval((/2d0**(k/2d0), 10000000d0/))+1
        
        allocate(x_r8(n_i8), x_i8(n_i8))
        allocate(y_r8(n_i8), y_i8(n_i8))
        x_r8_ptr = c_loc(x_r8)
        x_i8_ptr = c_loc(x_i8)
        call random_number(x_r8)
        call random_number(y_r8)
        x_r8 = 20 * x_r8 - 10
        x_i8 = x_r8
        y_r8 = 20 * y_r8 - 10
        y_i8 = y_r8

        ! n_iter=maxval((/10000000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/5000000000_8/n_i8, 1_8/))
        n_iter=maxval((/1000000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/100000000_8/n_i8, 1_8/))
        n_iter=maxval((/50000000_8/n_i8, 1_8/))
        ! n_iter=1

        print*, '============================================================='
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            res_r8=0d0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1); res_r8 = sum(pack(x_r8, mask=(y_r8<=thre_y_r8)))
                    case  (2); res_r8 = sum_up_left_loop_if_F_r8(x_r8, y_r8, thre_y_r8, n_i8)
                    case  (3); res_r8 = sum_up_left_loop_if_02_F_r8(x_r8, y_r8, thre_y_r8, n_i8)
                    case  (4); res_r8 = sum_up_left_loop_if_04_F_r8(x_r8, y_r8, thre_y_r8, n_i8)
                    case  (5); res_r8 = sum_up_left_loop_if_08_F_r8(x_r8, y_r8, thre_y_r8, n_i8)
                    case  (6); res_r8 = sum_up_left_loop_if_16_F_r8(x_r8, y_r8, thre_y_r8, n_i8)
                    case  (7); res_r8 = sum_up_left_loop_branchless_F_r8(x_r8, y_r8, thre_y_r8, n_i8)
                    case  (8); res_r8 = sum_up_left_loop_branchless_02_F_r8(x_r8, y_r8, thre_y_r8, n_i8)
                    case  (9); res_r8 = sum_up_left_loop_branchless_04_F_r8(x_r8, y_r8, thre_y_r8, n_i8)
                    case (10); res_r8 = sum_up_left_loop_branchless_08_F_r8(x_r8, y_r8, thre_y_r8, n_i8)
                    case (11); res_r8 = sum_up_left_loop_branchless_16_F_r8(x_r8, y_r8, thre_y_r8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, f30.15, f30.15)", types(iter_types), n_i8, real(times(iter_types)), res_r8
        end do
        write(10,*) k, n_i8, times

        print*, '============================================================='
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            res_i8=0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1); res_i8 = sum(pack(x_i8, mask=(y_i8<=thre_y_i8)))
                    case  (2); res_i8 = sum_up_left_loop_if_F_i8(x_i8, y_i8, thre_y_i8, n_i8)
                    case  (3); res_i8 = sum_up_left_loop_if_02_F_i8(x_i8, y_i8, thre_y_i8, n_i8)
                    case  (4); res_i8 = sum_up_left_loop_if_04_F_i8(x_i8, y_i8, thre_y_i8, n_i8)
                    case  (5); res_i8 = sum_up_left_loop_if_08_F_i8(x_i8, y_i8, thre_y_i8, n_i8)
                    case  (6); res_i8 = sum_up_left_loop_if_16_F_i8(x_i8, y_i8, thre_y_i8, n_i8)
                    case  (7); res_i8 = sum_up_left_loop_branchless_F_i8(x_i8, y_i8, thre_y_i8, n_i8)
                    case  (8); res_i8 = sum_up_left_loop_branchless_02_F_i8(x_i8, y_i8, thre_y_i8, n_i8)
                    case  (9); res_i8 = sum_up_left_loop_branchless_04_F_i8(x_i8, y_i8, thre_y_i8, n_i8)
                    case (10); res_i8 = sum_up_left_loop_branchless_08_F_i8(x_i8, y_i8, thre_y_i8, n_i8)
                    case (11); res_i8 = sum_up_left_loop_branchless_16_F_i8(x_i8, y_i8, thre_y_i8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, i30)",       types(iter_types), n_i8, real(times(iter_types)), res_i8
        end do
        write(20,*) k, n_i8, times
        deallocate(x_r8, x_i8)
        stop
        call date_and_time(values=e_value)
        avg_time  = time_diff(e_value, s_value) / cnt_k
        tool_time = time_diff(e_value, s_value)
        print*, "Remain: ", int(avg_time * k_remain/1000d0), "[sec]    ", "Took: ", int(tool_time/1000d0), "[sec]"
        k_remain = k_remain - 1
        cnt_k = cnt_k + 1
    end do
    close(10)
    close(20)
end program main_sum_up_left
