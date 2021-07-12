program main_minmax
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

    character(len=30), allocatable :: types(:)
    character(len=30) :: hoge

    real(c_double),     target, allocatable :: x_r8(:)
    integer(c_int64_t), target, allocatable :: x_i8(:)
    integer(c_int64_t), target, allocatable :: i_i8(:), i_full_i8(:)
    real(c_double)                          :: res_r8, min_r8, max_r8
    integer(c_int64_t)                      :: res_i8, min_i8, max_i8

    type(c_ptr) :: x_r8_ptr, x_i8_ptr
    type(c_ptr) :: v_r8_ptr, v_i8_ptr
    type(c_ptr) :: i_i8_ptr

    real(kind=8), ALLOCATABLE    :: col_ids_r(:)
    integer(kind=8), ALLOCATABLE :: col_ids_i(:)
    integer(kind=8) :: j, k, n_i8, c_i8, n_iter, iter, n_types, iter_types, iii, col_id, num_select
    character(len=128) :: filename_r8, filename_i8

    n_types = 21
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)   = "minmax_naive    :"
    types(2)   = "minmax_loop_F   :"
    types(3)   = "minmax_02_F     :"
    types(4)   = "minmax_04_F     :"
    types(5)   = "minmax_08_F     :"
    types(6)   = "minmax_16_F     :"
    types(7)   = "minmax_loop_C   :"
    types(8)   = "minmax_02_C     :"
    types(9)   = "minmax_04_C     :"
    types(10)  = "minmax_08_C     :"
    types(11)  = "minmax_16_C     :"
    types(12)  = "minmax_loop_A   :"
    types(13)  = "minmax_02_A     :"
    types(14)  = "minmax_04_A     :"
    types(15)  = "minmax_08_A     :"
    types(16)  = "minmax_08z_A    :"
    types(17)  = "minmax_16_A     :"
    types(18)  = "minmax_16z_A    :"
    types(19)  = "minmax_32_A     :"
    types(20)  = "minmax_64_A     :"
    types(21)  = "QuickSelect     :"

    filename_r8 = "time_minmax_vector_r8.txt"
    filename_i8 = "time_minmax_vector_i8.txt"

    open(10, file=filename_r8)
    open(20, file=filename_i8)

    k_s = 4
    k_e = 48
    cnt_iter = k_e - k_s
    k_remain = cnt_iter
    cnt_k = 1
    num_select = 10

    call date_and_time(values=s_value)
    do k=30, 48, 1
        n_i8 = minval((/2d0**(k/2d0), 10000000d0/))+1
        
        allocate(x_r8(n_i8), x_i8(n_i8))
        x_r8_ptr = c_loc(x_r8)
        x_i8_ptr = c_loc(x_i8)
        call random_number(x_r8)
        ! x_r8(n_i8)   = 100
        ! x_r8(n_i8-1) = -100
        ! x_r8 = 20 * x_r8 - 30000
        x_r8 = 20 * x_r8 - 10
        x_i8 = x_r8

        n_iter=maxval((/10000000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/5000000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/1000000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/100000000_8/n_i8, 1_8/))
        n_iter=maxval((/50000000_8/n_i8, 1_8/))
        ! n_iter=1

        print*, '============================================================='
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            min_r8 = huge(min_r8)
            max_r8 = - huge(max_r8)
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1); min_r8 = minval(x_r8); max_r8 = maxval(x_r8)
                    case  (2); call get_minmax_loop_F_r8(min_r8, max_r8, x_r8, n_i8)
                    case  (3); call get_minmax_02_F_r8(min_r8, max_r8, x_r8, n_i8)
                    case  (4); call get_minmax_04_F_r8(min_r8, max_r8, x_r8, n_i8)
                    case  (5); call get_minmax_08_F_r8(min_r8, max_r8, x_r8, n_i8)
                    case  (6); call get_minmax_16_F_r8(min_r8, max_r8, x_r8, n_i8)
                    case  (7); call get_minmax_c_r8(min_r8, max_r8, x_r8, n_i8)
                    case  (8); call get_minmax_unroll_02_c_r8(min_r8, max_r8, x_r8, n_i8)
                    case  (9); call get_minmax_unroll_04_c_r8(min_r8, max_r8, x_r8, n_i8)
                    case (10); call get_minmax_unroll_08_c_r8(min_r8, max_r8, x_r8, n_i8)
                    case (11); call get_minmax_unroll_16_c_r8(min_r8, max_r8, x_r8, n_i8)
                    case (12); call get_minmax_a_r8(min_r8, max_r8, x_r8, n_i8)
                    case (13); call get_minmax_unroll_02_a_r8(min_r8, max_r8, x_r8, n_i8)
                    case (14); call get_minmax_unroll_04_a_r8(min_r8, max_r8, x_r8, n_i8)
                    case (15); call get_minmax_unroll_08_a_r8(min_r8, max_r8, x_r8, n_i8)
                    case (16); call get_minmax_unroll_08z_a_r8(min_r8, max_r8, x_r8, n_i8)
                    case (17); call get_minmax_unroll_16_a_r8(min_r8, max_r8, x_r8, n_i8)
                    case (18); call get_minmax_unroll_16z_a_r8(min_r8, max_r8, x_r8, n_i8)
                    case (19); call get_minmax_unroll_32_a_r8(min_r8, max_r8, x_r8, n_i8)
                    case (20); call get_minmax_unroll_64_a_r8(min_r8, max_r8, x_r8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, f30.15, f30.15, i15, i15)", types(iter_types), n_i8, real(times(iter_types)), min_r8, max_r8, &
                    minloc(abs(x_r8-min_r8)), minloc(abs(x_r8-max_r8))
        end do
        write(10,*) k, n_i8, times

        print*, '============================================================='
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            min_i8 = huge(min_i8)
            max_i8 = - huge(max_i8)
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1); min_i8 = minval(x_i8); max_i8 = maxval(x_i8)
                    case  (2); call get_minmax_loop_F_i8(min_i8, max_i8, x_i8, n_i8)
                    case  (3); call get_minmax_02_F_i8(min_i8, max_i8, x_i8, n_i8)
                    case  (4); call get_minmax_04_F_i8(min_i8, max_i8, x_i8, n_i8)
                    case  (5); call get_minmax_08_F_i8(min_i8, max_i8, x_i8, n_i8)
                    case  (6); call get_minmax_16_F_i8(min_i8, max_i8, x_i8, n_i8)
                    case  (7); call get_minmax_c_i8(min_i8, max_i8, x_i8, n_i8)
                    case  (8); call get_minmax_unroll_02_c_i8(min_i8, max_i8, x_i8, n_i8)
                    case  (9); call get_minmax_unroll_04_c_i8(min_i8, max_i8, x_i8, n_i8)
                    case (10); call get_minmax_unroll_08_c_i8(min_i8, max_i8, x_i8, n_i8)
                    case (11); call get_minmax_unroll_16_c_i8(min_i8, max_i8, x_i8, n_i8)
                    case (12); call get_minmax_a_i8(min_i8, max_i8, x_i8, n_i8)
                    case (13); call get_minmax_unroll_02_a_i8(min_i8, max_i8, x_i8, n_i8)
                    case (14); call get_minmax_unroll_04_a_i8(min_i8, max_i8, x_i8, n_i8)
                    case (15); call get_minmax_unroll_08_a_i8(min_i8, max_i8, x_i8, n_i8)
                    case (16); call get_minmax_unroll_08z_a_i8(min_i8, max_i8, x_i8, n_i8)
                    case (17); call get_minmax_unroll_16_a_i8(min_i8, max_i8, x_i8, n_i8)
                    case (18); call get_minmax_unroll_16z_a_i8(min_i8, max_i8, x_i8, n_i8)
                    case (19); call get_minmax_unroll_32_a_i8(min_i8, max_i8, x_i8, n_i8)
                    case (20); call get_minmax_unroll_64_a_i8(min_i8, max_i8, x_i8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, i30, i30, i15, i15)",       types(iter_types), n_i8, real(times(iter_types)), min_i8, max_i8, &
                minloc(abs(x_i8-min_i8)), minloc(abs(x_i8-max_i8))
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
end program main_minmax
