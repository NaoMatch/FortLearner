program main_get_minmax_vector2vector
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

    character(len=40), allocatable :: types(:)
    character(len=40) :: hoge

    real(c_double),     target, allocatable :: x_r8(:)
    real(c_double),     target, allocatable :: min_vals(:), max_vals(:)
    real(c_double),     target, allocatable :: min_vals_orig(:), max_vals_orig(:)
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

    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)   = "get_minmax_vector2vector_01_F_r8    :"
    types(2)   = "get_minmax_vector2vector_02_F_r8    :"
    types(3)   = "get_minmax_vector2vector_04_F_r8    :"
    types(4)   = "get_minmax_vector2vector_08_F_r8    :"
    types(5)   = "get_minmax_vector2vector_16_F_r8    :"
    types(6)   = "get_minmax_vector2vector_01_C_r8    :"
    types(7)   = "get_minmax_vector2vector_02_C_r8    :"
    types(8)   = "get_minmax_vector2vector_04_C_r8    :"
    types(9)   = "get_minmax_vector2vector_08_C_r8    :"
    types(10)  = "get_minmax_vector2vector_16_C_r8    :"
    types(11)  = "get_minmax_vector2vector_02x_A_r8   :"
    types(12)  = "get_minmax_vector2vector_04x_A_r8   :"
    types(13)  = "get_minmax_vector2vector_04y_A_r8   :"
    types(14)  = "get_minmax_vector2vector_08y_A_r8   :"
    types(15)  = "get_minmax_vector2vector_08z_A_r8   :"
    types(16)  = "get_minmax_vector2vector_16y_A_r8   :"
    types(17)  = "get_minmax_vector2vector_16z_A_r8   :"
    types(18)  = "get_minmax_vector2vector_32z_A_r8   :"
    types(19)  = "get_minmax_vector2vector_r8         :"

    ! filename_r8 = "time_minmax_vector_r8.txt"
    ! filename_i8 = "time_minmax_vector_i8.txt"

    ! open(10, file=filename_r8)
    ! open(20, file=filename_i8)

    k_s = 25
    k_e = 72
    cnt_iter = k_e - k_s
    k_remain = cnt_iter
    cnt_k = 1
    num_select = 10

    call date_and_time(values=s_value)
    do k=k_s, k_e, 1
        n_i8 = minval((/2d0**(k/8d0), huge(0d0)/))+1
        ! n_i8 = 16
        
        allocate(x_r8(n_i8), min_vals(n_i8), max_vals(n_i8))
        allocate(min_vals_orig(n_i8), max_vals_orig(n_i8))
        x_r8_ptr = c_loc(x_r8)
        call random_number(x_r8)
        ! x_r8(n_i8)   = 100
        ! x_r8(n_i8-1) = -100
        ! x_r8 = 20 * x_r8 - 30000
        x_r8 = 20 * x_r8 - 10

        n_iter=maxval((/10000000000_8/n_i8, 1_8/))
        n_iter=maxval((/5000000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/1000000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/500000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/50000000_8/n_i8, 1_8/))
        ! n_iter=1

        print*, '============================================================='
        call random_number(min_vals_orig)
        call random_number(max_vals_orig)
        do iter_types=1, n_types, 1
            min_vals = min_vals_orig
            max_vals = max_vals_orig
            if (types(iter_types) .eq. hoge)  cycle
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1); call get_minmax_vector2vector_01_F_r8(x_r8, min_vals, max_vals, n_i8)
                    case  (2); call get_minmax_vector2vector_02_F_r8(x_r8, min_vals, max_vals, n_i8)
                    case  (3); call get_minmax_vector2vector_04_F_r8(x_r8, min_vals, max_vals, n_i8)
                    case  (4); call get_minmax_vector2vector_08_F_r8(x_r8, min_vals, max_vals, n_i8)
                    case  (5); call get_minmax_vector2vector_16_F_r8(x_r8, min_vals, max_vals, n_i8)
                    case  (6); call get_minmax_vector2vector_01_C_r8(x_r8, min_vals, max_vals, n_i8)
                    case  (7); call get_minmax_vector2vector_02_C_r8(x_r8, min_vals, max_vals, n_i8)
                    case  (8); call get_minmax_vector2vector_04_C_r8(x_r8, min_vals, max_vals, n_i8)
                    case  (9); call get_minmax_vector2vector_08_C_r8(x_r8, min_vals, max_vals, n_i8)
                    case (10); call get_minmax_vector2vector_16_C_r8(x_r8, min_vals, max_vals, n_i8)
                    case (11); call get_minmax_vector2vector_02x_A_r8(x_r8, min_vals, max_vals, n_i8)
                    case (12); call get_minmax_vector2vector_04x_A_r8(x_r8, min_vals, max_vals, n_i8)
                    case (13); call get_minmax_vector2vector_04y_A_r8(x_r8, min_vals, max_vals, n_i8)
                    case (14); call get_minmax_vector2vector_08y_A_r8(x_r8, min_vals, max_vals, n_i8)
                    case (15); call get_minmax_vector2vector_08z_A_r8(x_r8, min_vals, max_vals, n_i8)
                    case (16); call get_minmax_vector2vector_16y_A_r8(x_r8, min_vals, max_vals, n_i8)
                    case (17); call get_minmax_vector2vector_16z_A_r8(x_r8, min_vals, max_vals, n_i8)
                    case (18); call get_minmax_vector2vector_32z_A_r8(x_r8, min_vals, max_vals, n_i8)
                    case (19); call get_minmax_vector2vector(x_r8, min_vals, max_vals, n_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, i15, f30.15, f30.15, f30.15)", & 
                types(iter_types), k, n_i8, & 
                real(times(iter_types)), sum(min_vals), sum(max_vals)
        end do
        deallocate(x_r8, min_vals, max_vals)
        deallocate(min_vals_orig, max_vals_orig)
        write(10,*) k, n_i8, times
    end do

contains


end program main_get_minmax_vector2vector
