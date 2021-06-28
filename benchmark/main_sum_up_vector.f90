program main_sum_up_vector
    use mod_stats
    use mod_timer
    implicit none
    
    integer(kind=8)           :: date_value1(8), date_value2(8)
    real(kind=8), allocatable :: times(:)

    character(len=30), allocatable :: types(:)

    real(c_double),     target, allocatable :: x_r8(:)
    integer(c_int64_t), target, allocatable :: x_i8(:)
    real(c_double)                          :: res_r8
    integer(c_int64_t)                      :: res_i8

    integer(kind=8)                         :: k, n_i8, i_i8, n_iter, iter, n_types, iter_types

    n_types = 17
    allocate(times(n_types))
    allocate(types(n_types))
    types(1)  = "sum_up_vector_intrinsic  :"
    types(2)  = "sum_up_vector_best       :"
    types(3)  = "sum_up_vector_02_F       :"
    types(4)  = "sum_up_vector_04_F       :"
    types(5)  = "sum_up_vector_08_F       :"
    types(6)  = "sum_up_vector_16_F       :"
    types(7)  = "sum_up_vector_32_F       :"
    types(8)  = "sum_loop_C               :"
    types(9)  = "sum_loop_unroll_02_C     :"
    types(10) = "sum_loop_unroll_04_C     :"
    types(11) = "sum_loop_unroll_08_C     :"
    types(12) = "sum_loop_unroll_16_C     :"
    types(13) = "sum_loop_unroll_32_C     :"
    types(14) = "sum_loop_unroll_04_ASM   :"
    types(15) = "sum_loop_unroll_08_ASM   :"
    types(16) = "sum_loop_unroll_16_ASM   :"
    types(17) = "sum_loop_unroll_32_ASM   :"


    open(10, file="time_sum_up_vector_r8.csv")
    open(20, file="time_sum_up_vector_i8.csv")

    do k=2, 25, 1
        n_i8 = 2**k
        i_i8 = n_i8/2_8
        allocate(x_r8(n_i8), x_i8(n_i8))
        call random_number(x_r8)
        x_r8 = 10 * x_r8
        x_i8 = x_r8
        n_iter=maxval((/10000000000_8/n_i8, 1_8/))
        n_iter=maxval((/5000000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/500000000_8/n_i8, 1_8/))
        ! n_iter=maxval((/50000000_8/n_i8, 1_8/))
        ! n_iter=1

        print*, '============================================================='
        do iter_types=1, n_types, 1
            res_r8=0d0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1); res_r8 = sum(x_r8)
                    case  (2); res_r8 = sum_up(x_r8, n_i8)
                    case  (3); res_r8 = sum_up_02_f_r8(x_r8, n_i8)
                    case  (4); res_r8 = sum_up_04_f_r8(x_r8, n_i8)
                    case  (5); res_r8 = sum_up_08_f_r8(x_r8, n_i8)
                    case  (6); res_r8 = sum_up_16_f_r8(x_r8, n_i8)
                    case  (7); res_r8 = sum_up_32_f_r8(x_r8, n_i8)
                    case  (8); res_r8 = sum_up_C_r8(x_r8, n_i8)
                    case  (9); res_r8 = sum_up_02_C_r8(x_r8, n_i8)
                    case (10); res_r8 = sum_up_04_C_r8(x_r8, n_i8)
                    case (11); res_r8 = sum_up_08_C_r8(x_r8, n_i8)
                    case (12); res_r8 = sum_up_16_C_r8(x_r8, n_i8)
                    case (13); res_r8 = sum_up_32_C_r8(x_r8, n_i8)
                    case (14); res_r8 = sum_up_04_ASM_r8(x_r8, n_i8)
                    case (15); res_r8 = sum_up_08_ASM_r8(x_r8, n_i8)
                    case (16); res_r8 = sum_up_16_ASM_r8(x_r8, n_i8)
                    case (17); res_r8 = sum_up_32_ASM_r8(x_r8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, f30.15)", types(iter_types), n_i8, real(times(iter_types)), res_r8
        end do
        write(10,*) k, n_i8, times

        print*, '============================================================='
        do iter_types=1, n_types, 1
            res_i8=0d0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1); res_i8 = sum(x_i8)
                    case  (2); res_i8 = sum_up(x_i8, n_i8)
                    case  (3); res_i8 = sum_up_02_f_i8(x_i8, n_i8)
                    case  (4); res_i8 = sum_up_04_f_i8(x_i8, n_i8)
                    case  (5); res_i8 = sum_up_08_f_i8(x_i8, n_i8)
                    case  (6); res_i8 = sum_up_16_f_i8(x_i8, n_i8)
                    case  (7); res_i8 = sum_up_32_f_i8(x_i8, n_i8)
                    case  (8); res_i8 = sum_up_C_i8(x_i8, n_i8)
                    case  (9); res_i8 = sum_up_02_C_i8(x_i8, n_i8)
                    case (10); res_i8 = sum_up_04_C_i8(x_i8, n_i8)
                    case (11); res_i8 = sum_up_08_C_i8(x_i8, n_i8)
                    case (12); res_i8 = sum_up_16_C_i8(x_i8, n_i8)
                    case (13); res_i8 = sum_up_32_C_i8(x_i8, n_i8)
                    case (14); res_i8 = sum_up_04_ASM_i8(x_i8, n_i8)
                    case (15); res_i8 = sum_up_08_ASM_i8(x_i8, n_i8)
                    case (16); res_i8 = sum_up_16_ASM_i8(x_i8, n_i8)
                    case (17); res_i8 = sum_up_32_ASM_i8(x_i8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, i15)", types(iter_types), n_i8, real(times(iter_types)), res_i8
        end do
        write(20,*) k, n_i8, times
        deallocate(x_r8, x_i8)
    end do




end program main_sum_up_vector
