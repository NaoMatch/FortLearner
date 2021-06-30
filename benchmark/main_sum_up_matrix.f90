program main_sum_up_vector
    use mod_stats
    use mod_timer
    implicit none
    
    integer(kind=8)           :: date_value1(8), date_value2(8)
    real(kind=8), allocatable :: times(:)

    character(len=30), allocatable :: types(:)
    character(len=30) :: hoge

    real(c_double),     target, allocatable :: x_r8(:,:), vec_r8(:)
    integer(c_int64_t), target, allocatable :: x_i8(:,:), vec_i8(:)
    real(c_double)                          :: res_r8
    integer(c_int64_t)                      :: res_i8

    type(c_ptr) :: x_r8_ptr, x_i8_ptr
    type(c_ptr) :: vec_r8_ptr, vec_i8_ptr

    integer(kind=8)                         :: j, k, n_i8, c_i8, n_iter, iter, n_types, iter_types
    character(len=128) :: filename_r8, filename_i8

    n_types = 81
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "sum_up_matrix_intrinsic    :"
    types(2)  = "sum_up_matrix_best?        :"
    ! types(3)  = "sum_up_matrix_parallel     :"
    ! types(4)  = "sum_up_matrix_loop         :"
    ! types(5)  = "sum_up_matrix_02_F         :"
    ! types(6)  = "sum_up_matrix_04_F         :"
    ! types(7)  = "sum_up_matrix_08_F         :"
    ! types(8)  = "sum_up_matrix_16_F         :"
    ! types(9)  = "sum_up_matrix_32_F         :"
    ! types(10) = "sum_up_matrix_02_02_F      :"
    ! types(11) = "sum_up_matrix_02_04_F      :"
    ! types(12) = "sum_up_matrix_02_08_F      :"
    ! types(13) = "sum_up_matrix_02_16_F      :"
    types(14) = "sum_up_matrix_04_02_F      :"
    types(15) = "sum_up_matrix_04_04_F      :"
    types(16) = "sum_up_matrix_04_08_F      :"
    types(17) = "sum_up_matrix_08_02_F      :"
    types(18) = "sum_up_matrix_08_04_F      :"
    ! types(19) = "sum_up_matrix_16_02_F      :"
    ! types(20) = "sum_up_matrix_16_04_F      :"
    ! types(21) = "sum_up_matrix_32_02_F      :"
    ! 
    ! types(22) = "sum_up_matrix_02_F_P       :"
    ! types(23) = "sum_up_matrix_04_F_P       :"
    ! types(24) = "sum_up_matrix_08_F_P       :"
    ! types(25) = "sum_up_matrix_16_F_P       :"
    ! types(26) = "sum_up_matrix_32_F_P       :"
    ! types(27) = "sum_up_matrix_02_02_F_P1   :"
    ! types(28) = "sum_up_matrix_02_02_F_P2   :"
    ! types(29) = "sum_up_matrix_02_04_F_P1   :"
    ! types(30) = "sum_up_matrix_02_04_F_P2   :"
    ! types(31) = "sum_up_matrix_02_04_F_P1   :"
    ! types(32) = "sum_up_matrix_02_04_F_P2   :"
    ! types(33) = "sum_up_matrix_04_02_F_P1   :"
    ! types(34) = "sum_up_matrix_04_02_F_P2   :"
    ! types(35) = "sum_up_matrix_04_04_F_P1   :"
    ! types(36) = "sum_up_matrix_04_04_F_P2   :"
    ! types(37) = "sum_up_matrix_04_08_F_P1   :"
    ! types(38) = "sum_up_matrix_04_08_F_P2   :"
    ! types(39) = "sum_up_matrix_08_02_F_P1   :"
    ! types(40) = "sum_up_matrix_08_02_F_P2   :"
    ! types(41) = "sum_up_matrix_08_04_F_P1   :"
    ! types(42) = "sum_up_matrix_08_04_F_P2   :"
    ! types(43) = "sum_up_matrix_16_02_F_P1   :"
    ! types(44) = "sum_up_matrix_16_02_F_P2   :"
    ! types(45) = "sum_up_matrix_16_04_F_P1   :"
    ! types(46) = "sum_up_matrix_16_04_F_P2   :"
    ! types(47) = "sum_up_matrix_32_02_F_P1   :"
    ! types(48) = "sum_up_matrix_32_02_F_P2   :"
    ! 
    ! types(49) = "sum_up_matrix_C            :"
    ! types(50) = "sum_up_matrix_02_C         :"
    ! types(51) = "sum_up_matrix_04_C         :"
    ! types(52) = "sum_up_matrix_08_C         :"
    ! types(53) = "sum_up_matrix_16_C         :"
    ! types(54) = "sum_up_matrix_32_C         :"
    ! types(55) = "sum_up_matrix_64_C         :"
    types(56) = "sum_up_matrix_02_02_C      :"
    types(57) = "sum_up_matrix_02_04_C      :"
    types(58) = "sum_up_matrix_02_08_C      :"
    types(59) = "sum_up_matrix_04_02_C      :"
    types(60) = "sum_up_matrix_04_04_C      :"
    types(61) = "sum_up_matrix_04_08_C      :"
    types(62) = "sum_up_matrix_08_02_C      :"
    types(63) = "sum_up_matrix_08_04_C      :"
    types(64) = "sum_up_matrix_16_02_C      :"
    types(65) = "sum_up_matrix_16_04_C      :"
    types(66) = "sum_up_matrix_32_02_C      :"
    ! types(67) = "sum_up_matrix_04_ASM       :"
    ! types(68) = "sum_up_matrix_08_ASM       :"
    ! types(69) = "sum_up_matrix_16_ASM       :"
    ! types(70) = "sum_up_matrix_32_ASM       :"
    ! types(71) = "sum_up_matrix_64_ASM       :"
    types(72) = "sum_up_matrix_04_02_ASM    :"
    types(73) = "sum_up_matrix_04_04_ASM    :"
    types(74) = "sum_up_matrix_08_02_ASM    :"
    types(75) = "sum_up_matrix_08_04_ASM    :"
    types(76) = "sum_up_matrix_16_02_ASM    :"
    ! types(77) = "sum_up_matrix_32_02_ASM    :"
    ! types(80) = "sum_up_matrix_C_hybrid     :"
    ! types(81) = "sum_up_matrix_04_04_ASM_Pre:"

    do j=2, 8, 1
        c_i8 = 2**j

        write (filename_r8, '("time_sum_up_matrix_r8_", i4.4, "_.txt")') c_i8
        write (filename_i8, '("time_sum_up_matrix_i8_", i4.4, "_.txt")') c_i8

        open(10, file=filename_r8)
        open(20, file=filename_i8)

        do k=4, 48, 1
            n_i8 = minval((/2d0**(k/2d0), 10000000d0/))
            allocate(vec_r8(c_i8), vec_i8(c_i8))
            allocate(x_r8(n_i8,c_i8), x_i8(n_i8,c_i8))
            x_r8_ptr   = c_loc(x_r8)
            x_i8_ptr   = c_loc(x_i8)
            vec_r8_ptr = c_loc(vec_r8)
            vec_i8_ptr = c_loc(vec_i8)
            call random_number(x_r8)
            x_r8 = 10 * x_r8
            x_i8 = x_r8
            ! n_iter=maxval((/10000000000_8/n_i8/c_i8, 1_8/))
            n_iter=maxval((/5000000000_8/n_i8/c_i8, 1_8/))
            n_iter=maxval((/1000000000_8/n_i8/c_i8, 1_8/))
            ! n_iter=maxval((/100000000_8/n_i8/c_i8, 1_8/))
            ! n_iter=1

            print*, '============================================================='
            do iter_types=1, n_types, 1
                if (types(iter_types) .eq. hoge)  cycle
                vec_r8=0d0
                call date_and_time(values=date_value1)
                do iter=1, n_iter, 1
                    select case(iter_types)
                        case  (1); vec_r8 = sum(x_r8, dim=1)
                        case  (2); call sum_up_matrix_r8(vec_r8, x_r8, n_i8, c_i8)
                        case  (3); call sum_up_matrix_parallel_r8(vec_r8, x_r8, n_i8, c_i8)
                        case  (4); call sum_up_matrix_naive_r8(vec_r8, x_r8, n_i8, c_i8)
                        case  (5); call sum_up_matrix_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case  (6); call sum_up_matrix_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case  (7); call sum_up_matrix_08_r8(vec_r8, x_r8, n_i8, c_i8)
                        case  (8); call sum_up_matrix_16_r8(vec_r8, x_r8, n_i8, c_i8)
                        case  (9); call sum_up_matrix_32_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (10); call sum_up_matrix_02_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (11); call sum_up_matrix_02_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (12); call sum_up_matrix_02_08_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (13); call sum_up_matrix_02_16_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (14); call sum_up_matrix_04_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (15); call sum_up_matrix_04_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (16); call sum_up_matrix_04_08_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (17); call sum_up_matrix_08_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (18); call sum_up_matrix_08_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (19); call sum_up_matrix_16_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (20); call sum_up_matrix_16_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (21); call sum_up_matrix_32_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (22); call sum_up_matrix_parallel_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (23); call sum_up_matrix_parallel_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (24); call sum_up_matrix_parallel_08_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (25); call sum_up_matrix_parallel_16_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (26); call sum_up_matrix_parallel_32_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (27); call sum_up_matrix_parallel_ver01_02_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (28); call sum_up_matrix_parallel_ver02_02_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (29); call sum_up_matrix_parallel_ver01_02_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (30); call sum_up_matrix_parallel_ver02_02_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (31); call sum_up_matrix_parallel_ver01_02_08_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (32); call sum_up_matrix_parallel_ver02_02_08_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (33); call sum_up_matrix_parallel_ver01_04_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (34); call sum_up_matrix_parallel_ver02_04_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (35); call sum_up_matrix_parallel_ver01_04_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (36); call sum_up_matrix_parallel_ver02_04_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (37); call sum_up_matrix_parallel_ver01_04_08_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (38); call sum_up_matrix_parallel_ver02_04_08_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (39); call sum_up_matrix_parallel_ver01_08_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (40); call sum_up_matrix_parallel_ver02_08_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (41); call sum_up_matrix_parallel_ver01_08_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (42); call sum_up_matrix_parallel_ver02_08_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (43); call sum_up_matrix_parallel_ver01_16_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (44); call sum_up_matrix_parallel_ver02_16_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (45); call sum_up_matrix_parallel_ver01_16_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (46); call sum_up_matrix_parallel_ver02_16_04_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (47); call sum_up_matrix_parallel_ver01_32_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (48); call sum_up_matrix_parallel_ver02_32_02_r8(vec_r8, x_r8, n_i8, c_i8)
                        case (49); call sum_up_matrix_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (50); call sum_up_matrix_02_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (51); call sum_up_matrix_04_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (52); call sum_up_matrix_08_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (53); call sum_up_matrix_16_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (54); call sum_up_matrix_32_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (55); call sum_up_matrix_64_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (56); call sum_up_matrix_02_02_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (57); call sum_up_matrix_02_04_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (58); call sum_up_matrix_02_08_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (59); call sum_up_matrix_04_02_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (60); call sum_up_matrix_04_04_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (61); call sum_up_matrix_04_08_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (62); call sum_up_matrix_08_02_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (63); call sum_up_matrix_08_04_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (64); call sum_up_matrix_16_02_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (65); call sum_up_matrix_16_04_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (66); call sum_up_matrix_32_02_C_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (67); call sum_up_matrix_04_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (68); call sum_up_matrix_08_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (69); call sum_up_matrix_16_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (70); call sum_up_matrix_32_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (71); call sum_up_matrix_64_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (72); call sum_up_matrix_04_02_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (73); call sum_up_matrix_04_04_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (74); call sum_up_matrix_08_02_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (75); call sum_up_matrix_08_04_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (76); call sum_up_matrix_16_02_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (77); call sum_up_matrix_32_02_ASM_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (80); call sum_up_matrix_C_hybrid_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                        case (81); call sum_up_matrix_04_04_ASM_Pre_r8(vec_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    end select
                end do
                call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                print "(A, i15, i15, f30.15, f30.15)", types(iter_types), n_i8, c_i8, real(times(iter_types)), sum(vec_r8)
            end do
            write(10,*) k, n_i8, times

            print*, '============================================================='
            do iter_types=1, n_types, 1
                if (types(iter_types) .eq. hoge)  cycle
                vec_i8=0d0
                call date_and_time(values=date_value1)
                do iter=1, n_iter, 1
                    select case(iter_types)
                        case  (1); vec_i8 = sum(x_i8, dim=1)
                        case  (2); call sum_up_matrix_i8(vec_i8, x_i8, n_i8, c_i8)
                        case  (3); call sum_up_matrix_parallel_i8(vec_i8, x_i8, n_i8, c_i8)
                        case  (4); call sum_up_matrix_naive_i8(vec_i8, x_i8, n_i8, c_i8)
                        case  (5); call sum_up_matrix_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case  (6); call sum_up_matrix_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case  (7); call sum_up_matrix_08_i8(vec_i8, x_i8, n_i8, c_i8)
                        case  (8); call sum_up_matrix_16_i8(vec_i8, x_i8, n_i8, c_i8)
                        case  (9); call sum_up_matrix_32_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (10); call sum_up_matrix_02_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (11); call sum_up_matrix_02_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (12); call sum_up_matrix_02_08_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (13); call sum_up_matrix_02_16_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (14); call sum_up_matrix_04_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (15); call sum_up_matrix_04_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (16); call sum_up_matrix_04_08_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (17); call sum_up_matrix_08_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (18); call sum_up_matrix_08_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (19); call sum_up_matrix_16_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (20); call sum_up_matrix_16_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (21); call sum_up_matrix_32_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (22); call sum_up_matrix_parallel_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (23); call sum_up_matrix_parallel_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (24); call sum_up_matrix_parallel_08_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (25); call sum_up_matrix_parallel_16_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (26); call sum_up_matrix_parallel_32_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (27); call sum_up_matrix_parallel_ver01_02_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (28); call sum_up_matrix_parallel_ver02_02_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (29); call sum_up_matrix_parallel_ver01_02_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (30); call sum_up_matrix_parallel_ver02_02_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (31); call sum_up_matrix_parallel_ver01_02_08_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (32); call sum_up_matrix_parallel_ver02_02_08_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (33); call sum_up_matrix_parallel_ver01_04_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (34); call sum_up_matrix_parallel_ver02_04_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (35); call sum_up_matrix_parallel_ver01_04_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (36); call sum_up_matrix_parallel_ver02_04_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (37); call sum_up_matrix_parallel_ver01_04_08_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (38); call sum_up_matrix_parallel_ver02_04_08_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (39); call sum_up_matrix_parallel_ver01_08_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (40); call sum_up_matrix_parallel_ver02_08_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (41); call sum_up_matrix_parallel_ver01_08_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (42); call sum_up_matrix_parallel_ver02_08_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (43); call sum_up_matrix_parallel_ver01_16_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (44); call sum_up_matrix_parallel_ver02_16_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (45); call sum_up_matrix_parallel_ver01_16_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (46); call sum_up_matrix_parallel_ver02_16_04_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (47); call sum_up_matrix_parallel_ver01_32_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (48); call sum_up_matrix_parallel_ver02_32_02_i8(vec_i8, x_i8, n_i8, c_i8)
                        case (49); call sum_up_matrix_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (50); call sum_up_matrix_02_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (51); call sum_up_matrix_04_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (52); call sum_up_matrix_08_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (53); call sum_up_matrix_16_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (54); call sum_up_matrix_32_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (55); call sum_up_matrix_64_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (56); call sum_up_matrix_02_02_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (57); call sum_up_matrix_02_04_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (58); call sum_up_matrix_02_08_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (59); call sum_up_matrix_04_02_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (60); call sum_up_matrix_04_04_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (61); call sum_up_matrix_04_08_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (62); call sum_up_matrix_08_02_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (63); call sum_up_matrix_08_04_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (64); call sum_up_matrix_16_02_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (65); call sum_up_matrix_16_04_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (66); call sum_up_matrix_32_02_C_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (67); call sum_up_matrix_04_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (68); call sum_up_matrix_08_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (69); call sum_up_matrix_16_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (70); call sum_up_matrix_32_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (71); call sum_up_matrix_64_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (72); call sum_up_matrix_04_02_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (73); call sum_up_matrix_04_04_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (74); call sum_up_matrix_08_02_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (75); call sum_up_matrix_08_04_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (76); call sum_up_matrix_16_02_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (77); call sum_up_matrix_32_02_ASM_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (80); call sum_up_matrix_C_hybrid_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                        case (81); call sum_up_matrix_04_04_ASM_Pre_i8(vec_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    end select
                end do
                call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                print "(A, i15, i15, f30.15, i15)", types(iter_types), n_i8, c_i8, real(times(iter_types)), sum(vec_i8)
            end do
            write(20,*) k, n_i8, times
            deallocate(x_r8, x_i8)
            deallocate(vec_r8, vec_i8)
        end do

        close(10)
        close(20)
    end do


end program main_sum_up_vector
