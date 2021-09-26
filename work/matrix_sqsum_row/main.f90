program main
    use iso_c_binding
    use mod_const
    use mod_timer
    use mod_linalg
    implicit none

    integer(kind=8)              :: date_value1(8), date_value2(8)
    real(kind=8), allocatable, target    :: x(:,:), x_sqsum_row(:)
    integer(kind=8), allocatable :: array_n_samples(:), array_n_columns(:)
    integer(kind=8)              :: i, j

    real(kind=8), allocatable :: times(:)

    real(kind=8),     target, allocatable :: mat(:,:), mat_t(:,:)
    real(kind=8),     target, allocatable :: vec(:), vec_reshape(:,:)
    real(kind=8),     target, allocatable :: res(:), res_reshape(:,:)

    character(len=35), allocatable :: types(:)
    character(len=35) :: hoge

    integer(kind=8)   :: s, c, n_samples, n_columns, n_types, iter_types, iter, n_iter, base_iter
    type(c_ptr)       :: x_ptr
    integer(kind=8)   :: n_samples_list(5), n_columns_list(5)
    logical(kind=4)   :: is_stop_iter, is_stop_sample, is_stop_column


    allocate(array_n_samples(5))
    allocate(array_n_columns(5))

    is_stop_iter = f_
    is_stop_sample = f_
    is_stop_column = f_
    n_types = 99
    allocate(times(0:n_types))
    allocate(types(0:n_types))

    hoge = "hoge fuga piyo"
    types = hoge
    types(1)   = "matrix_sqsum_row_naive             :"

    ! types(2)   = "matrix_sqsum_row_01x01_F_r8        :"

    ! types(3)   = "matrix_sqsum_row_02x01_F_r8        :"
    ! types(4)   = "matrix_sqsum_row_04x01_F_r8        :"
    ! types(5)   = "matrix_sqsum_row_08x01_F_r8        :"
    ! types(6)   = "matrix_sqsum_row_16x01_F_r8        :"
    ! types(7)   = "matrix_sqsum_row_32x01_F_r8        :"

    ! types(8)   = "matrix_sqsum_row_01x02_F_r8        :"
    ! types(9)   = "matrix_sqsum_row_01x04_F_r8        :"
    ! types(10)  = "matrix_sqsum_row_01x08_F_r8        :"
    ! types(11)  = "matrix_sqsum_row_01x16_F_r8        :"
    ! types(12)  = "matrix_sqsum_row_01x32_F_r8        :"

    ! types(13)  = "matrix_sqsum_row_02x02_F_r8        :"
    ! types(14)  = "matrix_sqsum_row_02x04_F_r8        :"
    ! types(15)  = "matrix_sqsum_row_02x08_F_r8        :"
    ! types(16)  = "matrix_sqsum_row_02x16_F_r8        :"
    ! types(17)  = "matrix_sqsum_row_02x32_F_r8        :"

    ! types(18)  = "matrix_sqsum_row_04x02_F_r8        :"
    ! types(19)  = "matrix_sqsum_row_04x04_F_r8        :"
    ! types(20)  = "matrix_sqsum_row_04x08_F_r8        :"
    ! types(21)  = "matrix_sqsum_row_04x16_F_r8        :"
    ! types(22)  = "matrix_sqsum_row_04x32_F_r8        :"

    ! types(23)  = "matrix_sqsum_row_08x02_F_r8        :"
    ! types(24)  = "matrix_sqsum_row_08x04_F_r8        :"
    ! types(25)  = "matrix_sqsum_row_08x08_F_r8        :"
    ! types(26)  = "matrix_sqsum_row_08x16_F_r8        :"
    ! types(27)  = "matrix_sqsum_row_08x32_F_r8        :"

    ! types(28)  = "matrix_sqsum_row_01x01_C_r8        :"

    ! types(29)  = "matrix_sqsum_row_02x01_C_r8        :"
    ! types(30)  = "matrix_sqsum_row_04x01_C_r8        :"
    ! types(31)  = "matrix_sqsum_row_08x01_C_r8        :"
    ! types(32)  = "matrix_sqsum_row_16x01_C_r8        :"
    ! types(33)  = "matrix_sqsum_row_32x01_C_r8        :"

    ! types(29)  = "matrix_sqsum_row_02x01_C_r8        :"
    ! types(30)  = "matrix_sqsum_row_04x01_C_r8        :"
    ! types(31)  = "matrix_sqsum_row_08x01_C_r8        :"
    ! types(32)  = "matrix_sqsum_row_16x01_C_r8        :"
    ! types(33)  = "matrix_sqsum_row_32x01_C_r8        :"

    ! types(34)  = "matrix_sqsum_row_01x02_C_r8        :"
    ! types(35)  = "matrix_sqsum_row_01x04_C_r8        :"
    ! types(36)  = "matrix_sqsum_row_01x08_C_r8        :"
    ! types(37)  = "matrix_sqsum_row_01x16_C_r8        :"
    ! types(38)  = "matrix_sqsum_row_01x32_C_r8        :"

    ! types(39)  = "matrix_sqsum_row_02x02_C_r8        :"
    ! types(40)  = "matrix_sqsum_row_02x04_C_r8        :"
    ! types(41)  = "matrix_sqsum_row_02x08_C_r8        :"
    ! types(42)  = "matrix_sqsum_row_02x16_C_r8        :"
    ! types(43)  = "matrix_sqsum_row_02x32_C_r8        :"

    ! types(44)  = "matrix_sqsum_row_04x02_C_r8        :"
    ! types(45)  = "matrix_sqsum_row_04x04_C_r8        :"
    ! types(46)  = "matrix_sqsum_row_04x08_C_r8        :"
    ! types(47)  = "matrix_sqsum_row_04x16_C_r8        :"
    ! types(48)  = "matrix_sqsum_row_04x32_C_r8        :"

    ! types(49)  = "matrix_sqsum_row_08x02_C_r8        :"
    ! types(50)  = "matrix_sqsum_row_08x04_C_r8        :"
    ! types(51)  = "matrix_sqsum_row_08x08_C_r8        :"
    ! types(52)  = "matrix_sqsum_row_08x16_C_r8        :"
    ! types(53)  = "matrix_sqsum_row_08x32_C_r8        :"

    ! types(54)  = "matrix_sqsum_row_02x01_A_r8        :"
    ! types(55)  = "matrix_sqsum_row_04x01_A_r8        :"
    ! types(56)  = "matrix_sqsum_row_08x01_A_r8        :"
    ! types(57)  = "matrix_sqsum_row_16x01_A_r8        :"
    ! types(58)  = "matrix_sqsum_row_32x01_A_r8        :"

    ! types(59)  = "matrix_sqsum_row_02x02_A_r8        :"
    ! types(60)  = "matrix_sqsum_row_02x04_A_r8        :"
    ! types(61)  = "matrix_sqsum_row_02x08_A_r8        :"
    ! types(62)  = "matrix_sqsum_row_02x16_A_r8        :"
    ! types(63)  = "matrix_sqsum_row_02x32_A_r8        :"

    ! types(64)  = "matrix_sqsum_row_04x02_A_r8        :"
    ! types(65)  = "matrix_sqsum_row_04x04_A_r8        :"
    ! types(66)  = "matrix_sqsum_row_04x08_A_r8        :"
    ! types(67)  = "matrix_sqsum_row_04x16_A_r8        :"
    ! types(68)  = "matrix_sqsum_row_04x32_A_r8        :"

    ! types(69)  = "matrix_sqsum_row_08x02_A_r8        :"
    ! types(70)  = "matrix_sqsum_row_08x04_A_r8        :"
    ! types(71)  = "matrix_sqsum_row_08x08_A_r8        :"
    ! types(72)  = "matrix_sqsum_row_08x16_A_r8        :"
    ! types(73)  = "matrix_sqsum_row_08x32_A_r8        :"

    types(74)  = "matrix_sqsum_row_02x01_F_P_r8        :"
    types(75)  = "matrix_sqsum_row_04x01_F_P_r8        :"
    types(76)  = "matrix_sqsum_row_08x01_F_P_r8        :"
    types(77)  = "matrix_sqsum_row_16x01_F_P_r8        :"
    types(78)  = "matrix_sqsum_row_32x01_F_P_r8        :"


    array_n_samples = (/100, 1000, 10000, 100000, 1000000/)+31
    array_n_columns = (/5,   10,   50,    100,    200/)+27+31
    array_n_samples(:) = array_n_samples(size(array_n_samples):1:-1)
    base_iter = 1000000000_8
    ! base_iter = 1; is_stop_iter = t_
    base_iter = 1; is_stop_column = t_

    do i=1, size(array_n_samples), 1
        do j=1, size(array_n_columns), 1
            n_samples = array_n_samples(i)
            n_columns = array_n_columns(j)
            allocate(x(n_samples, n_columns), x_sqsum_row(n_samples))

            n_iter = maxval((/1_8, base_iter/n_samples/n_columns/))
            call random_number(x)
            x_ptr = c_loc(x)

            print*, '============================================================='
            print*, '============================================================='
            do iter_types=0, n_types, 1            
                if (types(iter_types) .eq. hoge)  cycle
                x_sqsum_row = 0d0
                call date_and_time(values=date_value1)
                do iter=1, n_iter, 1
                    select case(iter_types)
                        case  (1); x_sqsum_row = sum(x**2d0, dim=2)

                        case  (2); call matrix_sqsum_row_01x01_F_r8(x, x_sqsum_row, n_samples, n_columns)

                        case  (3); call matrix_sqsum_row_02x01_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case  (4); call matrix_sqsum_row_04x01_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case  (5); call matrix_sqsum_row_08x01_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case  (6); call matrix_sqsum_row_16x01_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case  (7); call matrix_sqsum_row_32x01_F_r8(x, x_sqsum_row, n_samples, n_columns)

                        case  (8); call matrix_sqsum_row_01x02_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case  (9); call matrix_sqsum_row_01x04_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (10); call matrix_sqsum_row_01x08_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (11); call matrix_sqsum_row_01x16_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (12); call matrix_sqsum_row_01x32_F_r8(x, x_sqsum_row, n_samples, n_columns)

                        case (13); call matrix_sqsum_row_02x02_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (14); call matrix_sqsum_row_02x04_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (15); call matrix_sqsum_row_02x08_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (16); call matrix_sqsum_row_02x16_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (17); call matrix_sqsum_row_02x32_F_r8(x, x_sqsum_row, n_samples, n_columns)

                        case (18); call matrix_sqsum_row_04x02_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (19); call matrix_sqsum_row_04x04_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (20); call matrix_sqsum_row_04x08_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (21); call matrix_sqsum_row_04x16_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (22); call matrix_sqsum_row_04x32_F_r8(x, x_sqsum_row, n_samples, n_columns)

                        case (23); call matrix_sqsum_row_08x02_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (24); call matrix_sqsum_row_08x04_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (25); call matrix_sqsum_row_08x08_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (26); call matrix_sqsum_row_08x16_F_r8(x, x_sqsum_row, n_samples, n_columns)
                        case (27); call matrix_sqsum_row_08x32_F_r8(x, x_sqsum_row, n_samples, n_columns)

                        case (28); call matrix_sqsum_row_01x01_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)

                        case (29); call matrix_sqsum_row_02x01_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (30); call matrix_sqsum_row_04x01_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (31); call matrix_sqsum_row_08x01_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (32); call matrix_sqsum_row_16x01_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (33); call matrix_sqsum_row_32x01_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)

                        case (34); call matrix_sqsum_row_01x02_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (35); call matrix_sqsum_row_01x04_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (36); call matrix_sqsum_row_01x08_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (37); call matrix_sqsum_row_01x16_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (38); call matrix_sqsum_row_01x32_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)

                        case (39); call matrix_sqsum_row_02x02_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (40); call matrix_sqsum_row_02x04_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (41); call matrix_sqsum_row_02x08_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (42); call matrix_sqsum_row_02x16_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (43); call matrix_sqsum_row_02x32_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)

                        case (44); call matrix_sqsum_row_04x02_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (45); call matrix_sqsum_row_04x04_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (46); call matrix_sqsum_row_04x08_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (47); call matrix_sqsum_row_04x16_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (48); call matrix_sqsum_row_04x32_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)

                        case (49); call matrix_sqsum_row_08x02_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (50); call matrix_sqsum_row_08x04_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (51); call matrix_sqsum_row_08x08_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (52); call matrix_sqsum_row_08x16_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (53); call matrix_sqsum_row_08x32_C_r8(x_ptr, x_sqsum_row, n_samples, n_columns)

                        case (54); call matrix_sqsum_row_02x01_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (55); call matrix_sqsum_row_04x01_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (56); call matrix_sqsum_row_08x01_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (57); call matrix_sqsum_row_16x01_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (58); call matrix_sqsum_row_32x01_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)

                        case (59); call matrix_sqsum_row_02x02_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (60); call matrix_sqsum_row_02x04_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (61); call matrix_sqsum_row_02x08_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (62); call matrix_sqsum_row_02x16_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (63); call matrix_sqsum_row_02x32_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)

                        case (64); call matrix_sqsum_row_04x02_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (65); call matrix_sqsum_row_04x04_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (66); call matrix_sqsum_row_04x08_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (67); call matrix_sqsum_row_04x16_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (68); call matrix_sqsum_row_04x32_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)

                        case (69); call matrix_sqsum_row_08x02_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (70); call matrix_sqsum_row_08x04_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (71); call matrix_sqsum_row_08x08_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (72); call matrix_sqsum_row_08x16_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                        case (73); call matrix_sqsum_row_08x32_A_r8(x_ptr, x_sqsum_row, n_samples, n_columns)
                    end select
                end do
                call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                print "(A, f30.15, i15, i15, f30.15)", types(iter_types), times(iter_types), n_samples, n_columns, sum(x_sqsum_row)
            end do
            deallocate(x, x_sqsum_row)
            if (is_stop_iter) stop
        end do
        if (is_stop_column) stop
    end do


    

contains





end program main
