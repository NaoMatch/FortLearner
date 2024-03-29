program main_mat_x_vec
    !$ use omp_lib
    use mod_timer
    use mod_linalg
    implicit none
    
    integer(kind=8) :: date_value1(8), date_value2(8)
    real(kind=8), allocatable :: times(:)

    real(kind=8),     target, allocatable :: mat(:,:), mat_t(:,:)
    real(kind=8),     target, allocatable :: vec(:), vec_reshape(:,:)
    real(kind=8),     target, allocatable :: res(:), res_reshape(:,:)

    character(len=35), allocatable :: types(:)
    character(len=35) :: hoge

    integer(kind=8)   :: s, c, n_samples, n_columns, n_types, iter_types, iter, n_iter, base_iter
    type(c_ptr)       :: mat_ptr
    integer(kind=8)   :: n_samples_list(5), n_columns_list(5)
    !$ double precision st, en

    n_types = 99
    allocate(times(0:n_types))
    allocate(types(0:n_types))

    hoge = "hoge fuga piyo"
    types = hoge
    types(0)   = "call dgemv_openblas             :"

    types(1)   = "call multi_mat_vec              :"

    types(2)   = "call multi_mat_vec_02x01_N_F_r8 :"
    ! types(3)   = "call multi_mat_vec_02x02_N_F_r8 :"
    ! types(4)   = "call multi_mat_vec_02x04_N_F_r8 :"
    ! types(5)   = "call multi_mat_vec_02x08_N_F_r8 :"
    ! types(6)   = "call multi_mat_vec_02x16_N_F_r8 :"
    ! types(7)   = "call multi_mat_vec_02x32_N_F_r8 :"

    ! types(8)   = "call multi_mat_vec_04x01_N_F_r8 :"
    ! types(9)   = "call multi_mat_vec_04x02_N_F_r8 :"
    ! types(10)  = "call multi_mat_vec_04x04_N_F_r8 :"
    ! types(11)  = "call multi_mat_vec_04x08_N_F_r8 :"
    ! types(12)  = "call multi_mat_vec_04x16_N_F_r8 :"
    ! types(13)  = "call multi_mat_vec_04x32_N_F_r8 :"

    ! types(14)  = "call multi_mat_vec_08x01_N_F_r8 :"
    ! types(15)  = "call multi_mat_vec_08x02_N_F_r8 :"
    ! types(16)  = "call multi_mat_vec_08x04_N_F_r8 :"
    ! types(17)  = "call multi_mat_vec_08x08_N_F_r8 :"
    ! types(18)  = "call multi_mat_vec_08x16_N_F_r8 :"
    ! types(19)  = "call multi_mat_vec_08x32_N_F_r8 :"

    ! types(20)  = "call multi_mat_vec_02x01_N_F_P_r8 :"
    ! types(21)  = "call multi_mat_vec_02x02_N_F_P_r8 :"
    ! types(22)  = "call multi_mat_vec_02x04_N_F_P_r8 :"
    ! types(23)  = "call multi_mat_vec_02x08_N_F_P_r8 :"
    ! types(24)  = "call multi_mat_vec_02x16_N_F_P_r8 :"
    ! types(25)  = "call multi_mat_vec_02x32_N_F_P_r8 :"

    ! types(26)  = "call multi_mat_vec_04x01_N_F_P_r8 :"
    ! types(27)  = "call multi_mat_vec_04x02_N_F_P_r8 :"
    ! types(28)  = "call multi_mat_vec_04x04_N_F_P_r8 :"
    ! types(29)  = "call multi_mat_vec_04x08_N_F_P_r8 :"
    ! types(30)  = "call multi_mat_vec_04x16_N_F_P_r8 :"
    ! types(31)  = "call multi_mat_vec_04x32_N_F_P_r8 :"

    ! types(32)  = "call multi_mat_vec_08x01_N_F_P_r8 :"
    ! types(33)  = "call multi_mat_vec_08x02_N_F_P_r8 :"
    ! types(34)  = "call multi_mat_vec_08x04_N_F_P_r8 :"
    ! types(35)  = "call multi_mat_vec_08x08_N_F_P_r8 :"
    ! types(36)  = "call multi_mat_vec_08x16_N_F_P_r8 :"
    ! types(37)  = "call multi_mat_vec_08x32_N_F_P_r8 :"

    base_iter = 5000000000_8
    ! base_iter = 5000000_8
    base_iter = 1

    n_samples_list = (/100_8, 1000_8, 10000_8, 100000_8, 1000000_8/) + 31
    n_columns_list = (/10_8, 50_8, 100_8, 200_8, 400_8/) + 31

    do s=1, size(n_samples_list), 1
        ! n_samples = minval((/2d0**(s/2d0), 10000000d0/)) + 31
        n_samples = n_samples_list(s)

        do c=1, size(n_columns_list), 1
            n_columns = 32*c + 7
            n_columns = n_columns_list(c)
            n_iter = maxval((/1_8, base_iter/n_samples/n_columns/))

            allocate(mat(n_samples, n_columns), mat_t(n_columns, n_samples))
            allocate(vec(n_columns), res(n_samples))
            allocate(vec_reshape(n_columns,1), res_reshape(n_samples,1))

            call random_number(mat)
            mat_t = transpose(mat)
            mat_ptr = c_loc(mat)

            call random_number(vec)
            vec_reshape = reshape(vec, shape=(/n_columns, 1_8/))
            res_reshape = reshape(res, shape=(/n_samples, 1_8/))

            print*, '============================================================='
            print*, '============================================================='
            do iter_types=0, n_types, 1            
                if (types(iter_types) .eq. hoge)  cycle
                res = 0d0
                !$ st = omp_get_wtime()
                call date_and_time(values=date_value1)
                do iter=1, n_iter, 1
                    select case(iter_types)
                        case  (0);  CALL DGEMV( "N", n_samples, n_columns, 1d0, mat, &
                                            n_samples, vec, 1_8, 0d0, res,&
                                            1_8 )
                        case  (1); call multi_mat_vec_r8(mat, vec, res, n_samples, n_columns, parallel=f_)
                        case  (2); call multi_mat_vec_r8(mat, vec, res, n_samples, n_columns, parallel=t_)

                        ! case  (2); call multi_mat_vec_02x01_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case  (3); call multi_mat_vec_02x02_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case  (4); call multi_mat_vec_02x04_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case  (5); call multi_mat_vec_02x08_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case  (6); call multi_mat_vec_02x16_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case  (7); call multi_mat_vec_02x32_N_F_B_r8(mat, vec, res, n_samples, n_columns)

                        ! case  (8); call multi_mat_vec_04x01_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case  (9); call multi_mat_vec_04x02_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case (10); call multi_mat_vec_04x04_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case (11); call multi_mat_vec_04x08_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case (12); call multi_mat_vec_04x16_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case (13); call multi_mat_vec_04x32_N_F_B_r8(mat, vec, res, n_samples, n_columns)

                        ! case (14); call multi_mat_vec_08x01_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case (15); call multi_mat_vec_08x02_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case (16); call multi_mat_vec_08x04_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case (17); call multi_mat_vec_08x08_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case (18); call multi_mat_vec_08x16_N_F_B_r8(mat, vec, res, n_samples, n_columns)
                        ! case (19); call multi_mat_vec_08x32_N_F_B_r8(mat, vec, res, n_samples, n_columns)

                        ! case (20); call multi_mat_vec_02x01_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (21); call multi_mat_vec_02x02_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (22); call multi_mat_vec_02x04_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (23); call multi_mat_vec_02x08_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (24); call multi_mat_vec_02x16_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (25); call multi_mat_vec_02x32_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)

                        ! case (26); call multi_mat_vec_04x01_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (27); call multi_mat_vec_04x02_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (28); call multi_mat_vec_04x04_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (29); call multi_mat_vec_04x08_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (30); call multi_mat_vec_04x16_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (31); call multi_mat_vec_04x32_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)

                        ! case (32); call multi_mat_vec_08x01_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (33); call multi_mat_vec_08x02_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (34); call multi_mat_vec_08x04_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (35); call multi_mat_vec_08x08_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (36); call multi_mat_vec_08x16_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)
                        ! case (37); call multi_mat_vec_08x32_N_F_B_P_r8(mat, vec, res, n_samples, n_columns)

                    end select
                end do
                call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                !$ en = omp_get_wtime()
                !$ print *, "Elapsed time in second is:", en-st                
                print "(A, f30.15, i15, i15, f30.15)", types(iter_types), times(iter_types), n_samples, n_columns, sum(res)
            end do
            deallocate(mat, mat_t, vec, res, vec_reshape, res_reshape)
            ! stop
        end do
    end do

    call date_and_time(values=date_value1)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

end program main_mat_x_vec
