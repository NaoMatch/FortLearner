program extract_m2v
    use mod_stats
    use mod_timer
    use mod_sort
    use mod_random
    implicit none
    
    integer(kind=8)           :: date_value1(8), date_value2(8)
    real(kind=8), allocatable :: times(:)

    character(len=30), allocatable :: types(:)
    character(len=30) :: hoge

    real(c_double),     target, allocatable :: x_r8(:,:), v_r8(:)
    integer(c_int64_t), target, allocatable :: x_i8(:,:), v_i8(:)
    integer(c_int64_t), target, allocatable :: i_i8(:), i_full_i8(:)
    real(c_double)                          :: res_r8
    integer(c_int64_t)                      :: res_i8

    type(c_ptr) :: x_r8_ptr, x_i8_ptr
    type(c_ptr) :: v_r8_ptr, v_i8_ptr
    type(c_ptr) :: i_i8_ptr

    real(kind=8), ALLOCATABLE    :: col_ids_r(:)
    integer(kind=8), ALLOCATABLE :: col_ids_i(:)
    integer(kind=8) :: j, k, n_i8, c_i8, j_i8, n_iter, iter, n_types, iter_types, iii, col_id
    character(len=128) :: filename_r8, filename_i8

    n_types = 8
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "extract_naive    :"
    types(2)  = "extract_loop     :"
    types(3)  = "extract_02_F     :"
    types(4)  = "extract_04_F     :"
    types(5)  = "extract_08_F     :"
    types(6)  = "extract_16_F     :"
    types(7)  = "extract_buf_08_F :"
    types(8)  = "extract_dir_08_F :"

    do j=2, 8, 1
        c_i8 = 2**j

        write (filename_r8, '("time_extract_matrix_r8_", i4.4, "_.txt")') c_i8
        write (filename_i8, '("time_extract_matrix_i8_", i4.4, "_.txt")') c_i8

        open(10, file=filename_r8)
        open(20, file=filename_i8)

        do k=20, 48, 1
            n_i8 = minval((/2d0**(k/2d0), 10000000d0/))
            j_i8 = n_i8/4_8
            
            allocate(i_full_i8(n_i8), i_i8(j_i8))
            allocate(v_r8(j_i8),      v_i8(j_i8))
            allocate(x_r8(n_i8,c_i8), x_i8(n_i8,c_i8))
            x_r8_ptr = c_loc(x_r8)
            x_i8_ptr = c_loc(x_i8)
            v_r8_ptr = c_loc(v_r8)
            v_i8_ptr = c_loc(v_i8)
            i_i8_ptr = c_loc(i_i8)
            call random_number(x_r8)
            x_r8 = 10 * x_r8
            x_i8 = x_r8
            n_iter=maxval((/10000000000_8/n_i8, 1_8/))
            ! n_iter=maxval((/5000000000_8/n_i8, 1_8/))
            n_iter=maxval((/1000000000_8/n_i8, 1_8/))
            ! n_iter=maxval((/100000000_8/n_i8, 1_8/))
            ! n_iter=10

            allocate(col_ids_r(n_iter))
            allocate(col_ids_i(n_iter))
            call RANDOM_NUMBER(col_ids_r)
            col_ids_i = col_ids_r * c_i8 + 1

            do iii=1, n_i8, 1
                i_full_i8(iii) = iii
            end do
            call permutation(i_full_i8, n_i8)
            i_i8 = i_full_i8(1:j_i8)
            call quick_sort(i_i8, j_i8)

            print*, '============================================================='
            do iter_types=1, n_types, 1
                if (types(iter_types) .eq. hoge)  cycle
                v_r8=0d0
                call date_and_time(values=date_value1)
                col_id = mod(iter_types, c_i8)+1
                do iter=1, n_iter, 1
                    select case(iter_types)
                        case  (1); v_r8 = x_r8(i_i8, 2)
                        case  (2); call extract_matrix2vector_loop_r8(v_r8, x_r8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (3); call extract_matrix2vector_02_F_r8(v_r8, x_r8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (4); call extract_matrix2vector_04_F_r8(v_r8, x_r8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (5); call extract_matrix2vector_08_F_r8(v_r8, x_r8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (6); call extract_matrix2vector_16_F_r8(v_r8, x_r8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (7); call extract_matrix2vector_buffer_08_F_r8(v_r8, x_r8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (8); call extract_matrix2vector_direct_08_F_r8(v_r8, x_r8, i_i8, n_i8, c_i8, j_i8, col_id)
                    end select
                end do
                call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                print "(A, i15, i15, f30.15, f30.15)", types(iter_types), n_i8, c_i8, real(times(iter_types)), sum_up(v_r8, j_i8)
            end do
            write(10,*) k, n_i8, times

            print*, '============================================================='
            do iter_types=1, n_types, 1
                if (types(iter_types) .eq. hoge)  cycle
                v_i8=0d0
                call date_and_time(values=date_value1)
                do iter=1, n_iter, 1
                    col_id = mod(iter, c_i8)+1
                    select case(iter_types)
                        case  (1); v_i8 = x_i8(i_i8, 2)
                        case  (2); call extract_matrix2vector_loop_i8(v_i8, x_i8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (3); call extract_matrix2vector_02_F_i8(v_i8, x_i8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (4); call extract_matrix2vector_04_F_i8(v_i8, x_i8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (5); call extract_matrix2vector_08_F_i8(v_i8, x_i8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (6); call extract_matrix2vector_16_F_i8(v_i8, x_i8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (7); call extract_matrix2vector_buffer_08_F_i8(v_i8, x_i8, i_i8, n_i8, c_i8, j_i8, col_id)
                        case  (8); call extract_matrix2vector_direct_08_F_i8(v_i8, x_i8, i_i8, n_i8, c_i8, j_i8, col_id)
                    end select
                end do
                call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                print "(A, i15, i15, f30.15, i15)", types(iter_types), n_i8, c_i8, real(times(iter_types)), sum_up(v_i8, j_i8)
            end do
            write(20,*) k, n_i8, times
            deallocate(x_r8, x_i8)
            deallocate(v_r8, v_i8)
            deallocate(i_full_i8, i_i8)
            deallocate(col_ids_i, col_ids_r)
        end do

        close(10)
        close(20)
    end do

contains
    subroutine extract_matrix2vector_loop_r8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        real(kind=8), intent(inout) :: vec(n_ids)
        real(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: i, idx

        do i=1, n_ids, 1
            idx = ids(i)
            vec(i) = mat(idx, col_id)
        end do
    end subroutine extract_matrix2vector_loop_r8

    subroutine extract_matrix2vector_loop_i8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        integer(kind=8), intent(inout) :: vec(n_ids)
        integer(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in)    :: ids(n_ids)
        integer(kind=8), intent(in)    :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: i, idx

        do i=1, n_ids, 1
            idx = ids(i)
            vec(i) = mat(idx, col_id)
        end do
    end subroutine extract_matrix2vector_loop_i8

    subroutine extract_matrix2vector_02_F_r8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        real(kind=8), intent(inout) :: vec(n_ids)
        real(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain
        integer(kind=8) :: idx00, idx01

        unroll_size = 2
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            idx00 = ids(i)
            idx01 = ids(i+1)
            vec(i)   = mat(idx00, col_id)
            vec(i+1) = mat(idx01, col_id)
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                idx00  = ids(i)
                vec(i) = mat(idx00, col_id)
            end do
        end if
    end subroutine extract_matrix2vector_02_F_r8

    subroutine extract_matrix2vector_02_F_i8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        integer(kind=8), intent(inout) :: vec(n_ids)
        integer(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in)    :: ids(n_ids)
        integer(kind=8), intent(in)    :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain
        integer(kind=8) :: idx00, idx01

        unroll_size = 2
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            idx00 = ids(i)
            idx01 = ids(i+1)
            vec(i)   = mat(idx00, col_id)
            vec(i+1) = mat(idx01, col_id)
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                idx00  = ids(i)
                vec(i) = mat(idx00, col_id)
            end do
        end if
    end subroutine extract_matrix2vector_02_F_i8

    subroutine extract_matrix2vector_04_F_r8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        real(kind=8), intent(inout) :: vec(n_ids)
        real(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain
        integer(kind=8) :: idx00, idx01, idx02, idx03

        unroll_size = 4
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            idx00 = ids(i)
            idx01 = ids(i+1)
            idx02 = ids(i+2)
            idx03 = ids(i+3)
            vec(i)   = mat(idx00, col_id)
            vec(i+1) = mat(idx01, col_id)
            vec(i+2) = mat(idx02, col_id)
            vec(i+3) = mat(idx03, col_id)
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                idx00  = ids(i)
                vec(i) = mat(idx00, col_id)
            end do
        end if
    end subroutine extract_matrix2vector_04_F_r8

    subroutine extract_matrix2vector_04_F_i8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        integer(kind=8), intent(inout) :: vec(n_ids)
        integer(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in)    :: ids(n_ids)
        integer(kind=8), intent(in)    :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain
        integer(kind=8) :: idx00, idx01, idx02, idx03

        unroll_size = 4
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            idx00 = ids(i)
            idx01 = ids(i+1)
            idx02 = ids(i+2)
            idx03 = ids(i+3)
            vec(i)   = mat(idx00, col_id)
            vec(i+1) = mat(idx01, col_id)
            vec(i+2) = mat(idx02, col_id)
            vec(i+3) = mat(idx03, col_id)
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                idx00  = ids(i)
                vec(i) = mat(idx00, col_id)
            end do
        end if
    end subroutine extract_matrix2vector_04_F_i8

    subroutine extract_matrix2vector_08_F_r8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        real(kind=8), intent(inout) :: vec(n_ids)
        real(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain
        integer(kind=8) :: idx00, idx01, idx02, idx03
        integer(kind=8) :: idx04, idx05, idx06, idx07

        unroll_size = 8
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            idx00 = ids(i)
            idx01 = ids(i+1)
            idx02 = ids(i+2)
            idx03 = ids(i+3)
            idx04 = ids(i+4)
            idx05 = ids(i+5)
            idx06 = ids(i+6)
            idx07 = ids(i+7)
            vec(i)   = mat(idx00, col_id)
            vec(i+1) = mat(idx01, col_id)
            vec(i+2) = mat(idx02, col_id)
            vec(i+3) = mat(idx03, col_id)
            vec(i+4) = mat(idx04, col_id)
            vec(i+5) = mat(idx05, col_id)
            vec(i+6) = mat(idx06, col_id)
            vec(i+7) = mat(idx07, col_id)
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                idx00  = ids(i)
                vec(i) = mat(idx00, col_id)
            end do
        end if
    end subroutine extract_matrix2vector_08_F_r8

    subroutine extract_matrix2vector_08_F_i8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        integer(kind=8), intent(inout) :: vec(n_ids)
        integer(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in)    :: ids(n_ids)
        integer(kind=8), intent(in)    :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain
        integer(kind=8) :: idx00, idx01, idx02, idx03
        integer(kind=8) :: idx04, idx05, idx06, idx07

        unroll_size = 8
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            idx00 = ids(i)
            idx01 = ids(i+1)
            idx02 = ids(i+2)
            idx03 = ids(i+3)
            idx04 = ids(i+4)
            idx05 = ids(i+5)
            idx06 = ids(i+6)
            idx07 = ids(i+7)
            vec(i)   = mat(idx00, col_id)
            vec(i+1) = mat(idx01, col_id)
            vec(i+2) = mat(idx02, col_id)
            vec(i+3) = mat(idx03, col_id)
            vec(i+4) = mat(idx04, col_id)
            vec(i+5) = mat(idx05, col_id)
            vec(i+6) = mat(idx06, col_id)
            vec(i+7) = mat(idx07, col_id)
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                idx00  = ids(i)
                vec(i) = mat(idx00, col_id)
            end do
        end if
    end subroutine extract_matrix2vector_08_F_i8

    subroutine extract_matrix2vector_16_F_r8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        real(kind=8), intent(inout) :: vec(n_ids)
        real(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain
        integer(kind=8) :: idx00, idx01, idx02, idx03
        integer(kind=8) :: idx04, idx05, idx06, idx07
        integer(kind=8) :: idx08, idx09, idx10, idx11
        integer(kind=8) :: idx12, idx13, idx14, idx15

        unroll_size = 16
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            idx00 = ids(i)
            idx01 = ids(i+1)
            idx02 = ids(i+2)
            idx03 = ids(i+3)
            idx04 = ids(i+4)
            idx05 = ids(i+5)
            idx06 = ids(i+6)
            idx07 = ids(i+7)
            idx08 = ids(i+8)
            idx09 = ids(i+9)
            idx10 = ids(i+10)
            idx11 = ids(i+11)
            idx12 = ids(i+12)
            idx13 = ids(i+13)
            idx14 = ids(i+14)
            idx15 = ids(i+15)
            vec(i)    = mat(idx00, col_id)
            vec(i+1)  = mat(idx01, col_id)
            vec(i+2)  = mat(idx02, col_id)
            vec(i+3)  = mat(idx03, col_id)
            vec(i+4)  = mat(idx04, col_id)
            vec(i+5)  = mat(idx05, col_id)
            vec(i+6)  = mat(idx06, col_id)
            vec(i+7)  = mat(idx07, col_id)
            vec(i+8)  = mat(idx08, col_id)
            vec(i+9)  = mat(idx09, col_id)
            vec(i+10) = mat(idx10, col_id)
            vec(i+11) = mat(idx11, col_id)
            vec(i+12) = mat(idx12, col_id)
            vec(i+13) = mat(idx13, col_id)
            vec(i+14) = mat(idx14, col_id)
            vec(i+15) = mat(idx15, col_id)
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                idx00  = ids(i)
                vec(i) = mat(idx00, col_id)
            end do
        end if
    end subroutine extract_matrix2vector_16_F_r8

    subroutine extract_matrix2vector_16_F_i8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        integer(kind=8), intent(inout) :: vec(n_ids)
        integer(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in)    :: ids(n_ids)
        integer(kind=8), intent(in)    :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain
        integer(kind=8) :: idx00, idx01, idx02, idx03
        integer(kind=8) :: idx04, idx05, idx06, idx07
        integer(kind=8) :: idx08, idx09, idx10, idx11
        integer(kind=8) :: idx12, idx13, idx14, idx15

        unroll_size = 16
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            idx00 = ids(i)
            idx01 = ids(i+1)
            idx02 = ids(i+2)
            idx03 = ids(i+3)
            idx04 = ids(i+4)
            idx05 = ids(i+5)
            idx06 = ids(i+6)
            idx07 = ids(i+7)
            idx08 = ids(i+8)
            idx09 = ids(i+9)
            idx10 = ids(i+10)
            idx11 = ids(i+11)
            idx12 = ids(i+12)
            idx13 = ids(i+13)
            idx14 = ids(i+14)
            idx15 = ids(i+15)
            vec(i)    = mat(idx00, col_id)
            vec(i+1)  = mat(idx01, col_id)
            vec(i+2)  = mat(idx02, col_id)
            vec(i+3)  = mat(idx03, col_id)
            vec(i+4)  = mat(idx04, col_id)
            vec(i+5)  = mat(idx05, col_id)
            vec(i+6)  = mat(idx06, col_id)
            vec(i+7)  = mat(idx07, col_id)
            vec(i+8)  = mat(idx08, col_id)
            vec(i+9)  = mat(idx09, col_id)
            vec(i+10) = mat(idx10, col_id)
            vec(i+11) = mat(idx11, col_id)
            vec(i+12) = mat(idx12, col_id)
            vec(i+13) = mat(idx13, col_id)
            vec(i+14) = mat(idx14, col_id)
            vec(i+15) = mat(idx15, col_id)
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                idx00  = ids(i)
                vec(i) = mat(idx00, col_id)
            end do
        end if
    end subroutine extract_matrix2vector_16_F_i8

    subroutine extract_matrix2vector_buffer_08_F_r8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        real(kind=8), intent(inout) :: vec(n_ids)
        real(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain
        integer(kind=8) :: idx00, idx01, idx02, idx03
        integer(kind=8) :: idx04, idx05, idx06, idx07
        real(kind=8)    :: buf00, buf01, buf02, buf03
        real(kind=8)    :: buf04, buf05, buf06, buf07

        unroll_size = 8
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            idx00 = ids(i)
            idx01 = ids(i+1)
            idx02 = ids(i+2)
            idx03 = ids(i+3)
            idx04 = ids(i+4)
            idx05 = ids(i+5)
            idx06 = ids(i+6)
            idx07 = ids(i+7)
            buf00 = mat(idx00, col_id)
            buf01 = mat(idx01, col_id)
            buf02 = mat(idx02, col_id)
            buf03 = mat(idx03, col_id)
            buf04 = mat(idx04, col_id)
            buf05 = mat(idx05, col_id)
            buf06 = mat(idx06, col_id)
            buf07 = mat(idx07, col_id)
            vec(i)   = buf00
            vec(i+1) = buf01
            vec(i+2) = buf02
            vec(i+3) = buf03
            vec(i+4) = buf04
            vec(i+5) = buf05
            vec(i+6) = buf06
            vec(i+7) = buf07
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                idx00  = ids(i)
                vec(i) = mat(idx00, col_id)
            end do
        end if
    end subroutine extract_matrix2vector_buffer_08_F_r8

    subroutine extract_matrix2vector_buffer_08_F_i8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        integer(kind=8), intent(inout) :: vec(n_ids)
        integer(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in)    :: ids(n_ids)
        integer(kind=8), intent(in)    :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain
        integer(kind=8) :: idx00, idx01, idx02, idx03
        integer(kind=8) :: idx04, idx05, idx06, idx07
        integer(kind=8) :: buf00, buf01, buf02, buf03
        integer(kind=8) :: buf04, buf05, buf06, buf07

        unroll_size = 8
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            idx00 = ids(i)
            idx01 = ids(i+1)
            idx02 = ids(i+2)
            idx03 = ids(i+3)
            idx04 = ids(i+4)
            idx05 = ids(i+5)
            idx06 = ids(i+6)
            idx07 = ids(i+7)
            buf00 = mat(idx00, col_id)
            buf01 = mat(idx01, col_id)
            buf02 = mat(idx02, col_id)
            buf03 = mat(idx03, col_id)
            buf04 = mat(idx04, col_id)
            buf05 = mat(idx05, col_id)
            buf06 = mat(idx06, col_id)
            buf07 = mat(idx07, col_id)
            vec(i)   = buf00
            vec(i+1) = buf01
            vec(i+2) = buf02
            vec(i+3) = buf03
            vec(i+4) = buf04
            vec(i+5) = buf05
            vec(i+6) = buf06
            vec(i+7) = buf07
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                idx00  = ids(i)
                vec(i) = mat(idx00, col_id)
            end do
        end if
    end subroutine extract_matrix2vector_buffer_08_F_i8

    subroutine extract_matrix2vector_direct_08_F_r8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        real(kind=8), intent(inout) :: vec(n_ids)
        real(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain

        unroll_size = 8
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            vec(i)   = mat(ids(i), col_id)
            vec(i+1) = mat(ids(i+1), col_id)
            vec(i+2) = mat(ids(i+2), col_id)
            vec(i+3) = mat(ids(i+3), col_id)
            vec(i+4) = mat(ids(i+4), col_id)
            vec(i+5) = mat(ids(i+5), col_id)
            vec(i+6) = mat(ids(i+6), col_id)
            vec(i+7) = mat(ids(i+7), col_id)
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                vec(i) = mat(ids(i), col_id)
            end do
        end if
    end subroutine extract_matrix2vector_direct_08_F_r8

    subroutine extract_matrix2vector_direct_08_F_i8(vec, mat, ids, r_mat, c_mat, n_ids, col_id)
        implicit none
        integer(kind=8), intent(inout) :: vec(n_ids)
        integer(kind=8), intent(in)    :: mat(r_mat, c_mat)
        integer(kind=8), intent(in)    :: ids(n_ids)
        integer(kind=8), intent(in)    :: r_mat, c_mat, n_ids, col_id

        integer(kind=8) :: unroll_size
        integer(kind=8) :: i, i_unroll, i_remain

        unroll_size = 8
        i_remain = mod(n_ids, unroll_size)
        i_unroll = n_ids - i_remain

        do i=1, i_unroll, unroll_size
            vec(i)   = mat(ids(i), col_id)
            vec(i+1) = mat(ids(i+1), col_id)
            vec(i+2) = mat(ids(i+2), col_id)
            vec(i+3) = mat(ids(i+3), col_id)
            vec(i+4) = mat(ids(i+4), col_id)
            vec(i+5) = mat(ids(i+5), col_id)
            vec(i+6) = mat(ids(i+6), col_id)
            vec(i+7) = mat(ids(i+7), col_id)
        end do

        if (i_remain>0) then
            do i=i_unroll+1, n_ids, 1
                vec(i) = mat(ids(i), col_id)
            end do
        end if
    end subroutine extract_matrix2vector_direct_08_F_i8

end program extract_m2v
