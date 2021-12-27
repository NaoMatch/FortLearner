program main_get_matrix_minmax
    !$ use omp_lib
    use mod_random
    use mod_sort
    use mod_timer
    use mod_stats
    implicit none
    
    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8)              :: st, en
    real(kind=8)              :: min_val, max_val
    real(kind=8), allocatable, target :: min_vals(:), max_vals(:)
    real(kind=8), allocatable, target :: min_vals_orig(:), max_vals_orig(:)
    real(kind=8), allocatable, target :: mat(:,:), mat_t(:,:), f(:), y(:)
    real(kind=8), allocatable, target :: thresholds(:)
    integer(kind=8), allocatable, target :: indices(:), indices_diff(:), indices_full(:)
    integer(kind=8)           :: n_rows, n_cols, idx, n_indices
    integer(kind=8)           :: i, j, iter, max_iter, div, n_types, iter_types, n_iter, n_iter_base
    type(c_ptr) :: min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, indices_diff_ptr, thresholds_ptr

    integer(kind=8), allocatable :: n_rows_test(:), n_cols_test(:), n_divs_test(:)
    real(kind=8), allocatable :: times(:)
    character(len=60), allocatable :: types(:)
    character(len=30) :: hoge
    integer(kind=8)   :: r, c, d, n_thds
    real(kind=8)   :: rate
    character(len=256) :: filename_r8

    n_rows_test = (/100, 1000, 10000, 100000, 1000000/)
    n_cols_test = (/4, 8, 16, 32, 64/)
    n_iter_base = 50000000_8
    n_iter_base = 50000000_8

    n_rows_test(size(n_rows_test):1:-1) = n_rows_test
    n_cols_test(size(n_cols_test):1:-1) = n_cols_test

    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    ! types(1)  = "call get_matrix_minmax_r8                                    :"
    types(2)  = "call get_matrix_minmax_r8_Intrinsic                          :"
    types(3)  = "call get_matrix_minmax_r8_loop                               :"
    types(4)  = "call get_matrix_minmax_r8_loop_unrollC02_unrollR01           :"
    types(5)  = "call get_matrix_minmax_r8_loop_unrollC04_unrollR01           :"
    types(6)  = "call get_matrix_minmax_r8_loop_unrollC08_unrollR01           :"

    types(7)  = "call get_matrix_minmax_r8_loop_unrollC02_unrollR02           :"
    types(8)  = "call get_matrix_minmax_r8_loop_unrollC04_unrollR02           :"
    types(9)  = "call get_matrix_minmax_r8_loop_unrollC08_unrollR02           :"

    types(10)  = "call get_matrix_minmax_r8_loop_unrollC02_unrollR04          :"
    types(11)  = "call get_matrix_minmax_r8_loop_unrollC04_unrollR04          :"
    types(12)  = "call get_matrix_minmax_r8_loop_unrollC08_unrollR04          :"

    types(13)  = "call get_matrix_minmax_r8_loop_unrollC02_unrollR08          :"
    types(14)  = "call get_matrix_minmax_r8_loop_unrollC04_unrollR08          :"
    types(15)  = "call get_matrix_minmax_r8_loop_unrollC08_unrollR08          :"

    types(16)  = "call get_matrix_minmax_r8_loop_unrollC08_unrollR01_parallel :"

    do c=1, size(n_cols_test), 1
        n_cols = n_cols_test(c)

        allocate(min_vals(n_cols), min_vals_orig(n_cols))
        allocate(max_vals(n_cols), max_vals_orig(n_cols))

        min_vals_ptr = c_loc(min_vals)
        max_vals_ptr = c_loc(max_vals)

        do r=1, size(n_rows_test), 1
            n_rows = n_rows_test(r)
            allocate(indices_full(n_rows))
            do i=1, n_rows, 1
                indices_full(i) = i
            end do
            call permutation(indices_full, n_rows)

            allocate(mat(n_rows, n_cols))
            allocate(mat_t(n_cols, n_rows))
            call random_number(mat)
            mat_t = transpose(mat)
            mat_t_ptr = c_loc(mat_t)

                write (filename_r8, '("time_get_matrix_minmax_with_index_r8_c", i4.4, "_r", i8.8, ".txt")') & 
                    n_cols, n_rows

                n_indices = n_rows
                n_iter=maxval((/n_iter_base/n_rows/n_cols, 1_8/))

                print*, '================================================================================================'
                do iter_types=1, n_types, 1
                    if (types(iter_types) .eq. hoge)  cycle
                    call date_and_time(values=date_value1)
                    min_vals = huge(0d0)
                    max_vals = -huge(0d0)
                    do iter=1, n_iter, 1
                        select case(iter_types)
                            case  (1)
                                call get_matrix_minmax_r8(min_vals, max_vals, mat_t, indices_full, n_indices, n_rows, n_cols)
                            case  (2)
                                call get_matrix_minmax_r8_Intrinsic(min_vals, max_vals, mat, n_rows, n_cols)
                            case  (3)
                                call get_matrix_minmax_r8_loop(min_vals, max_vals, mat, n_rows, n_cols)

                            case  (4)
                                call get_matrix_minmax_r8_loop_unrollC02_unrollR01(min_vals, max_vals, mat, n_rows, n_cols)
                            case  (5)
                                call get_matrix_minmax_r8_loop_unrollC04_unrollR01(min_vals, max_vals, mat, n_rows, n_cols)
                            case  (6)
                                call get_matrix_minmax_r8_loop_unrollC08_unrollR01(min_vals, max_vals, mat, n_rows, n_cols)

                            case  (7)
                                call get_matrix_minmax_r8_loop_unrollC02_unrollR02(min_vals, max_vals, mat, n_rows, n_cols)
                            case  (8)
                                call get_matrix_minmax_r8_loop_unrollC04_unrollR02(min_vals, max_vals, mat, n_rows, n_cols)
                            case  (9)
                                call get_matrix_minmax_r8_loop_unrollC08_unrollR02(min_vals, max_vals, mat, n_rows, n_cols)

                            case  (10)
                                call get_matrix_minmax_r8_loop_unrollC02_unrollR04(min_vals, max_vals, mat, n_rows, n_cols)
                            case  (11)
                                call get_matrix_minmax_r8_loop_unrollC04_unrollR04(min_vals, max_vals, mat, n_rows, n_cols)
                            case  (12)
                                call get_matrix_minmax_r8_loop_unrollC08_unrollR04(min_vals, max_vals, mat, n_rows, n_cols)

                            case  (13)
                                call get_matrix_minmax_r8_loop_unrollC02_unrollR08(min_vals, max_vals, mat, n_rows, n_cols)
                            case  (14)
                                call get_matrix_minmax_r8_loop_unrollC04_unrollR08(min_vals, max_vals, mat, n_rows, n_cols)
                            case  (15)
                                call get_matrix_minmax_r8_loop_unrollC08_unrollR08(min_vals, max_vals, mat, n_rows, n_cols)

                            case  (16)
                                call get_matrix_minmax_r8_loop_unrollC08_unrollR01_parallel(min_vals, max_vals, &
                                    mat, n_rows, n_cols)
                        end select
                    end do
                    call date_and_time(values=date_value2)
                    times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                    print "(A, i15, i15, f12.4, f30.15, f30.15)", types(iter_types), n_cols, n_rows, &
                        real(times(iter_types)), sum(min_vals-min_vals_orig), sum(max_vals-max_vals_orig)
                end do
            deallocate(indices_full)
            deallocate(mat, mat_t)
        end do
        deallocate(min_vals, min_vals_orig)
        deallocate(max_vals, max_vals_orig)
    end do

contains

    subroutine get_matrix_minmax_r8_Intrinsic(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        min_vals(:) = minval(mat, dim=1)
        max_vals(:) = maxval(mat, dim=1)
    end subroutine 

    subroutine get_matrix_minmax_r8_loop(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j
        real(kind=8) :: tmp_min, tmp_max, val

        do j=1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/val, tmp_min/))
                tmp_max = maxval((/val, tmp_max/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine

    subroutine get_matrix_minmax_r8_loop_unrollC02_unrollR01(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(2), tmp_maxs(2), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 1_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 2_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/val, tmp_min/))
                tmp_max = maxval((/val, tmp_max/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC02_unrollR01

    subroutine get_matrix_minmax_r8_loop_unrollC04_unrollR01(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(4), tmp_maxs(4), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 1_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 4_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/val, tmp_min/))
                tmp_max = maxval((/val, tmp_max/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC04_unrollR01

    subroutine get_matrix_minmax_r8_loop_unrollC08_unrollR01(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(8), tmp_maxs(8), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 1_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 8_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/val, tmp_min/))
                tmp_max = maxval((/val, tmp_max/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC08_unrollR01

    subroutine get_matrix_minmax_r8_loop_unrollC02_unrollR02(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(2), tmp_maxs(2), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 2_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 2_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    do ii=0, r_unroll_size-1, 1
                        val = mat(i+ii,j+jj)
                        tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                        tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                    end do
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows_unroll, r_unroll_size
                do ii=0, r_unroll_size-1, 1
                    val = mat(i+ii,j)
                    tmp_min = minval((/tmp_min, val/))
                    tmp_max = maxval((/tmp_max, val/))
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/tmp_min, val/))
                tmp_max = maxval((/tmp_max, val/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC02_unrollR02

    subroutine get_matrix_minmax_r8_loop_unrollC04_unrollR02(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(4), tmp_maxs(4), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 2_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 4_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    do ii=0, r_unroll_size-1, 1
                        val = mat(i+ii,j+jj)
                        tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                        tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                    end do
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows_unroll, r_unroll_size
                do ii=0, r_unroll_size-1, 1
                    val = mat(i+ii,j)
                    tmp_min = minval((/tmp_min, val/))
                    tmp_max = maxval((/tmp_max, val/))
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/tmp_min, val/))
                tmp_max = maxval((/tmp_max, val/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC04_unrollR02

    subroutine get_matrix_minmax_r8_loop_unrollC08_unrollR02(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(8), tmp_maxs(8), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 2_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 8_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    do ii=0, r_unroll_size-1, 1
                        val = mat(i+ii,j+jj)
                        tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                        tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                    end do
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows_unroll, r_unroll_size
                do ii=0, r_unroll_size-1, 1
                    val = mat(i+ii,j)
                    tmp_min = minval((/tmp_min, val/))
                    tmp_max = maxval((/tmp_max, val/))
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/tmp_min, val/))
                tmp_max = maxval((/tmp_max, val/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC08_unrollR02

    subroutine get_matrix_minmax_r8_loop_unrollC02_unrollR04(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(2), tmp_maxs(2), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 4_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 2_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    do ii=0, r_unroll_size-1, 1
                        val = mat(i+ii,j+jj)
                        tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                        tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                    end do
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows_unroll, r_unroll_size
                do ii=0, r_unroll_size-1, 1
                    val = mat(i+ii,j)
                    tmp_min = minval((/tmp_min, val/))
                    tmp_max = maxval((/tmp_max, val/))
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/tmp_min, val/))
                tmp_max = maxval((/tmp_max, val/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC02_unrollR04

    subroutine get_matrix_minmax_r8_loop_unrollC04_unrollR04(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(4), tmp_maxs(4), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 4_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 4_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    do ii=0, r_unroll_size-1, 1
                        val = mat(i+ii,j+jj)
                        tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                        tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                    end do
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows_unroll, r_unroll_size
                do ii=0, r_unroll_size-1, 1
                    val = mat(i+ii,j)
                    tmp_min = minval((/tmp_min, val/))
                    tmp_max = maxval((/tmp_max, val/))
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/tmp_min, val/))
                tmp_max = maxval((/tmp_max, val/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC04_unrollR04

    subroutine get_matrix_minmax_r8_loop_unrollC08_unrollR04(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(8), tmp_maxs(8), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 4_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 8_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    do ii=0, r_unroll_size-1, 1
                        val = mat(i+ii,j+jj)
                        tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                        tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                    end do
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows_unroll, r_unroll_size
                do ii=0, r_unroll_size-1, 1
                    val = mat(i+ii,j)
                    tmp_min = minval((/tmp_min, val/))
                    tmp_max = maxval((/tmp_max, val/))
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/tmp_min, val/))
                tmp_max = maxval((/tmp_max, val/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC08_unrollR04

    subroutine get_matrix_minmax_r8_loop_unrollC02_unrollR08(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(2), tmp_maxs(2), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 8_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 2_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    do ii=0, r_unroll_size-1, 1
                        val = mat(i+ii,j+jj)
                        tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                        tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                    end do
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows_unroll, r_unroll_size
                do ii=0, r_unroll_size-1, 1
                    val = mat(i+ii,j)
                    tmp_min = minval((/tmp_min, val/))
                    tmp_max = maxval((/tmp_max, val/))
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/tmp_min, val/))
                tmp_max = maxval((/tmp_max, val/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC02_unrollR08

    subroutine get_matrix_minmax_r8_loop_unrollC04_unrollR08(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(4), tmp_maxs(4), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 8_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 4_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    do ii=0, r_unroll_size-1, 1
                        val = mat(i+ii,j+jj)
                        tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                        tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                    end do
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows_unroll, r_unroll_size
                do ii=0, r_unroll_size-1, 1
                    val = mat(i+ii,j)
                    tmp_min = minval((/tmp_min, val/))
                    tmp_max = maxval((/tmp_max, val/))
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/tmp_min, val/))
                tmp_max = maxval((/tmp_max, val/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC04_unrollR08

    subroutine get_matrix_minmax_r8_loop_unrollC08_unrollR08(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(8), tmp_maxs(8), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 8_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 8_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    do ii=0, r_unroll_size-1, 1
                        val = mat(i+ii,j+jj)
                        tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                        tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                    end do
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows_unroll, r_unroll_size
                do ii=0, r_unroll_size-1, 1
                    val = mat(i+ii,j)
                    tmp_min = minval((/tmp_min, val/))
                    tmp_max = maxval((/tmp_max, val/))
                end do
            end do

            do i=n_rows_unroll+1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/tmp_min, val/))
                tmp_max = maxval((/tmp_max, val/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC08_unrollR08


    subroutine get_matrix_minmax_r8_loop_unrollC08_unrollR01_parallel(min_vals, max_vals, mat, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols)
        real(kind=8), intent(inout) :: max_vals(n_cols)
        real(kind=8), intent(in)    :: mat(n_rows, n_cols)
        integer(kind=8), intent(in) :: n_rows, n_cols

        integer(kind=8) :: i, j, jj, ii
        integer(kind=8) :: r_unroll_size, c_unroll_size
        integer(kind=8) :: n_rows_unroll, n_rows_remain
        integer(kind=8) :: n_cols_unroll, n_cols_remain
        real(kind=8) :: tmp_mins(8), tmp_maxs(8), val
        real(kind=8) :: tmp_min, tmp_max

        r_unroll_size = 1_8
        n_rows_remain = mod(n_rows, r_unroll_size)
        n_rows_unroll = n_rows - n_rows_remain

        c_unroll_size = 8_8
        n_cols_remain = mod(n_cols, c_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        !$omp parallel num_threads(4)
        !$omp do reduction(min: min_vals) reduction(max: max_vals)  private(i,j,ii,jj,val,tmp_mins,tmp_maxs)
        do j=1, n_cols_unroll, c_unroll_size
            tmp_mins(:) =   huge(0d0)
            tmp_maxs(:) = - huge(0d0)
            do i=1, n_rows_unroll, r_unroll_size
                do jj=0, c_unroll_size-1, 1
                    val = mat(i,j+jj)
                    tmp_mins(jj+1) = minval((/tmp_mins(jj+1), val/))
                    tmp_maxs(jj+1) = maxval((/tmp_maxs(jj+1), val/))
                end do
            end do

            do jj=0, c_unroll_size-1, 1
                min_vals(j+jj) = tmp_mins(jj+1)
                max_vals(j+jj) = tmp_maxs(jj+1)
            end do
        end do
        !$omp end do
        !$omp end parallel

        do j=n_cols_unroll+1, n_cols, 1
            tmp_min =   huge(tmp_min)
            tmp_max = - huge(tmp_min)
            do i=1, n_rows, 1
                val = mat(i,j)
                tmp_min = minval((/val, tmp_min/))
                tmp_max = maxval((/val, tmp_max/))
            end do
            min_vals(j) = tmp_min
            max_vals(j) = tmp_max
        end do
    end subroutine get_matrix_minmax_r8_loop_unrollC08_unrollR01_parallel

end program main_get_matrix_minmax
