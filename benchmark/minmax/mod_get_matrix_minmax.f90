module mod_get_matrix_minmax
    !$ use omp_lib
    use iso_c_binding
    implicit none

    Interface
        subroutine get_matrix_minmax_loop_C(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_C')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_C

        subroutine get_matrix_minmax_loop_02_C(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_02_C')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_02_C

        subroutine get_matrix_minmax_loop_04_C(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_04_C')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_04_C

        subroutine get_matrix_minmax_loop_08_C(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_08_C')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_08_C

        subroutine get_matrix_minmax_loop_02_A(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_02_A')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_02_A

        subroutine get_matrix_minmax_loop_04_A(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_04_A')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_04_A

        subroutine get_matrix_minmax_loop_04x_A(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_04x_A')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_04x_A

        subroutine get_matrix_minmax_loop_08_A(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_08_A')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_08_A

        subroutine get_matrix_minmax_loop_08z_A(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_08z_A')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_08z_A

        subroutine get_matrix_minmax_loop_16_A(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_16_A')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_16_A

        subroutine get_matrix_minmax_loop_16z_A(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='get_matrix_minmax_loop_16z_A')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine get_matrix_minmax_loop_16z_A
    end interface


contains

    subroutine get_indices_diff(indices_diff, indices, n_indices)
        integer(kind=8), intent(inout) :: indices_diff(n_indices)
        integer(kind=8), intent(in)    :: indices(n_indices)
        integer(kind=8), intent(in)    :: n_indices

        integer(kind=8) :: n_idx
        integer(kind=8) :: idx, start_idx
        integer(kind=8) :: idx_unroll_size
        integer(kind=8) :: n_idx_unroll, n_idx_remain

        n_idx = n_indices-1
        idx_unroll_size=1
        n_idx_remain = mod(n_indices, idx_unroll_size)
        n_idx_unroll = n_indices - n_idx_remain

        start_idx=1
        indices_diff(1) = indices(1)-start_idx

        do idx=2, n_idx_unroll, 1
            indices_diff(idx) = indices(idx)-indices(idx-1)
        end do
    end subroutine get_indices_diff

    subroutine get_matrix_minmax_col_major_loop_with_index(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)    :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in) :: indices(n_indices)
        integer(kind=8), intent(in) :: n_indices, n_rows, n_cols
        integer(kind=8)             :: r, c, idx
        real(kind=8)                :: r00, r01, r02

        min_vals = huge(min_vals(1))
        max_vals = - huge(max_vals(1))

        do r=1, n_indices, 1
            idx = indices(r)
            do c=1, n_cols, 1
                r00 = min_vals(c)
                r01 = max_vals(c)

                r02 = mat_t(c,idx)

                r00 = minval((/r00, r02/))
                r01 = maxval((/r01, r02/))

                min_vals(c) = r00
                max_vals(c) = r01
            end do
        end do
    end subroutine get_matrix_minmax_col_major_loop_with_index

    subroutine get_matrix_minmax_col_major_loop_with_index_02(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)    :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in) :: indices(n_indices)
        integer(kind=8), intent(in) :: n_indices, n_rows, n_cols
        integer(kind=8)             :: r, c, unroll_size, n_cols_unroll, n_cols_remain, idx
        real(kind=8)                :: r00, r01, r02
        real(kind=8)                :: r03, r04, r05

        min_vals = huge(min_vals(1))
        max_vals = - huge(max_vals(1))

        unroll_size = 2
        n_cols_remain = mod(n_cols, unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do r=1, n_indices, 1
            idx = indices(r)
            do c=1, n_cols_unroll, unroll_size
                ! =============================================================
                r00 = min_vals(c)
                r01 = max_vals(c)
                r02 = mat_t(c,idx)

                r00 = minval((/r00, r02/))
                r01 = maxval((/r01, r02/))

                ! =============================================================
                r03 = min_vals(c+1)
                r04 = max_vals(c+1)
                r05 = mat_t(c+1,idx)

                r03 = minval((/r03, r05/))
                r04 = maxval((/r04, r05/))

                min_vals(c) = r00
                max_vals(c) = r01

                min_vals(c+1) = r03
                max_vals(c+1) = r04
            end do

            do c=n_cols_unroll+1, n_cols, 1
                ! =============================================================
                r00 = min_vals(c)
                r01 = max_vals(c)
                r02 = mat_t(c,idx)

                r00 = minval((/r00, r02/))
                r01 = maxval((/r01, r02/))

                min_vals(c) = r00
                max_vals(c) = r01
            end do
        end do
    end subroutine get_matrix_minmax_col_major_loop_with_index_02

    subroutine get_matrix_minmax_col_major_loop_with_index_04(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)    :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in) :: indices(n_indices)
        integer(kind=8), intent(in) :: n_indices, n_rows, n_cols
        integer(kind=8)             :: r, c, unroll_size, n_cols_unroll, n_cols_remain, idx
        real(kind=8)                :: r00, r01, r02
        real(kind=8)                :: r03, r04, r05
        real(kind=8)                :: r06, r07, r08
        real(kind=8)                :: r09, r10, r11

        min_vals = huge(min_vals(1))
        max_vals = - huge(max_vals(1))

        unroll_size = 4
        n_cols_remain = mod(n_cols, unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do r=1, n_indices, 1
            idx = indices(r)
            do c=1, n_cols_unroll, unroll_size
                ! =============================================================
                r00 = min_vals(c)
                r01 = max_vals(c)
                r02 = mat_t(c,idx)

                r00 = minval((/r00, r02/))
                r01 = maxval((/r01, r02/))

                ! =============================================================
                r03 = min_vals(c+1)
                r04 = max_vals(c+1)
                r05 = mat_t(c+1,idx)

                r03 = minval((/r03, r05/))
                r04 = maxval((/r04, r05/))

                ! =============================================================
                r06 = min_vals(c+2)
                r07 = max_vals(c+2)
                r08 = mat_t(c+2,idx)

                r06 = minval((/r06, r08/))
                r07 = maxval((/r07, r08/))

                ! =============================================================
                r09 = min_vals(c+3)
                r10 = max_vals(c+3)
                r11 = mat_t(c+3,idx)

                r09 = minval((/r09, r11/))
                r10 = maxval((/r10, r11/))

                ! =============================================================
                min_vals(c) = r00
                max_vals(c) = r01

                min_vals(c+1) = r03
                max_vals(c+1) = r04

                min_vals(c+2) = r06
                max_vals(c+2) = r07

                min_vals(c+3) = r09
                max_vals(c+3) = r10
            end do

            do c=n_cols_unroll+1, n_cols, 1
                ! =============================================================
                r00 = min_vals(c)
                r01 = max_vals(c)
                r02 = mat_t(c,idx)

                r00 = minval((/r00, r02/))
                r01 = maxval((/r01, r02/))

                min_vals(c) = r00
                max_vals(c) = r01
            end do
        end do
    end subroutine get_matrix_minmax_col_major_loop_with_index_04

    subroutine get_matrix_minmax_col_major_loop_with_index_08(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout) :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)    :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in) :: indices(n_indices)
        integer(kind=8), intent(in) :: n_indices, n_rows, n_cols
        integer(kind=8)             :: r, c, unroll_size, n_cols_unroll, n_cols_remain, idx
        real(kind=8)                :: r00, r01, r02
        real(kind=8)                :: r03, r04, r05
        real(kind=8)                :: r06, r07, r08
        real(kind=8)                :: r09, r10, r11
        real(kind=8)                :: r12, r13, r14

        min_vals = huge(min_vals(1))
        max_vals = - huge(max_vals(1))

        unroll_size = 8
        n_cols_remain = mod(n_cols, unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do r=1, n_indices, 1
            idx = indices(r)
            do c=1, n_cols_unroll, unroll_size
                ! =============================================================
                r00 = min_vals(c)
                r01 = max_vals(c)
                r02 = mat_t(c,idx)

                r00 = minval((/r00, r02/))
                r01 = maxval((/r01, r02/))

                ! =============================================================
                r03 = min_vals(c+1)
                r04 = max_vals(c+1)
                r05 = mat_t(c+1,idx)

                r03 = minval((/r03, r05/))
                r04 = maxval((/r04, r05/))

                ! =============================================================
                r06 = min_vals(c+2)
                r07 = max_vals(c+2)
                r08 = mat_t(c+2,idx)

                r06 = minval((/r06, r08/))
                r07 = maxval((/r07, r08/))

                ! =============================================================
                r09 = min_vals(c+3)
                r10 = max_vals(c+3)
                r11 = mat_t(c+3,idx)

                r09 = minval((/r09, r11/))
                r10 = maxval((/r10, r11/))


                ! =============================================================
                min_vals(c) = r00
                max_vals(c) = r01

                min_vals(c+1) = r03
                max_vals(c+1) = r04

                min_vals(c+2) = r06
                max_vals(c+2) = r07

                min_vals(c+3) = r09
                max_vals(c+3) = r10

                ! =============================================================
                r00 = min_vals(c+4)
                r01 = max_vals(c+4)
                r02 = mat_t(c+4,idx)

                r00 = minval((/r00, r02/))
                r01 = maxval((/r01, r02/))

                ! =============================================================
                r03 = min_vals(c+5)
                r04 = max_vals(c+5)
                r05 = mat_t(c+5,idx)

                r03 = minval((/r03, r05/))
                r04 = maxval((/r04, r05/))

                ! =============================================================
                r06 = min_vals(c+6)
                r07 = max_vals(c+6)
                r08 = mat_t(c+6,idx)

                r06 = minval((/r06, r08/))
                r07 = maxval((/r07, r08/))

                ! =============================================================
                r09 = min_vals(c+7)
                r10 = max_vals(c+7)
                r11 = mat_t(c+7,idx)

                r09 = minval((/r09, r11/))
                r10 = maxval((/r10, r11/))

                ! =============================================================
                min_vals(c+4) = r00
                max_vals(c+4) = r01

                min_vals(c+5) = r03
                max_vals(c+5) = r04

                min_vals(c+6) = r06
                max_vals(c+6) = r07

                min_vals(c+7) = r09
                max_vals(c+7) = r10
            end do

            do c=n_cols_unroll+1, n_cols, 1
                ! =============================================================
                r00 = min_vals(c)
                r01 = max_vals(c)
                r02 = mat_t(c,idx)

                r00 = minval((/r00, r02/))
                r01 = maxval((/r01, r02/))

                min_vals(c) = r00
                max_vals(c) = r01
            end do
        end do
    end subroutine get_matrix_minmax_col_major_loop_with_index_08

end module mod_get_matrix_minmax
