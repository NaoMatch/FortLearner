module mod_matrix_sum_up_gt_with_index
    use iso_c_binding
    implicit none

    interface 
        subroutine get_matrix_sum_up_gt_with_index_C(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_C')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_C

        subroutine get_matrix_sum_up_gt_with_index_C_02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_C_02')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_C_02

        subroutine get_matrix_sum_up_gt_with_index_C_04(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_C_04')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_C_04

        subroutine get_matrix_sum_up_gt_with_index_C_08(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_C_08')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_C_08



        subroutine get_matrix_sum_up_gt_with_index_A_02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_A_02')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_A_02

        subroutine get_matrix_sum_up_gt_with_index_A_04(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_A_04')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_A_04

        subroutine get_matrix_sum_up_gt_with_index_A_04x(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_A_04x')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_A_04x

        subroutine get_matrix_sum_up_gt_with_index_A_08(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_A_08')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_A_08

        subroutine get_matrix_sum_up_gt_with_index_A_08z(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_A_08z')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_A_08z

        subroutine get_matrix_sum_up_gt_with_index_A_16(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_A_16')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_A_16

        subroutine get_matrix_sum_up_gt_with_index_A_16z(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols) &
            Bind(C,Name='get_matrix_sum_up_gt_with_index_A_16z')
            Import
            type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
            integer(c_int64_t), value :: n_idxs, n_cols, n_rows
        end subroutine get_matrix_sum_up_gt_with_index_A_16z
    end interface 
    
contains
    subroutine get_indices_diff(indices_diff, indices, n_idxs)
        integer(kind=8), intent(inout) :: indices_diff(n_idxs)
        integer(kind=8), intent(in)    :: indices(n_idxs)
        integer(kind=8), intent(in)    :: n_idxs

        integer(kind=8) :: n_idx
        integer(kind=8) :: idx, start_idx
        integer(kind=8) :: idx_unroll_size
        integer(kind=8) :: n_idx_unroll, n_idx_remain

        n_idx = n_idxs-1
        idx_unroll_size=1
        n_idx_remain = mod(n_idxs, idx_unroll_size)
        n_idx_unroll = n_idxs - n_idx_remain

        start_idx=1
        indices_diff(1) = indices(1)-start_idx

        do idx=2, n_idx_unroll, 1
            indices_diff(idx) = indices(idx)-indices(idx-1)
        end do
    end subroutine get_indices_diff

    subroutine get_matrix_sum_up_gt_with_index(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout)    :: sum_vals(n_cols)
        integer(kind=8), intent(inout) :: cnt_vals(n_cols)
        real(kind=8), intent(inout)    :: thr_vals(n_cols)
        real(kind=8), intent(in)       :: mat_t(n_cols, n_rows)
        real(kind=8), intent(in)       :: y(n_rows)
        integer(kind=8), intent(in)    :: indices(n_idxs)
        integer(kind=8), intent(in)    :: n_idxs, n_rows, n_cols

        integer(kind=8) :: r, c, idx
        real(kind=8)    :: r00, r03
        integer(kind=8) :: r01, r02

        sum_vals = 0d0
        cnt_vals = 0_8

        do r=1, n_idxs, 1
            idx = indices(r)
            r03 = y(idx)
            do c=1, n_cols, 1
                r00 = sum_vals(c)
                r01 = cnt_vals(c)

                r02 = mat_t(c,idx)>thr_vals(c)

                r00 = r00 + r03 * r02
                r01 = r01 + r02

                sum_vals(c) = r00
                cnt_vals(c) = r01
            end do
        end do
    end subroutine get_matrix_sum_up_gt_with_index

    subroutine get_matrix_sum_up_gt_with_index_02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout)    :: sum_vals(n_cols)
        integer(kind=8), intent(inout) :: cnt_vals(n_cols)
        real(kind=8), intent(inout)    :: thr_vals(n_cols)
        real(kind=8), intent(in)       :: mat_t(n_cols, n_rows)
        real(kind=8), intent(in)       :: y(n_rows)
        integer(kind=8), intent(in)    :: indices(n_idxs)
        integer(kind=8), intent(in)    :: n_idxs, n_rows, n_cols

        integer(kind=8) :: n_cols_unroll_size, n_cols_unroll, n_cols_remain
        integer(kind=8) :: r, c, idx
        real(kind=8)    :: r00, r02, r15
        integer(kind=8) :: r01, r03, r04, r05

        sum_vals = 0d0
        cnt_vals = 0_8

        n_cols_unroll_size = 2
        n_cols_remain = mod(n_cols, n_cols_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do r=1, n_idxs, 1
            idx = indices(r)
            r15 = y(idx)
            do c=1, n_cols_unroll, n_cols_unroll_size
                r00 = sum_vals(c)
                r01 = cnt_vals(c)
                r02 = sum_vals(c+1)
                r03 = cnt_vals(c+1)

                r04 = mat_t(c,idx)  >thr_vals(c)
                r05 = mat_t(c+1,idx)>thr_vals(c+1)

                r00 = r00 + r15 * r04
                r01 = r01 + r04
                r02 = r02 + r15 * r05
                r03 = r03 + r05

                sum_vals(c)   = r00
                cnt_vals(c)   = r01
                sum_vals(c+1) = r02
                cnt_vals(c+1) = r03
            end do

            do c=n_cols_unroll+1, n_cols, 1
                r00 = sum_vals(c)
                r01 = cnt_vals(c)

                r04 = mat_t(c,idx)>thr_vals(c)

                r00 = r00 + r15 * r04
                r01 = r01 + r04

                sum_vals(c) = r00
                cnt_vals(c) = r01
            end do
        end do
    end subroutine get_matrix_sum_up_gt_with_index_02

    subroutine get_matrix_sum_up_gt_with_index_04(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout)    :: sum_vals(n_cols)
        integer(kind=8), intent(inout) :: cnt_vals(n_cols)
        real(kind=8), intent(inout)    :: thr_vals(n_cols)
        real(kind=8), intent(in)       :: mat_t(n_cols, n_rows)
        real(kind=8), intent(in)       :: y(n_rows)
        integer(kind=8), intent(in)    :: indices(n_idxs)
        integer(kind=8), intent(in)    :: n_idxs, n_rows, n_cols

        integer(kind=8) :: n_cols_unroll_size, n_cols_unroll, n_cols_remain
        integer(kind=8) :: r, c, idx
        real(kind=8)    :: r00, r02, r04, r06, r15
        integer(kind=8) :: r01, r03, r05, r07
        integer(kind=8) :: r08, r09, r10, r11

        sum_vals = 0d0
        cnt_vals = 0_8

        n_cols_unroll_size = 4
        n_cols_remain = mod(n_cols, n_cols_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do r=1, n_idxs, 1
            idx = indices(r)
            r15 = y(idx)
            do c=1, n_cols_unroll, n_cols_unroll_size
                r00 = sum_vals(c)
                r01 = cnt_vals(c)
                r02 = sum_vals(c+1)
                r03 = cnt_vals(c+1)
                r04 = sum_vals(c+2)
                r05 = cnt_vals(c+2)
                r06 = sum_vals(c+3)
                r07 = cnt_vals(c+3)

                r08 = mat_t(c,idx)  >thr_vals(c)
                r09 = mat_t(c+1,idx)>thr_vals(c+1)
                r10 = mat_t(c+2,idx)>thr_vals(c+2)
                r11 = mat_t(c+3,idx)>thr_vals(c+3)

                r00 = r00 + r15 * r08
                r01 = r01 + r08
                r02 = r02 + r15 * r09
                r03 = r03 + r09

                r04 = r04 + r15 * r10
                r05 = r05 + r10
                r06 = r06 + r15 * r11
                r07 = r07 + r11

                sum_vals(c)   = r00
                cnt_vals(c)   = r01
                sum_vals(c+1) = r02
                cnt_vals(c+1) = r03
                sum_vals(c+2) = r04
                cnt_vals(c+2) = r05
                sum_vals(c+3) = r06
                cnt_vals(c+3) = r07
            end do

            do c=n_cols_unroll+1, n_cols, 1
                r00 = sum_vals(c)
                r01 = cnt_vals(c)

                r08 = mat_t(c,idx)>thr_vals(c)

                r00 = r00 + r15 * r08
                r01 = r01 + r08

                sum_vals(c) = r00
                cnt_vals(c) = r01
            end do
        end do
    end subroutine get_matrix_sum_up_gt_with_index_04

    subroutine get_matrix_sum_up_gt_with_index_08(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_idxs, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout)    :: sum_vals(n_cols)
        integer(kind=8), intent(inout) :: cnt_vals(n_cols)
        real(kind=8), intent(inout)    :: thr_vals(n_cols)
        real(kind=8), intent(in)       :: mat_t(n_cols, n_rows)
        real(kind=8), intent(in)       :: y(n_rows)
        integer(kind=8), intent(in)    :: indices(n_idxs)
        integer(kind=8), intent(in)    :: n_idxs, n_rows, n_cols

        integer(kind=8) :: n_cols_unroll_size, n_cols_unroll, n_cols_remain
        integer(kind=8) :: r, c, idx
        real(kind=8)    :: r00, r02, r04, r06, r15
        integer(kind=8) :: r01, r03, r05, r07
        integer(kind=8) :: r08, r09, r10, r11

        sum_vals = 0d0
        cnt_vals = 0_8

        n_cols_unroll_size = 8
        n_cols_remain = mod(n_cols, n_cols_unroll_size)
        n_cols_unroll = n_cols - n_cols_remain

        do r=1, n_idxs, 1
            idx = indices(r)
            r15 = y(idx)
            do c=1, n_cols_unroll, n_cols_unroll_size
                r00 = sum_vals(c)
                r01 = cnt_vals(c)
                r02 = sum_vals(c+1)
                r03 = cnt_vals(c+1)
                r04 = sum_vals(c+2)
                r05 = cnt_vals(c+2)
                r06 = sum_vals(c+3)
                r07 = cnt_vals(c+3)

                r08 = mat_t(c,idx)  >thr_vals(c)
                r09 = mat_t(c+1,idx)>thr_vals(c+1)
                r10 = mat_t(c+2,idx)>thr_vals(c+2)
                r11 = mat_t(c+3,idx)>thr_vals(c+3)

                r00 = r00 + r15 * r08
                r01 = r01 + r08
                r02 = r02 + r15 * r09
                r03 = r03 + r09

                r04 = r04 + r15 * r10
                r05 = r05 + r10
                r06 = r06 + r15 * r11
                r07 = r07 + r11

                sum_vals(c)   = r00
                cnt_vals(c)   = r01
                sum_vals(c+1) = r02
                cnt_vals(c+1) = r03
                sum_vals(c+2) = r04
                cnt_vals(c+2) = r05
                sum_vals(c+3) = r06
                cnt_vals(c+3) = r07

                r00 = sum_vals(c+4)
                r01 = cnt_vals(c+4)
                r02 = sum_vals(c+5)
                r03 = cnt_vals(c+5)
                r04 = sum_vals(c+6)
                r05 = cnt_vals(c+6)
                r06 = sum_vals(c+7)
                r07 = cnt_vals(c+7)

                r08 = mat_t(c+4,idx)>thr_vals(c+4)
                r09 = mat_t(c+5,idx)>thr_vals(c+5)
                r10 = mat_t(c+6,idx)>thr_vals(c+6)
                r11 = mat_t(c+7,idx)>thr_vals(c+7)

                r00 = r00 + r15 * r08
                r01 = r01 + r08
                r02 = r02 + r15 * r09
                r03 = r03 + r09

                r04 = r04 + r15 * r10
                r05 = r05 + r10
                r06 = r06 + r15 * r11
                r07 = r07 + r11

                sum_vals(c+4) = r00
                cnt_vals(c+4) = r01
                sum_vals(c+5) = r02
                cnt_vals(c+5) = r03
                sum_vals(c+6) = r04
                cnt_vals(c+6) = r05
                sum_vals(c+7) = r06
                cnt_vals(c+7) = r07
            end do

            do c=n_cols_unroll+1, n_cols, 1
                r00 = sum_vals(c)
                r01 = cnt_vals(c)

                r08 = mat_t(c,idx)>thr_vals(c)

                r00 = r00 + r15 * r08
                r01 = r01 + r08

                sum_vals(c) = r00
                cnt_vals(c) = r01
            end do
        end do
    end subroutine get_matrix_sum_up_gt_with_index_08



end module mod_matrix_sum_up_gt_with_index
