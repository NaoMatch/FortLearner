interface 
    subroutine get_matrix_count_and_sum_up_gt_with_index_C(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
        n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_C')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_C

    subroutine get_matrix_count_and_sum_up_gt_with_index_C_02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_C_02')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_C_02

    subroutine get_matrix_count_and_sum_up_gt_with_index_C_04(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_C_04')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_C_04

    subroutine get_matrix_count_and_sum_up_gt_with_index_C_08(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_C_08')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_C_08



    subroutine get_matrix_count_and_sum_up_gt_with_index_A_02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_02')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_02

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_04(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_04')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_04

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_04x(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_04x')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_04x

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_08(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_08')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_08

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_16(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_16')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_16

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_02_ver02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
        n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_02_ver02')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_02_ver02

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_04_ver02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
        n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_04_ver02')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_04_ver02

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_08_ver02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
        n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_08_ver02')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_08_ver02

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_16_ver02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
        n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_16_ver02')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_16_ver02

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_04_02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
        n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_04_02')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_04_02

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_04_04(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
        n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_04_04')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_04_04

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_08_02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
        n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_08_02')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_08_02

    subroutine get_matrix_count_and_sum_up_gt_with_index_A_08_04(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
        n_idxs, n_rows, n_cols) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_A_08_04')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_cols, n_rows
    end subroutine get_matrix_count_and_sum_up_gt_with_index_A_08_04
end interface 
