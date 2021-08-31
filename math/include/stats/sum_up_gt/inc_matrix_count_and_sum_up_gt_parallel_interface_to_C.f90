interface 
    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_01_C(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
        n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_01_C')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_01_C

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_02_C(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
        n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_02_C')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_02_C

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_04_C(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
        n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_04_C')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_04_C

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_08_C(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
        n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_08_C')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_08_C

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_16_C(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
        n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_16_C')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_16_C


    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_02x_A(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_02x_A')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_02x_A

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_04x_A(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_04x_A')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_04x_A

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_04y_A(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_04y_A')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_04y_A

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_08y_A(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_08y_A')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_08y_A

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_08z_A(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_08z_A')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_08z_A

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_16y_A(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_16y_A')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_16y_A

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_16z_A(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_16z_A')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_16z_A

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_32y_A(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_32y_A')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_32y_A

    subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_32z_A(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, &
		n_idxs, n_rows, n_cols, n_thds) &
        Bind(C,Name='get_matrix_count_and_sum_up_gt_with_index_parallel_32z_A')
        Import
        type(c_ptr), value :: sum_vals, cnt_vals, thr_vals, mat_t, y, indices
        integer(c_int64_t), value :: n_idxs, n_rows, n_cols, n_thds
    end subroutine get_matrix_count_and_sum_up_gt_with_index_parallel_32z_A

end interface 
