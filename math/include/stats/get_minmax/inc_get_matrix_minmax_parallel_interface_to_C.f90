interface
    subroutine get_matrix_minmax_with_index_parallel_01_C_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_01_C_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_01_C_r8

    subroutine get_matrix_minmax_with_index_parallel_02_C_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_02_C_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_02_C_r8

    subroutine get_matrix_minmax_with_index_parallel_04_C_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_04_C_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_04_C_r8

    subroutine get_matrix_minmax_with_index_parallel_08_C_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_08_C_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_08_C_r8

    subroutine get_matrix_minmax_with_index_parallel_16_C_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_16_C_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_16_C_r8

    subroutine get_matrix_minmax_with_index_parallel_02x_A_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_02x_A_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_02x_A_r8

    subroutine get_matrix_minmax_with_index_parallel_04x_A_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_04x_A_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_04x_A_r8

    subroutine get_matrix_minmax_with_index_parallel_04y_A_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_04y_A_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_04y_A_r8

    subroutine get_matrix_minmax_with_index_parallel_08y_A_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_08y_A_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_08y_A_r8

    subroutine get_matrix_minmax_with_index_parallel_08z_A_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_08z_A_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_08z_A_r8

    subroutine get_matrix_minmax_with_index_parallel_16y_A_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_16y_A_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_16y_A_r8

    subroutine get_matrix_minmax_with_index_parallel_16z_A_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_16z_A_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_16z_A_r8

    subroutine get_matrix_minmax_with_index_parallel_32z_A_r8(min_vals, max_vals, mat_t, indices, & 
        n_indices, n_cols, n_rows, n_thds) & 
        Bind(C,Name='get_matrix_minmax_with_index_parallel_32z_A_r8')
        Import
        type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
        integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_thds
    end subroutine get_matrix_minmax_with_index_parallel_32z_A_r8

end interface