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
