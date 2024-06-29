module mod_new_get_matrix_minmax
    use iso_c_binding
    implicit none
    

    Interface
        subroutine new_get_matrix_minmax(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows, n_jobs) & 
            Bind(C,Name='new_get_matrix_minmax_ver05')
            Import
            integer(c_int64_t), value      :: n_indices
            integer(c_int64_t), value      :: n_cols
            integer(c_int64_t), value      :: n_rows
            integer(c_int64_t), value      :: n_jobs
            real(c_double), intent(inout)  :: min_vals(n_cols)
            real(c_double), intent(inout)  :: max_vals(n_cols)
            real(c_double), intent(in)     :: mat_t(n_cols, n_rows)
            integer(c_int64_t), intent(in) :: indices(n_indices)
        end subroutine new_get_matrix_minmax

        subroutine new_get_matrix_minmax_ver00(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='new_get_matrix_minmax_ver00')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine new_get_matrix_minmax_ver00

        subroutine new_get_matrix_minmax_ver01(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='new_get_matrix_minmax_ver01')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine new_get_matrix_minmax_ver01

        subroutine new_get_matrix_minmax_ver02(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='new_get_matrix_minmax_ver02')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine new_get_matrix_minmax_ver02

        subroutine new_get_matrix_minmax_ver03(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='new_get_matrix_minmax_ver03')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine new_get_matrix_minmax_ver03

        subroutine new_get_matrix_minmax_ver04(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='new_get_matrix_minmax_ver04')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine new_get_matrix_minmax_ver04

        subroutine new_get_matrix_minmax_ver05(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows, n_jobs) & 
            Bind(C,Name='new_get_matrix_minmax_ver05')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows, n_jobs
        end subroutine new_get_matrix_minmax_ver05

        subroutine new_get_matrix_minmax_ver06(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='new_get_matrix_minmax_ver06')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine new_get_matrix_minmax_ver06

        subroutine new_get_matrix_minmax_ver07(min_vals, max_vals, mat_t, indices, n_indices, n_cols, n_rows) & 
            Bind(C,Name='new_get_matrix_minmax_ver07')
            Import
            type(c_ptr), value        :: min_vals, max_vals, mat_t, indices
            integer(c_int64_t), value :: n_indices, n_cols, n_rows
        end subroutine new_get_matrix_minmax_ver07
    end interface

contains
    
end module mod_new_get_matrix_minmax