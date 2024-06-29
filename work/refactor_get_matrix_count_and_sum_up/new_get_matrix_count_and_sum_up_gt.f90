module mod_new_get_matrix_count_and_sum_up_gt
    use iso_c_binding
    implicit none
    Interface
        subroutine new_get_matrix_count_and_sum_up_gt(&
                sum_vals_r, cnt_vals_r, thr_vals, &
                mat_t, y, indices, &
                n_indices, n_rows, n_cols, n_jobs) & 
            Bind(C,Name='new_get_matrix_count_and_sum_up_gt')
            Import
            integer(c_int64_t), value          :: n_indices
            integer(c_int64_t), value          :: n_rows
            integer(c_int64_t), value          :: n_cols
            integer(c_int64_t), value          :: n_jobs
            real(c_double), intent(inout)      :: sum_vals_r(n_cols)
            integer(c_int64_t), intent(inout)  :: cnt_vals_r(n_cols)
            real(c_double), intent(inout)      :: thr_vals(n_cols)
            real(c_double), intent(in)         :: mat_t(n_cols, n_rows)
            real(c_double), intent(in)         :: y(n_rows)
            integer(c_int64_t), intent(in)     :: indices(n_indices)
        end subroutine new_get_matrix_count_and_sum_up_gt

        subroutine new_get_matrix_count_and_sum_up_gt_ver01(&
                sum_vals_r, cnt_vals_r, thr_vals, &
                mat_t, y, indices, &
                n_indices, n_rows, n_cols, n_jobs) & 
            Bind(C,Name='new_get_matrix_count_and_sum_up_gt_ver01')
            Import
            integer(c_int64_t), value          :: n_indices
            integer(c_int64_t), value          :: n_rows
            integer(c_int64_t), value          :: n_cols
            integer(c_int64_t), value          :: n_jobs
            real(c_double), intent(inout)      :: sum_vals_r(n_cols)
            integer(c_int64_t), intent(inout)  :: cnt_vals_r(n_cols)
            real(c_double), intent(inout)      :: thr_vals(n_cols)
            real(c_double), intent(in)         :: mat_t(n_cols, n_rows)
            real(c_double), intent(in)         :: y(n_rows)
            integer(c_int64_t), intent(in)     :: indices(n_indices)
        end subroutine new_get_matrix_count_and_sum_up_gt_ver01

        subroutine new_get_matrix_count_and_sum_up_gt_ver02(&
                sum_vals_r, cnt_vals_r, thr_vals, &
                mat_t, y, indices, &
                n_indices, n_rows, n_cols, n_jobs) & 
            Bind(C,Name='new_get_matrix_count_and_sum_up_gt_ver02')
            Import
            integer(c_int64_t), value          :: n_indices
            integer(c_int64_t), value          :: n_rows
            integer(c_int64_t), value          :: n_cols
            integer(c_int64_t), value          :: n_jobs
            real(c_double), intent(inout)      :: sum_vals_r(n_cols)
            integer(c_int64_t), intent(inout)  :: cnt_vals_r(n_cols)
            real(c_double), intent(inout)      :: thr_vals(n_cols)
            real(c_double), intent(in)         :: mat_t(n_cols, n_rows)
            real(c_double), intent(in)         :: y(n_rows)
            integer(c_int64_t), intent(in)     :: indices(n_indices)
        end subroutine new_get_matrix_count_and_sum_up_gt_ver02

        subroutine new_get_matrix_count_and_sum_up_gt_ver03(&
                sum_vals_r, cnt_vals_r, thr_vals, &
                mat_t, y, indices, &
                n_indices, n_rows, n_cols, n_jobs) & 
            Bind(C,Name='new_get_matrix_count_and_sum_up_gt_ver03')
            Import
            integer(c_int64_t), value          :: n_indices
            integer(c_int64_t), value          :: n_rows
            integer(c_int64_t), value          :: n_cols
            integer(c_int64_t), value          :: n_jobs
            real(c_double), intent(inout)      :: sum_vals_r(n_cols)
            integer(c_int64_t), intent(inout)  :: cnt_vals_r(n_cols)
            real(c_double), intent(inout)      :: thr_vals(n_cols)
            real(c_double), intent(in)         :: mat_t(n_cols, n_rows)
            real(c_double), intent(in)         :: y(n_rows)
            integer(c_int64_t), intent(in)     :: indices(n_indices)
        end subroutine new_get_matrix_count_and_sum_up_gt_ver03

        subroutine new_get_matrix_count_and_sum_up_gt_ver04(&
                sum_vals_r, cnt_vals_r, thr_vals, &
                mat_t, y, indices, &
                n_indices, n_rows, n_cols, n_jobs) & 
            Bind(C,Name='new_get_matrix_count_and_sum_up_gt_ver04')
            Import
            integer(c_int64_t), value          :: n_indices
            integer(c_int64_t), value          :: n_rows
            integer(c_int64_t), value          :: n_cols
            integer(c_int64_t), value          :: n_jobs
            real(c_double), intent(inout)      :: sum_vals_r(n_cols)
            integer(c_int64_t), intent(inout)  :: cnt_vals_r(n_cols)
            real(c_double), intent(inout)      :: thr_vals(n_cols)
            real(c_double), intent(in)         :: mat_t(n_cols, n_rows)
            real(c_double), intent(in)         :: y(n_rows)
            integer(c_int64_t), intent(in)     :: indices(n_indices)
        end subroutine new_get_matrix_count_and_sum_up_gt_ver04

        subroutine new_get_matrix_count_and_sum_up_gt_ver05(&
                sum_vals_r, cnt_vals_r, thr_vals, &
                mat_t, y, indices, &
                n_indices, n_rows, n_cols, n_jobs) & 
            Bind(C,Name='new_get_matrix_count_and_sum_up_gt_ver05')
            Import
            integer(c_int64_t), value          :: n_indices
            integer(c_int64_t), value          :: n_rows
            integer(c_int64_t), value          :: n_cols
            integer(c_int64_t), value          :: n_jobs
            real(c_double), intent(inout)      :: sum_vals_r(n_cols)
            integer(c_int64_t), intent(inout)  :: cnt_vals_r(n_cols)
            real(c_double), intent(inout)      :: thr_vals(n_cols)
            real(c_double), intent(in)         :: mat_t(n_cols, n_rows)
            real(c_double), intent(in)         :: y(n_rows)
            integer(c_int64_t), intent(in)     :: indices(n_indices)
        end subroutine new_get_matrix_count_and_sum_up_gt_ver05
    end Interface
    
contains
    
end module mod_new_get_matrix_count_and_sum_up_gt