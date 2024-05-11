module mod_sparse_dgemv
    use iso_c_binding
    implicit none

    interface
        subroutine sparse_mv_ver0(vec, cols, vals, b_t, n, m, k, l) bind(C, name="sparse_mv_ver0")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: l
            real(c_double), intent(inout) :: vec(l)
            integer(c_int64_t), intent(in) :: cols(n)
            real(c_double), intent(in) :: vals(n), b_t(l,k)
        end subroutine sparse_mv_ver0                   

        subroutine sparse_dgemv_ver0(c, rows, cols, vals, b_t, n, m, k, n_jobs) bind(C, name="sparse_dgemv_ver0")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: n_jobs
            real(c_double), intent(inout) :: c(1,m)
            integer(c_int64_t), intent(in) :: rows(m+1), cols(n)
            real(c_double), intent(in) :: vals(n), b_t(1,k)
        end subroutine sparse_dgemv_ver0                   

        subroutine sparse_dgemv_ver1(c, rows, cols, vals, b_t, n, m, k, n_jobs) bind(C, name="sparse_dgemv_ver1")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: n_jobs
            real(c_double), intent(inout) :: c(1,m)
            integer(c_int64_t), intent(in) :: rows(m+1), cols(n)
            real(c_double), intent(in) :: vals(n), b_t(1,k)
        end subroutine sparse_dgemv_ver1                   

        subroutine sparse_dgemv_ver2(c, rows, cols, vals, b_t, n, m, k, n_jobs) bind(C, name="sparse_dgemv_ver2")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: n_jobs
            real(c_double), intent(inout) :: c(1,m)
            integer(c_int64_t), intent(in) :: rows(m+1), cols(n)
            real(c_double), intent(in) :: vals(n), b_t(1,k)
        end subroutine sparse_dgemv_ver2                   

        subroutine sparse_dgemv_ver3(c, rows, cols, vals, b_t, n, m, k, n_jobs) bind(C, name="sparse_dgemv_ver3")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: n_jobs
            real(c_double), intent(inout) :: c(1,m)
            integer(c_int64_t), intent(in) :: rows(m+1), cols(n)
            real(c_double), intent(in) :: vals(n), b_t(1,k)
        end subroutine sparse_dgemv_ver3                   

        subroutine sparse_dgemv(c, rows, cols, vals, b_t, n, m, k, n_jobs) bind(C, name="sparse_dgemv")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: n_jobs
            real(c_double), intent(inout) :: c(1,m)
            integer(c_int64_t), intent(in) :: rows(m+1), cols(n)
            real(c_double), intent(in) :: vals(n), b_t(1,k)
        end subroutine sparse_dgemv                   

        subroutine sparse_dgemv_ver5(c, rows, cols, vals, b_t, n, m, k, n_jobs) bind(C, name="sparse_dgemv_ver5")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: n_jobs
            real(c_double), intent(inout) :: c(1,m)
            integer(c_int64_t), intent(in) :: rows(m+1), cols(n)
            real(c_double), intent(in) :: vals(n), b_t(1,k)
        end subroutine sparse_dgemv_ver5                   

        subroutine sparse_dgemv_ver6(c, rows, cols, vals, b_t, n, m, k, n_jobs) bind(C, name="sparse_dgemv_ver6")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: n_jobs
            real(c_double), intent(inout) :: c(1,m)
            integer(c_int64_t), intent(in) :: rows(m+1), cols(n)
            real(c_double), intent(in) :: vals(n), b_t(1,k)
        end subroutine sparse_dgemv_ver6                   
    end interface 

contains
    
end module mod_sparse_dgemv
