module mod_sparse_dgemm
    use iso_c_binding
    implicit none

    interface
        subroutine sparse_dgemm_ver0(c, rows, cols, vals, b, n, m, k, l) bind(C, name="sparse_dgemm_ver0")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: l
            real(c_double), intent(inout) :: c(l,m)
            integer(c_int64_t), intent(in) :: rows(m), cols(n)
            real(c_double), intent(in) :: vals(n), b(l,k)
        end subroutine sparse_dgemm_ver0            

        subroutine sparse_dgemm(c, rows, cols, vals, b, n, m, k, l, n_jobs) bind(C, name="sparse_dgemm")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: l
            integer(c_int64_t), Value     :: n_jobs
            real(c_double), intent(inout) :: c(l,m)
            integer(c_int64_t), intent(in) :: rows(m), cols(n)
            real(c_double), intent(in) :: vals(n), b(l,k)
        end subroutine sparse_dgemm            

        subroutine sparse_dgemm_ver2(c, rows, cols, vals, b, n, m, k, l, n_jobs) bind(C, name="sparse_dgemm_ver2")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: l
            integer(c_int64_t), Value     :: n_jobs
            real(c_double), intent(inout) :: c(l,m)
            integer(c_int64_t), intent(in) :: rows(m), cols(n)
            real(c_double), intent(in) :: vals(n), b(l,k)
        end subroutine sparse_dgemm_ver2            

        subroutine sparse_dgemm_ver3(c, rows, cols, vals, counter, b, n, m, k, l, n_jobs) bind(C, name="sparse_dgemm_ver3")
            Import  
            integer(c_int64_t), Value     :: n
            integer(c_int64_t), Value     :: m
            integer(c_int64_t), Value     :: k
            integer(c_int64_t), Value     :: l
            integer(c_int64_t), Value     :: n_jobs
            real(c_double), intent(inout) :: c(l,m)
            integer(c_int64_t), intent(in) :: rows(m), cols(n), counter(m)
            real(c_double), intent(in) :: vals(n), b(l,k)
        end subroutine sparse_dgemm_ver3            
    end interface 

contains
    
end module mod_sparse_dgemm
