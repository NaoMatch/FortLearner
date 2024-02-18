module prefixsum
    use iso_c_binding
    implicit none

    interface
        subroutine prefix_sum(a, b, n) bind(C, name='prefix_sum')
            import
            integer(c_int64_t), value :: n
            real(c_double), intent(in) :: a(n), b(n)
        end subroutine prefix_sum

        subroutine prefix_sum_simd(a, b, n) bind(C, name='prefix_sum_simd')
            import
            integer(c_int64_t), value :: n
            real(c_double), intent(in) :: a(n), b(n)
        end subroutine prefix_sum_simd

        subroutine prefix_sum_omp_simd(a, b, n) bind(C, name='prefix_sum_omp_simd')
            import
            integer(c_int64_t), value :: n
            real(c_double), intent(in) :: a(n), b(n)
        end subroutine prefix_sum_omp_simd
    end interface


end module prefixsum