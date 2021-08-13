program main_linalg_ax_plus_y
    use mod_timer
    use mod_linalg, only: ax_plus_y
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    integer(kind=8)           :: n_samples, vec_size(5), i
    real(kind=8) :: a
    real(kind=8), allocatable :: x(:)
    real(kind=8), allocatable :: y(:), y_orig(:)


    vec_size = (/100, 1000, 10000, 100000, 1000000/)

    print*, "           n_samples                 diff"
    do i=1, 5, 1
        n_samples = vec_size(i)
        allocate(x(n_samples))
        allocate(y(n_samples))
        allocate(y_orig(n_samples))
        call random_number(a)
        call random_number(x)
        call random_number(y_orig)
        y = y_orig

        call ax_plus_y(a, x, y, n_samples)
        print*, n_samples, sum(y-(a*x+y_orig))

        deallocate(x, y, y_orig)
    end do


end program main_linalg_ax_plus_y
