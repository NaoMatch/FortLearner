program autodiff
    use mod_autodiff
    implicit none

    type(variable) :: x1, x2
    type(variable) :: y
    integer(kind=8) :: i

    x1 = variable(-3000d0)
    X2 = variable(4000d0)
    do i=1, 10000, 1
        ! y = x1*x1*x1*x1 - 20.d0 * x1*x1 + 20.d0 * x1
        y = 2d0*x1*x1 - 2.d0 * x1*x2 + 2d0*x2*x2
        print*, i, x1%get_val(), x2%get_val(), y%get_val()
        call y%backward()
        x1 = x1 - x1%get_grad() * 0.005
        x2 = x2 - x2%get_grad() * 0.005
    end do





end program autodiff
