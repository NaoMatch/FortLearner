program main_multi_layer_perceptron
    use mod_var
    use mod_func
    implicit none

    type(func) :: f
    type(square) :: sq
    type(exponential) :: ex
    type(sub) :: sb
    type(variable) :: var_x, var_m
    real(kind=8) :: x
    real(kind=8), allocatable :: v(:), vm(:,:)
    real(kind=8), allocatable :: m(:,:)

    x = .5d0
    var_x = variable(x)

    var_x = sq%act(var_x)
    print*, var_x%v
    var_x = ex%act(var_x)
    print*, var_x%v
    var_x = sq%act(var_x)
    print*, var_x%v


    var_x = sb%act(var_x)

end program main_multi_layer_perceptron