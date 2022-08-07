program main_multi_layer_perceptron
    use mod_var
    use mod_activation_functions2
    implicit none

    type(variable) :: var_x, var_a, var_b, var_c, var_d, grad
    real(kind=8) :: x(1,1), g(1,1)
    type(activation_function) :: act1, act2, act3
    integer(kind=8) :: iter, max_iter

    max_iter = 20
    x = .5d0
    g = 1d0
    
    x = .5d0
    max_iter = 100
    do iter=1, max_iter, 1
        var_x = variable(x)
        var_a = act1%square(var_x)
        var_b = act2%exponential(var_a)
        var_c = act3%square(var_b)
        
        var_c%g = g
        call var_c%backward()
        x = var_x%v - 0.1d0 * var_x%g
    end do
    print*, x


end program main_multi_layer_perceptron