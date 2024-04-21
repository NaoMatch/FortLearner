module mod_intrinsics
    use mod_variable
    use mod_addition
    use mod_multiply
    use mod_power
    use mod_substraction
    use mod_absolute_value
    use mod_sinusoidal
    use mod_cosinusoidal
    use mod_tangent
    use mod_tangent_hyperbolic
    use mod_tangent
    use mod_summation
    use mod_reshaping
    use mod_transposing
    use mod_spreading
    use mod_matrix_multiplication
    use mod_get_item
    implicit none
    
contains

    function len_(var)
        implicit none
        type(variable) :: var
        integer(kind=8) :: len_

        len_ = size(vstack(var%id)%v, dim=1)
    end function len_

    function shape_(var)
        implicit none
        type(variable) :: var
        integer(kind=8) :: shape_(2)

        shape_ = shape(vstack(var%id)%v)
    end function shape_

    subroutine matmul_shape_checker(x_shape1, y_shape2)
        implicit none
        integer(kind=8) :: x_shape1(2), y_shape2(2)

    end subroutine 

    
end module mod_intrinsics