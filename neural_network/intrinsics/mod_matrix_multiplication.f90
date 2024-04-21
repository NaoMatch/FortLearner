module mod_matrix_multiplication
    use mod_variable
    implicit none

    type, extends(base_function) :: matrix_multiplication
    contains
        procedure :: forward_2in_1out => forward_matrix_multiplication
        procedure :: backward_1in_2out => backward_matrix_multiplication
    end type matrix_multiplication
    
    interface matrix_multiplication
        module procedure new_matrix_multiplication
    end interface matrix_multiplication


contains

    function matmul_(var_in_1, var_in_2) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in_1, var_in_2
        type(variable) :: var_out
        type(matrix_multiplication) :: mat_mul_
        mat_mul_ = matrix_multiplication()
        var_out = mat_mul_%act(var_in_1, var_in_2)
    end function matmul_

    function new_matrix_multiplication()
        implicit none
        type(matrix_multiplication) :: new_matrix_multiplication
        new_matrix_multiplication%fname = "matrix_multiplication"
        new_matrix_multiplication%n_in = 2
        new_matrix_multiplication%n_out = 1
    end function new_matrix_multiplication

    function forward_matrix_multiplication(this, v_in_1, v_in_2) result(v_out)
        implicit none
        class(matrix_multiplication) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable :: v_out(:,:)

        integer(kind=8) :: shape1(2), shape2(2)

        shape1 = shape(v_in_1)
        shape2 = shape(v_in_2)
        if (shape1(2) /= shape2(1)) goto 990
        v_out = matmul(v_in_1, v_in_2)

        return
        990 continue 
        print*, "Shape ", shape1, " ::: ", shape2
        stop "matmul_ shape mismatch error."
    end function forward_matrix_multiplication

    function backward_matrix_multiplication(this, g_in) result(g_outs)
        implicit none
        class(matrix_multiplication) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: shape1(2), shape2(2)

        g_outs(1)%g = matmul(g_in, transpose(vstack(this%id_in_2)%v))
        g_outs(2)%g = matmul(transpose(vstack(this%id_in_1)%v), g_in)
    end function backward_matrix_multiplication

end module mod_matrix_multiplication