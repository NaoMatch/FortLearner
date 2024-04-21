module mod_multiply
    use mod_variable
    implicit none

    type, extends(base_function) :: multiply
    contains
        procedure :: forward_2in_1out => forward_multiply
        procedure :: backward_1in_2out => backward_multiply
    end type multiply
    
    interface multiply
        module procedure new_multiply
    end interface multiply

    interface operator(*)
        module procedure multiply_var_var
        module procedure multiply_r8_var
        module procedure multiply_mat_r8_var
        module procedure multiply_var_r8
        module procedure multiply_var_mat_r8
    end interface operator(*)

contains

    function multiply_var_var(var_in_1, var_in_2) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in_1, var_in_2
        type(variable) :: var_out
        type(multiply) :: mul
        mul = multiply()
        var_out = mul%act(var_in_1, var_in_2)
    end function multiply_var_var    

    function multiply_r8_var(scl, var_in) result(var_out)
        implicit none
        real(kind=8), intent(in) :: scl
        type(variable), intent(in) :: var_in
        type(variable) :: var_out
        type(variable) :: var_scl
        type(multiply) :: mul
        mul = multiply()
        var_scl = variable(scl)
        var_out = mul%act(var_scl, var_in)
    end function multiply_r8_var    

    function multiply_mat_r8_var(mat, var_in) result(var_out)
        implicit none
        real(kind=8), intent(in) :: mat(:,:)
        type(variable), intent(in) :: var_in
        type(variable) :: var_out
        type(variable) :: var_scl
        type(multiply) :: mul
        mul = multiply()
        var_scl = variable(mat)
        var_out = mul%act(var_scl, var_in)
    end function multiply_mat_r8_var    

    function multiply_var_r8(var_in, scl) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in
        real(kind=8), intent(in) :: scl
        type(variable) :: var_out
        type(variable) :: var_scl
        type(multiply) :: mul
        mul = multiply()
        var_scl = variable(scl)
        var_out = mul%act(var_in, var_scl)
    end function multiply_var_r8    

    function multiply_var_mat_r8(var_in, mat) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in
        real(kind=8), intent(in) :: mat(:,:)
        type(variable) :: var_out
        type(variable) :: var_mat
        type(multiply) :: mul
        mul = multiply()
        var_mat = variable(mat)
        var_out = mul%act(var_in, var_mat)
    end function multiply_var_mat_r8    

    function new_multiply()
        implicit none
        type(multiply) :: new_multiply
        new_multiply%fname = "multiply"
        new_multiply%n_in = 2
        new_multiply%n_out = 1
    end function new_multiply

    function forward_multiply(this, v_in_1, v_in_2) result(v_out)
        implicit none
        class(multiply) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable :: v_out(:,:)
        v_out = v_in_1 * v_in_2
    end function forward_multiply

    function backward_multiply(this, g_in) result(g_outs)
        implicit none
        class(multiply) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = vstack(this%id_in_2)%v * g_in
        g_outs(2)%g = vstack(this%id_in_1)%v * g_in
    end function backward_multiply

end module mod_multiply