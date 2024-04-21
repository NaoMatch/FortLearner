module mod_simple_residual_block
    use mod_variable
    use mod_intrinsics
    use mod_functions
    use mod_linear
    implicit none
    
    type, extends(base_layer) :: simple_residual_block
        type(linear) :: l1
        type(linear) :: l2
    contains
        procedure :: forward => forward_simple_residual_block
    end type simple_residual_block

    interface simple_residual_block
        module procedure new_simple_residual_block
    end interface simple_residual_block

contains

    function new_simple_residual_block(hidden_size, out_size, no_bias, in_size) result(res_block)
        implicit none
        type(simple_residual_block) :: res_block
        integer(kind=8), intent(in) :: hidden_size
        integer(kind=8), intent(in) :: out_size
        logical(kind=4), intent(in) :: no_bias
        integer(kind=8), optional :: in_size
        real(kind=8), allocatable :: w(:,:), b(:,:)

        res_block%l1 = linear(out_size=hidden_size, no_bias=no_bias)
        res_block%l2 = linear(out_size=out_size,    no_bias=no_bias)
    end function new_simple_residual_block

    function forward_simple_residual_block(this, var_in) result(var_out)
        implicit none
        class(simple_residual_block) :: this
        type(variable) :: var_in
        type(variable) :: var_out

        var_out = this%l1%act(var_in)
        var_out = relu_(var_out)
        var_out = this%l2%act(var_out) + var_in
    end function forward_simple_residual_block

end module mod_simple_residual_block