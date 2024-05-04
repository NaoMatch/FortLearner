module mod_binary_cross_entropy_with_logits
    use mod_const
    use mod_common
    use mod_variable
    use mod_math
    implicit none
    
    type, extends(base_function) :: binary_cross_entropy_with_logits
    contains
        procedure :: forward_2in_1out => forward_binary_cross_entropy_with_logits
        procedure :: backward_1in_2out => backward_binary_cross_entropy_with_logits
    end type binary_cross_entropy_with_logits
    
    interface binary_cross_entropy_with_logits
        module procedure new_binary_cross_entropy_with_logits
    end interface binary_cross_entropy_with_logits

contains

    function binary_cross_entropy_with_logits_(var_true, var_pred) result(var_out)
        implicit none
        type(variable), intent(in) :: var_true, var_pred
        type(variable) :: var_out
        type(binary_cross_entropy_with_logits) :: loss_
        loss_ = binary_cross_entropy_with_logits()
        var_out = loss_%act(var_true, var_pred)
    end function binary_cross_entropy_with_logits_

    function new_binary_cross_entropy_with_logits()
        implicit none
        type(binary_cross_entropy_with_logits) :: new_binary_cross_entropy_with_logits
        new_binary_cross_entropy_with_logits%fname = "binary_cross_entropy_with_logits"
        new_binary_cross_entropy_with_logits%n_in = 2
        new_binary_cross_entropy_with_logits%n_out = 1
    end function new_binary_cross_entropy_with_logits

    function forward_binary_cross_entropy_with_logits(this, v_in_1, v_in_2) result(v_out)
        implicit none
        class(binary_cross_entropy_with_logits) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable :: v_out(:,:)

        if (size(v_in_1, dim=2) /= 1 .or. size(v_in_2, dim=2) /= 1) &
            stop "OutputDim must be '1' at " // num2char(__LINE__) // " in " // __FILE__ // "."
        v_out = - v_in_1 * log(sigmoid(v_in_2) + epsilon_for_log) - (1-v_in_1) * log(1-sigmoid(v_in_2) + epsilon_for_log)
    end function forward_binary_cross_entropy_with_logits

    function backward_binary_cross_entropy_with_logits(this, g_in) result(g_outs)
        implicit none
        class(binary_cross_entropy_with_logits) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: shape1(2), shape2(2)

        integer(kind=8) :: id_in_1, id_in_2

        id_in_1 = this%id_in_1
        id_in_2 = this%id_in_2

        g_outs(1)%g = 
    end function backward_binary_cross_entropy_with_logits

end module mod_binary_cross_entropy_wit_logitsh