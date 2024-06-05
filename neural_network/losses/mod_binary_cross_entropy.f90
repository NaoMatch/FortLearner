module mod_binary_cross_entropy
    use mod_const
    use mod_common
    use mod_variable
    implicit none
    
    type, extends(base_function) :: binary_cross_entropy
    contains
        procedure :: forward_2in_1out => forward_binary_cross_entropy
        procedure :: backward_1in_2out => backward_binary_cross_entropy
    end type binary_cross_entropy
    
    interface binary_cross_entropy
        module procedure new_binary_cross_entropy
    end interface binary_cross_entropy

contains

    function binary_cross_entropy_(var_true, var_pred, reduction) result(var_out)
        implicit none
        type(variable), intent(in) :: var_true, var_pred
        character(len=*), optional :: reduction
        type(variable) :: var_out
        type(binary_cross_entropy) :: loss_
        character(len=:), allocatable :: reduction_opt
        reduction_opt = "mean"
        if (present(reduction)) reduction_opt = reduction
        loss_ = binary_cross_entropy(reduction=reduction_opt)
        var_out = loss_%act(var_true, var_pred)
    end function binary_cross_entropy_

    function new_binary_cross_entropy(reduction)
        implicit none
        type(binary_cross_entropy) :: new_binary_cross_entropy
        character(len=*), optional :: reduction
        new_binary_cross_entropy%fname = "binary_cross_entropy"
        new_binary_cross_entropy%n_in = 2
        new_binary_cross_entropy%n_out = 1
        if (present(reduction)) new_binary_cross_entropy%reduction = reduction
    end function new_binary_cross_entropy

    subroutine forward_binary_cross_entropy(this, v_out, v_in_1, v_in_2)
        implicit none
        class(binary_cross_entropy) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)

        integer(kind=8) :: n_rows

        if (size(v_in_1, dim=2) /= 1 .or. size(v_in_2, dim=2) /= 1) &
            stop "OutputDim must be '1' at " // num2char(__LINE__) // " in " // __FILE__ // "."
        allocate(v_out(1,1))
        v_out = sum(- v_in_1 * log(v_in_2 + epsilon_for_log) - (1-v_in_1) * log(1-v_in_2 + epsilon_for_log))
        if (this%reduction == "mean") then
            n_rows = size(v_in_2, dim=1)
            v_out = v_out / dble(n_rows)
        end if
    end subroutine forward_binary_cross_entropy

    function backward_binary_cross_entropy(this, g_in) result(g_outs)
        implicit none
        class(binary_cross_entropy) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: shape1(2), shape2(2)

        integer(kind=8) :: id_in_1, id_in_2, n_rows

        id_in_1 = this%id_in_1
        id_in_2 = this%id_in_2
        n_rows = size(vstack(id_in_2)%v, dim=1)
        allocate(g_outs(2)%g, source=vstack(id_in_2)%v)

        g_outs(2)%g = - ( &
            vstack(id_in_1)%v / (vstack(id_in_2)%v + epsilon_for_log) &
            + (1-vstack(id_in_1)%v) / (1-vstack(id_in_2)%v + epsilon_for_log) &
            ) &
            * spread(g_in(1,:), dim=1, ncopies=n_rows)
        if (this%reduction == "mean") then
            n_rows = size(vstack(id_in_2)%v, dim=1)
            g_outs(2)%g = g_outs(2)%g / dble(n_rows)
        end if            
    end function backward_binary_cross_entropy

end module mod_binary_cross_entropy