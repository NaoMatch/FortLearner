module mod_sigmoid_with_loss
    use mod_const
    use mod_variable
    use mod_softmax
    implicit none

    type, extends(base_function) :: sigmoid_with_loss
    contains
        procedure :: forward_2in_1out => forward_sigmoid_with_loss
        procedure :: backward_1in_2out => backward_sigmoid_with_loss
    end type sigmoid_with_loss
    
    interface sigmoid_with_loss
        module procedure new_sigmoid_with_loss
    end interface sigmoid_with_loss

contains


    function sigmoid_with_loss_(var_true, var_pred, n_classes, reduction) result(var_out)
        implicit none
        type(variable), intent(in) :: var_true, var_pred
        type(variable) :: var_out
        integer(kind=8), intent(in) :: n_classes
        character(len=*), optional :: reduction
        character(len=:), allocatable :: reduction_opt

        type(sigmoid_with_loss) :: s_max_loss

        reduction_opt = "mean"
        if (present(reduction)) reduction_opt = reduction

        s_max_loss = sigmoid_with_loss(n_classes, reduction=reduction_opt)

        var_out = s_max_loss%act(var_true, var_pred)
    end function sigmoid_with_loss_


    function new_sigmoid_with_loss(n_classes, reduction) result(new_obj)
        implicit none
        type(sigmoid_with_loss) :: new_obj
        integer(kind=8) :: n_classes
        character(len=*), optional :: reduction
        new_obj%fname = "sigmoid_with_loss"
        new_obj%n_in = 2
        new_obj%n_out = 1
        if (present(reduction)) new_obj%reduction = reduction
    end function new_sigmoid_with_loss

    function sigmoid_val(x) result(y)
        implicit none
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), allocatable :: y(:,:)
        y = 1d0 / (1d0 + exp(-x))
    end function sigmoid_val


    subroutine forward_sigmoid_with_loss(this, v_out, v_in_1, v_in_2)
        implicit none
        class(sigmoid_with_loss) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)

        integer(kind=8) :: n_rows, n_cols, c
        real(kind=8), allocatable :: soft_max_val(:,:)

        n_rows = size(v_in_1, dim=1)
        n_cols = size(v_in_1, dim=2)

        ! Sigmoid
        soft_max_val = sigmoid_val(v_in_2)

        ! Loss
        allocate(v_out(1, 1))
        v_out = 0d0
        v_out(1,1) = v_out(1,1) - sum(v_in_1(:,1) * log(soft_max_val(:,1) + epsilon_for_log))

        if (this%reduction == "mean") then
            v_out = v_out / dble(n_rows)
        end if
    end subroutine forward_sigmoid_with_loss

    function backward_sigmoid_with_loss(this, g_in) result(g_outs)
        implicit none
        class(sigmoid_with_loss) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: n_rows, n_cols
        real(kind=8), allocatable :: sumdx(:,:), out_spread(:,:)

        n_rows = size(vstack(this%id_in_1)%v, dim=1)
        n_cols = size(vstack(this%id_in_1)%v, dim=2)

        allocate(out_spread(n_rows, n_cols))
        out_spread = vstack(this%id_out_1)%v(1,1)

        g_outs(2)%g = -(out_spread - sigmoid_val(vstack(this%id_in_2)%v)) * g_in(1,1)

        if (this%reduction == "mean") then
            g_outs(2)%g = g_outs(2)%g / dble(n_rows)
        end if
    end function backward_sigmoid_with_loss

end module mod_sigmoid_with_loss