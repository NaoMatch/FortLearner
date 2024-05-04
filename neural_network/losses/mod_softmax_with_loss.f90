module mod_softmax_with_loss
    use mod_const
    use mod_variable
    use mod_softmax
    implicit none

    type, extends(base_function) :: softmax_with_loss
    contains
        procedure :: forward_2in_1out => forward_softmax_with_loss
        procedure :: backward_1in_2out => backward_softmax_with_loss
    end type softmax_with_loss
    
    interface softmax_with_loss
        module procedure new_softmax_with_loss
    end interface softmax_with_loss

contains


    function softmax_with_loss_(var_true, var_pred, n_classes, reduction) result(var_out)
        implicit none
        type(variable), intent(in) :: var_true, var_pred
        type(variable) :: var_out
        integer(kind=8), intent(in) :: n_classes
        character(len=*), optional :: reduction
        character(len=:), allocatable :: reduction_opt

        type(softmax_with_loss) :: s_max_loss

        reduction_opt = "mean"
        if (present(reduction)) reduction_opt = reduction

        s_max_loss = softmax_with_loss(n_classes, reduction=reduction_opt)

        var_out = s_max_loss%act(var_true, var_pred)
    end function softmax_with_loss_


    function new_softmax_with_loss(n_classes, reduction) result(new_obj)
        implicit none
        type(softmax_with_loss) :: new_obj
        integer(kind=8) :: n_classes
        character(len=*), optional :: reduction
        new_obj%fname = "softmax_with_loss"
        new_obj%n_in = 2
        new_obj%n_out = 1
        if (present(reduction)) new_obj%reduction = reduction
    end function new_softmax_with_loss

    function softmax_val(x) result(y)
        implicit none
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), allocatable :: y(:,:)
        real(kind=8), allocatable :: x_max(:), x_shift(:,:), exp_sum(:,:)

        integer(kind=8) :: n_rows, n_cols

        n_rows = size(x, dim=1)
        n_cols = size(x, dim=2)

        x_max = maxval(x, dim=2)

        x_shift = x - spread(x_max, dim=2, ncopies=n_cols)

        allocate(exp_sum(n_rows, n_cols))
        exp_sum = exp(x_shift)
        y = exp_sum / spread(sum(exp_sum, dim=2), dim=2, ncopies=n_cols)
    end function softmax_val


    function forward_softmax_with_loss(this, v_in_1, v_in_2) result(v_out)
        implicit none
        class(softmax_with_loss) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable :: v_out(:,:)

        integer(kind=8) :: n_rows, n_cols, c
        real(kind=8), allocatable :: soft_max_val(:,:)

        n_rows = size(v_in_1, dim=1)
        n_cols = size(v_in_1, dim=2)

        ! Softmax
        soft_max_val = softmax_val(v_in_2)

        ! Loss
        allocate(v_out(1, 1))
        v_out = 0d0
        do c=1, n_cols, 1
            v_out(1,1) = v_out(1,1) - sum(v_in_1(:,c) * log(soft_max_val(:,c) + epsilon_for_log))
        end do

        if (this%reduction == "mean") then
            v_out = v_out / dble(n_rows)
        end if
    end function forward_softmax_with_loss

    function backward_softmax_with_loss(this, g_in) result(g_outs)
        implicit none
        class(softmax_with_loss) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: n_rows, n_cols
        real(kind=8), allocatable :: sumdx(:,:), out_spread(:,:)

        n_rows = size(vstack(this%id_in_1)%v, dim=1)
        n_cols = size(vstack(this%id_in_1)%v, dim=2)

        allocate(out_spread, source=vstack(this%id_in_1)%v)

        g_outs(2)%g = (softmax_val(vstack(this%id_in_2)%v) - out_spread) * g_in(1,1)

        if (this%reduction == "mean") then
            g_outs(2)%g = g_outs(2)%g / dble(n_rows)
        end if
    end function backward_softmax_with_loss

end module mod_softmax_with_loss