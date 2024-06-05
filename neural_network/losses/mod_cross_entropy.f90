module mod_cross_entropy
    use mod_const
    use mod_common
    use mod_variable
    implicit none
    
    type, extends(base_function) :: cross_entropy
    contains
        procedure :: forward_2in_1out => forward_cross_entropy
        procedure :: backward_1in_2out => backward_cross_entropy
    end type cross_entropy
    
    interface cross_entropy
        module procedure new_cross_entropy
    end interface cross_entropy

contains

    function cross_entropy_(var_true, var_pred, n_classes, reduction) result(var_out)
        implicit none
        type(variable), intent(in) :: var_true, var_pred
        integer(kind=8), intent(in) :: n_classes
        character(len=*), optional :: reduction
        type(variable) :: var_out
        type(cross_entropy) :: loss_
        character(len=:), allocatable :: reduction_opt
        reduction_opt = "mean"
        if (present(reduction)) reduction_opt = reduction
        loss_ = cross_entropy(n_classes, reduction=reduction_opt)
        var_out = loss_%act(var_true, var_pred)
    end function cross_entropy_

    function new_cross_entropy(n_classes, reduction)
        implicit none
        type(cross_entropy) :: new_cross_entropy
        integer(kind=8) :: n_classes
        character(len=*), optional :: reduction
        new_cross_entropy%fname = "cross_entropy"
        new_cross_entropy%n_in = 2
        new_cross_entropy%n_out = 1
        new_cross_entropy%n_classes = n_classes
        if (present(reduction)) new_cross_entropy%reduction = reduction
    end function new_cross_entropy

    subroutine forward_cross_entropy(this, v_out, v_in_1, v_in_2)
        implicit none
        class(cross_entropy) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)

        integer(kind=8) :: n_rows, n

        if (size(v_in_1, dim=2) /= this%n_classes .or. size(v_in_2, dim=2) /= this%n_classes) &
            stop "OutputDim must be '1' at " // num2char(__LINE__) // " in " // __FILE__ // "."
        allocate(v_out(1,1))
        v_out = 0d0
        do n=1, this%n_classes, 1
            v_out = v_out - sum(v_in_1(:,n) * log(v_in_2(:,n) + epsilon_for_log))
        end do
        
        if (this%reduction == "mean") then
            n_rows = size(v_in_2, dim=1)
            v_out = v_out / dble(n_rows)
        end if
    end subroutine forward_cross_entropy

    function backward_cross_entropy(this, g_in) result(g_outs)
        implicit none
        class(cross_entropy) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: shape1(2), shape2(2)

        integer(kind=8) :: id_in_1, id_in_2, n_rows, n

        id_in_1 = this%id_in_1
        id_in_2 = this%id_in_2
        n_rows = size(vstack(id_in_2)%v, dim=1)
        allocate(g_outs(1)%g, source=vstack(id_in_1)%v)
        allocate(g_outs(2)%g, source=vstack(id_in_2)%v)
        g_outs(1)%g = 0d0
        g_outs(2)%g = 0d0

        do n=1, this%n_classes, 1
            g_outs(1)%g = g_outs(1)%g - log(vstack(id_in_2)%v + epsilon_for_log)
            g_outs(2)%g = g_outs(2)%g - vstack(id_in_1)%v / (vstack(id_in_2)%v + epsilon_for_log)
        end do
        
        if (this%reduction == "mean") then
            n_rows = size(vstack(id_in_2)%v, dim=1)
            g_outs(1)%g = g_outs(1)%g / dble(n_rows)
            g_outs(2)%g = g_outs(2)%g / dble(n_rows)
        end if
    end function backward_cross_entropy

end module mod_cross_entropy











