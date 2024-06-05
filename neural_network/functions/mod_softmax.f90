module mod_softmax
    use mod_variable
    implicit none

    type, extends(base_function) :: softmax
    contains
        procedure :: forward_1in_1out => forward_softmax
        procedure :: backward_1in_2out => backward_softmax
    end type softmax
    
    interface softmax
        module procedure new_softmax
    end interface softmax

contains

    function softmax_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(softmax) :: smax

        smax = softmax()

        var_out = smax%act(var_in)
    end function softmax_


    function new_softmax()
        implicit none
        type(softmax) :: new_softmax
        new_softmax%fname = "softmax"
        new_softmax%n_in = 1
        new_softmax%n_out = 1
    end function new_softmax

    subroutine forward_softmax(this, v_out, v_in)
        implicit none
        class(softmax) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)

        integer(kind=8) :: n_cols
        real(kind=8), allocatable :: v_in_max(:), exp_v_in(:,:)

        n_cols = size(v_in, dim=2)

        v_in_max = maxval(v_in, dim=2)

        exp_v_in = v_in - spread(v_in_max, dim=2, ncopies=n_cols)
        exp_v_in = exp(exp_v_in)

        v_out = exp_v_in / spread(sum(exp_v_in, dim=2), dim=2, ncopies=n_cols)
    end subroutine forward_softmax

    function backward_softmax(this, g_in) result(g_outs)
        implicit none
        class(softmax) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: n_cols
        real(kind=8), allocatable :: sumdx(:,:)

        n_cols = size(g_in, dim=2)

        g_outs(1)%g = g_in * vstack(this%id_out_1)%v
        sumdx = spread(sum(g_outs(1)%g, dim=2), dim=2, ncopies=n_cols) * vstack(this%id_out_1)%v
        g_outs(1)%g = g_outs(1)%g - sumdx
    end function backward_softmax

end module mod_softmax