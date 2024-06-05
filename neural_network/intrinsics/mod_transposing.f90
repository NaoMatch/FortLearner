module mod_transposing
    use mod_variable
    implicit none

    type, extends(base_function) :: transposing
    contains
        procedure :: forward_1in_1out => forward_transposing
        procedure :: backward_1in_2out => backward_transposing
    end type transposing

    interface transposing
        module procedure new_transposing
    end interface transposing    

contains

    function transpose_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(transposing) :: trans

        trans = transposing()

        var_out = trans%act(var_in)
    end function transpose_

    function new_transposing()
        implicit none
        type(transposing) :: new_transposing
        integer(kind=8) :: shape(2)
        new_transposing%fname = "transposing"
        new_transposing%n_in = 1
        new_transposing%n_out = 1
    end function new_transposing

    subroutine forward_transposing(this, v_out, v_in)
        implicit none
        class(transposing) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        integer(kind=8) :: n_cols, n_rows

        v_out = transpose(v_in)
    end subroutine forward_transposing

    function backward_transposing(this, g_in) result(g_outs)
        implicit none
        class(transposing) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        integer(kind=8) :: n_cols, n_rows, shape_g(2)

        shape_g = shape(g_in)
        n_rows = shape_g(1)
        n_cols = shape_g(2)

        allocate(g_outs(1)%g(n_cols, n_rows))
        g_outs(1)%g(:,:) = transpose(g_in(:,:))
    end function backward_transposing    

end module mod_transposing