module mod_reshaping
    use mod_variable
    implicit none

    type, extends(base_function) :: reshaping
    contains
        procedure :: forward_1in_1out => forward_reshaping
        procedure :: backward_1in_2out => backward_reshaping
    end type reshaping

    interface reshaping
        module procedure new_reshaping
    end interface reshaping    

contains
    function reshape_(var_in, shape) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out
        integer(kind=8) :: shape(2)
        type(reshaping) :: rshp

        rshp = reshaping(shape=shape)

        var_out = rshp%act(var_in)
    end function reshape_

    function new_reshaping(shape)
        implicit none
        type(reshaping) :: new_reshaping
        integer(kind=8) :: shape(2)
        new_reshaping%fname = "reshaping"
        new_reshaping%n_in = 1
        new_reshaping%n_out = 1
        new_reshaping%shape = shape
    end function new_reshaping

    subroutine forward_reshaping(this, v_out, v_in)
        implicit none
        class(reshaping) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        integer(kind=8) :: n_cols, n_rows

        v_out = reshape(v_in, shape=this%shape)
    end subroutine forward_reshaping

    function backward_reshaping(this, g_in) result(g_outs)
        implicit none
        class(reshaping) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        integer(kind=8) :: n_cols, n_rows

        g_outs(1)%g = reshape(g_in, shape=shape(vstack(this%id_in_1)%v))
    end function backward_reshaping    

end module mod_reshaping