module mod_spreading
    use mod_variable
    implicit none

    type, extends(base_function) :: spreading
    contains
        procedure :: forward_1in_1out => forward_spreading
        procedure :: backward_1in_2out => backward_spreading
    end type spreading

    interface spreading
        module procedure new_spreading
    end interface spreading    

contains

    function spread_(var_in, dim, ncopies) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out
        integer(kind=8) :: dim
        integer(kind=8) :: ncopies

        type(spreading) :: spr

        spr = spreading(dim=dim, ncopies=ncopies)

        var_out = spr%act(var_in)
    end function spread_

    function new_spreading(dim, ncopies)
        implicit none
        type(spreading) :: new_spreading
        integer(kind=8) :: dim
        integer(kind=8) :: ncopies
        new_spreading%fname = "spreading"
        new_spreading%n_in = 1
        new_spreading%n_out = 1
        new_spreading%dim = dim
        new_spreading%ncopies = ncopies
    end function new_spreading

    subroutine forward_spreading(this, v_out, v_in)
        implicit none
        class(spreading) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        integer(kind=8) :: n_cols, n_rows

        if (this%dim==1) then
            v_out = spread(v_in(1,:), dim=this%dim, ncopies=this%ncopies)
        else
            v_out = spread(v_in(:,1), dim=this%dim, ncopies=this%ncopies)
        end if
    end subroutine forward_spreading

    function backward_spreading(this, g_in) result(g_outs)
        implicit none
        class(spreading) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        integer(kind=8) :: n_cols, n_rows

        g_outs(1)%g = reshape(sum(g_in, dim=this%dim), shape=shape(vstack(this%id_in_1)%v))
    end function backward_spreading    

end module mod_spreading