module mod_summation
    use mod_variable
    implicit none

    type, extends(base_function) :: summation
    contains
        procedure :: forward_1in_1out => forward_summation
        procedure :: backward_1in_2out => backward_summation
    end type summation

    interface summation
        module procedure new_summation
    end interface summation    

contains

    function sum_(var_in, dim) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out
        integer(kind=8), optional :: dim

        integer(kind=8) :: dim_opt

        type(summation) :: summing

        dim_opt = -1
        if (present(dim)) dim_opt = dim

        summing = summation(dim=dim_opt)

        var_out = summing%act(var_in)
    end function sum_


    function new_summation(dim)
        implicit none
        type(summation) :: new_summation
        integer(kind=8), optional :: dim
        new_summation%fname = "summation"
        new_summation%n_in = 1
        new_summation%n_out = 1
        if (present(dim)) new_summation%dim = dim
    end function new_summation

    subroutine forward_summation(this, v_out, v_in)
        implicit none
        class(summation) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        integer(kind=8) :: n_cols, n_rows
        if (this%dim==-1) then
            v_out = reshape([sum(v_in)], [1,1])
        elseif (this%dim==1) then
            n_cols = size(v_in, dim=2)
            v_out = reshape(sum(v_in, dim=this%dim), [1_8,n_cols])
        elseif (this%dim==2) then
            n_rows = size(v_in, dim=1)
            v_out = reshape(sum(v_in, dim=this%dim), [n_rows,1_8])
        end if
    end subroutine forward_summation

    function backward_summation(this, g_in) result(g_outs)
        implicit none
        class(summation) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        integer(kind=8) :: n_cols, n_rows

        if (this%dim==-1) then
            allocate(g_outs(1)%g, source=vstack(this%id_in_1)%v)
            g_outs(1)%g(:,:) = g_in(1,1)
        elseif (this%dim==1) then
            n_rows = size(vstack(this%id_in_1)%v, dim=1)
            g_outs(1)%g = spread(g_in(1,:), dim=1, ncopies=n_rows)
        elseif (this%dim==2) then
            n_cols = size(vstack(this%id_in_1)%v, dim=2)
            g_outs(1)%g = spread(g_in(:,1), dim=2, ncopies=n_cols)
        end if
    end function backward_summation    

end module mod_summation