module mod_concatination
    use mod_variable
    implicit none

    type, extends(base_function) :: concatination
    contains
        procedure :: forward_2in_1out => forward_concatination
        procedure :: backward_1in_2out => backward_concatination
    end type concatination
    
    interface concatination
        module procedure new_concatination
    end interface concatination

    interface concat_
        module procedure concat_2vars
        module procedure concat_many_vars
    end interface concat_

contains

    function concat_2vars(var_in_1, var_in_2, dim) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in_1, var_in_2
        type(variable) :: var_out
        type(concatination) :: concat
        integer(kind=8) :: dim
        concat = concatination(dim)
        var_out = concat%act(var_in_1, var_in_2)
    end function concat_2vars

    function concat_many_vars(vars, dim) result(var_out)
        implicit none
        type(variable), intent(in) :: vars(:)
        type(variable) :: var_out
        integer(kind=8) :: dim

        integer(kind=8) :: n_vars, v

        n_vars = size(vars)

        if (n_vars == 1_8) then
            var_out = vars(1)
        elseif (n_vars==2_8) then
            var_out = concat_2vars(vars(1), vars(2), dim)
        else
            var_out = concat_2vars(vars(1), vars(2), dim)
            do v=3, n_vars, 1
                var_out = concat_2vars(vars(v), var_out, dim)
            end do    
        end if
    end function concat_many_vars

    function new_concatination(dim)
        implicit none
        type(concatination) :: new_concatination
        integer(kind=8) :: dim
        new_concatination%fname = "concatination"
        new_concatination%n_in = 2
        new_concatination%n_out = 1
        new_concatination%dim = dim
    end function new_concatination

    subroutine forward_concatination(this, v_out, v_in_1, v_in_2)
        implicit none
        class(concatination) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)

        integer(kind=8) :: shape1(2), shape2(2)

        shape1 = shape(v_in_1)
        shape2 = shape(v_in_2)

        if (this%dim == 1_8 .and. shape1(2) /= shape2(2)) goto 990
        if (this%dim == 2_8 .and. shape1(1) /= shape2(1)) goto 990

        if (this%dim == 1_8) then
            allocate(v_out(shape1(1)+shape2(1), shape1(2)))
            v_out(1:shape1(1), :) = v_in_1
            v_out(shape1(1)+1:,:) = v_in_2
        elseif (this%dim == 2_8) then
            allocate(v_out(shape1(1), shape1(2)+shape2(2)))
            v_out(:, 1:shape1(2))  = v_in_1
            v_out(:, shape1(2)+1:) = v_in_2
        else
            stop "NotImplemented Error, " // __FILE__
        end if

        return
        990 continue 
        print*, "Shape ", shape1, " ::: ", shape2
        stop trim(this%fname) // " shape mismatch error."
    end subroutine forward_concatination

    function backward_concatination(this, g_in) result(g_outs)
        implicit none
        class(concatination) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: shape1(2), shape2(2)

        shape1 = shape(vstack(this%id_in_1)%v)
        shape2 = shape(vstack(this%id_in_2)%v)

        if (this%dim==1_8) then
            g_outs(1)%g = g_in(1:shape1(1),  :)
            g_outs(2)%g = g_in(shape1(1)+1:, :)
        else
            g_outs(1)%g = g_in(:, 1:shape1(2)) 
            g_outs(2)%g = g_in(:, shape1(2)+1:)
        end if
    end function backward_concatination

end module mod_concatination