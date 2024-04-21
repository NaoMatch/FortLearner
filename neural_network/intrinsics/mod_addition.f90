module mod_addition
    use mod_variable
    implicit none

    type, extends(base_function) :: addition
    contains
        procedure :: forward_2in_1out => forward_addition
        procedure :: backward_1in_2out => backward_addition
    end type addition
    
    interface addition
        module procedure new_addition
    end interface addition

    interface operator(+)
        module procedure addition_var_var
        module procedure addition_var_r8
        module procedure addition_r8_var
    end interface operator(+)

contains
    
    function add_(var_in_1, var_in_2) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in_1, var_in_2
        type(variable) :: var_out
        var_out = var_in_1 + var_in_2
    end function 

    function addition_var_var(var_in_1, var_in_2) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in_1, var_in_2
        type(variable) :: var_out
        type(addition) :: add
        add = addition()
        var_out = add%act(var_in_1, var_in_2)
    end function addition_var_var    

    function addition_var_r8(var_in, scl) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in
        real(kind=8), intent(in) :: scl
        type(variable) :: var_out
        type(variable) :: var_scl
        type(addition) :: add
        var_scl = variable(scl)
        add = addition()
        var_out = add%act(var_in, var_scl)
    end function addition_var_r8    

    function addition_r8_var(scl, var_in) result(var_out)
        implicit none
        real(kind=8), intent(in) :: scl
        type(variable), intent(in) :: var_in
        type(variable) :: var_out
        type(variable) :: var_scl
        type(addition) :: add
        var_scl = variable(scl)
        add = addition()
        var_out = add%act(var_scl, var_in)
    end function addition_r8_var    

    function new_addition()
        implicit none
        type(addition) :: new_addition
        new_addition%fname = "addition"
        new_addition%n_in = 2
        new_addition%n_out = 1
    end function new_addition

    function forward_addition(this, v_in_1, v_in_2) result(v_out)
        implicit none
        class(addition) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable :: v_out(:,:)

        integer(kind=8) :: shape1(2), shape2(2)

        shape1 = shape(v_in_1)
        shape2 = shape(v_in_2)
        if (any(shape1 /= shape2)) then ! Shape Mismatch pattern
            if (all(shape1 == [1_8, 1_8])) then
                v_out = v_in_1(1,1) + v_in_2
            elseif (all(shape2 == [1_8, 1_8])) then
                v_out = v_in_1 + v_in_2(1,1)
            else
                print*, "Input Shape : ", shape(v_in_1), " vs ", shape(v_in_2), "."
                stop "No Autobroadcast Implemented. Use 'spread_(x, dim, ncopies)'."
            end if
        else
            v_out = v_in_1 + v_in_2
        end if
    end function forward_addition

    function backward_addition(this, g_in) result(g_outs)
        implicit none
        class(addition) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: shape1(2), shape2(2)

        shape1 = shape(vstack(this%id_in_1)%v)
        shape2 = shape(vstack(this%id_in_2)%v)

        if (any(shape1 /= shape2)) then ! Shape Mismatch pattern
            if     (all(shape1 == [1_8, 1_8])) then
                g_outs(1)%g = reshape([sum(g_in)], shape=shape1)
                g_outs(2)%g = g_in
            elseif (all(shape2 == [1_8, 1_8])) then
                g_outs(1)%g = g_in
                g_outs(2)%g = reshape([sum(g_in)], shape=shape2)
            end if
        else
            g_outs(1)%g = g_in
            g_outs(2)%g = g_in
        end if
    end function backward_addition

end module mod_addition