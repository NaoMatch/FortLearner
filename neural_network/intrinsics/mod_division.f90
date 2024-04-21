module mod_division
    use mod_variable
    implicit none

    type, extends(base_function) :: division
    contains
        procedure :: forward_2in_1out => forward_division
        procedure :: backward_1in_2out => backward_division
    end type division
    
    interface division
        module procedure new_division
    end interface division

    interface operator(/)
        module procedure division_var_var
        module procedure division_r8_var
        module procedure division_var_r8
        module procedure division_var_i8
    end interface operator(/)

contains

    function division_var_var(var_in_1, var_in_2) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in_1, var_in_2
        type(variable) :: var_out
        type(division) :: div
        div = division()
        var_out = div%act(var_in_1, var_in_2)
    end function division_var_var    

    function division_r8_var(scl, var_in) result(var_out)
        implicit none
        real(kind=8), intent(in) :: scl
        type(variable), intent(in) :: var_in
        type(variable) :: var_out
        type(variable) :: var_scl
        type(division) :: div
        div = division()
        var_scl = variable(scl)
        var_out = div%act(var_scl, var_in)
    end function division_r8_var    

    function division_var_r8(var_in, scl) result(var_out)
        implicit none
        real(kind=8), intent(in) :: scl
        type(variable), intent(in) :: var_in
        type(variable) :: var_out
        type(variable) :: var_scl
        type(division) :: div
        div = division()
        var_scl = variable(scl)
        var_out = div%act(var_in, var_scl)
    end function division_var_r8    

    function division_var_i8(var_in, scl) result(var_out)
        implicit none
        integer(kind=8), intent(in) :: scl
        type(variable), intent(in) :: var_in
        type(variable) :: var_out
        type(variable) :: var_scl
        type(division) :: div
        div = division()
        var_scl = variable(scl)
        var_out = div%act(var_in, var_scl)
    end function division_var_i8    

    function new_division()
        implicit none
        type(division) :: new_division
        new_division%fname = "division"
        new_division%n_in = 2
        new_division%n_out = 1
    end function new_division

    function forward_division(this, v_in_1, v_in_2) result(v_out)
        implicit none
        class(division) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable :: v_out(:,:)

        integer(kind=8) :: shape1(2), shape2(2)

        shape1 = shape(v_in_1)
        shape2 = shape(v_in_2)

        if (any(shape1 /= shape2)) then ! Shape Mismatch pattern
            if (all(shape1 == [1_8, 1_8])) then
                v_out = v_in_1(1,1) / v_in_2
            elseif (all(shape2 == [1_8, 1_8])) then
                v_out = v_in_1 / v_in_2(1,1)
            else
                print*, "Input Shape : ", shape(v_in_1), " vs ", shape(v_in_2), "."
                stop "No Autobroadcast Implemented. Use 'spread_(x, dim, ncopies)'."
            end if
        else
            v_out = v_in_1 / v_in_2
        end if
    end function forward_division

    function backward_division(this, g_in) result(g_outs)
        implicit none
        class(division) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in / vstack(this%id_in_2)%v
        g_outs(2)%g = g_in * (-vstack(this%id_in_1)%v / (vstack(this%id_in_2)%v**2d0))
    end function backward_division

end module mod_division