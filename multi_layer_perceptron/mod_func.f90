module mod_func
    use mod_var
    implicit none

    type func
    contains
        procedure :: act => act_base
    end type func

    type, extends(func) :: square
    contains
        procedure :: act => act_square
    end type square

    type, extends(func) :: exponential
    contains
        procedure :: act => act_exponential
    end type exponential
    
    type, extends(func) :: sub
    end type sub
contains

    function act_base(this, var)
        implicit none
        class(func) :: this
        type(variable) :: act_base
        type(variable) :: var
        stop "NotImplementedError"
    end function act_base

    function act_square(this, var)
        implicit none
        class(square) :: this
        type(variable) :: act_square
        type(variable) :: var
        act_square%v = var%v**2d0
    end function act_square

    function act_exponential(this, var)
        implicit none
        class(exponential) :: this
        type(variable) :: act_exponential
        type(variable) :: var
        act_exponential%v = exp(var%v)
    end function act_exponential

end module mod_func