module mod_sgd
    use mod_variable
    implicit none

    type, extends(optimizer) :: sgd
    contains
        procedure :: update_one_param => update_one_param_sgd
    end type sgd
    
    interface sgd
        module procedure new_sgd
    end interface

contains

    function new_sgd(lr)
        implicit none
        type(sgd) :: new_sgd
        real(kind=8) :: lr
        new_sgd%lr = lr
    end function new_sgd

    subroutine update_one_param_sgd(this, id)
        implicit none
        class(sgd) :: this
        integer(kind=8) :: id

        vstack(id)%v = vstack(id)%v - this%lr * vstack(id)%g
    end subroutine update_one_param_sgd


    
end module mod_sgd