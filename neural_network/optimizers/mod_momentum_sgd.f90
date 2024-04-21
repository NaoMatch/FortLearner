module mod_momentum_sgd
    use mod_variable
    implicit none

    type, extends(optimizer) :: momentum_sgd
        real(kind=8) :: momentum
    contains
        procedure :: update_one_param => update_one_param_momentum_sgd
    end type momentum_sgd
    
    interface momentum_sgd
        module procedure new_momentum_sgd
    end interface

contains

    function new_momentum_sgd(lr, momentum)
        implicit none
        type(momentum_sgd) :: new_momentum_sgd
        real(kind=8) :: lr
        real(kind=8) :: momentum
        new_momentum_sgd%lr = lr
        new_momentum_sgd%momentum = momentum
    end function new_momentum_sgd

    subroutine update_one_param_momentum_sgd(this, id)
        implicit none
        class(momentum_sgd) :: this
        integer(kind=8) :: id

        if (.not. allocated(vstack(id)%c)) then
            allocate(vstack(id)%c, source=vstack(id)%v)
            vstack(id)%c = 0d0
        end if

        vstack(id)%c = this%momentum*vstack(id)%c - this%lr*vstack(id)%g

        vstack(id)%v = vstack(id)%v + vstack(id)%c
    end subroutine update_one_param_momentum_sgd


    
end module mod_momentum_sgd