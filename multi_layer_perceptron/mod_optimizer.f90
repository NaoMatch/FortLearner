module mod_optimizer
    use mod_var
    implicit none
    
    type optimizer
        real(kind=8) :: learning_rate
        real(kind=8) :: alpha
    contains
        procedure :: update => update_optimizer
    end type optimizer

    interface optimizer
        module procedure :: new_optimizer
    end interface optimizer

contains

    function new_optimizer(learning_rate, alpha)
        implicit none
        type(optimizer) :: new_optimizer
        real(kind=8) :: learning_rate
        real(kind=8) :: alpha
        new_optimizer%learning_rate = learning_rate
        new_optimizer%alpha = alpha
    end function new_optimizer

    subroutine update_optimizer(this, variables)
        implicit none
        class(optimizer) :: this
        type(variable_ptr), intent(inout) :: variables(:)
        integer(kind=8) :: i, g_shape(2)

        do i=1, size(variables), 1
            if ( .not. allocated(variables(i)%ptr%g_) ) then
                g_shape = shape(variables(i)%ptr%g)
                allocate(variables(i)%ptr%g_(g_shape(1), g_shape(2)))
            end if

            variables(i)%ptr%v = variables(i)%ptr%v & 
                            - this%learning_rate * variables(i)%ptr%g
            variables(i)%ptr%g_ = variables(i)%ptr%g
        end do
    end subroutine update_optimizer

end module mod_optimizer