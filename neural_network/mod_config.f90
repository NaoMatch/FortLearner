module mod_config
    implicit none

    logical(kind=4) :: is_train = .false.

contains
    
    subroutine train_mode()
        implicit none
        is_train = .true.
    end subroutine train_mode
    
    subroutine test_mode()
        implicit none
        is_train = .false.
    end subroutine test_mode

end module mod_config