module mod_error

    implicit none
    
    type error
    contains
        procedure :: sample_size_mismatch
        procedure :: only_accept_Nx1_matrix
        procedure :: check_estimator_is_fitted
    end type

contains

    subroutine check_estimator_is_fitted(this, is_trained, function_name)
        implicit none
        class(error)     :: this
        logical(kind=4)  :: is_trained
        character(len=*) :: function_name
        character(len=512) :: err_msg

        if ( is_trained ) then
        else
            err_msg = "NotFitted: Estimator '" // trim(function_name) // "' is not fitted yet."
            stop trim(err_msg)
        end if
    end subroutine check_estimator_is_fitted       


    subroutine sample_size_mismatch(this, shape_x, variable_name_x, shape_y, variable_name_y, function_name)
        implicit none
        class(error)     :: this
        integer(kind=8)  :: shape_x(2), shape_y(2)
        character(len=*) :: variable_name_x, variable_name_y
        character(len=*) :: function_name
        character(len=512) :: err_msg

        if (shape_x(1) .ne. shape_y(1)) then
            err_msg = "ShapeMismatch: The number of samples for " // trim(variable_name_x) // " and " // trim(variable_name_y)
            err_msg = trim(err_msg) // " must be the same in " // trim(function_name) // "."
            stop trim(err_msg)
        end if
    end subroutine sample_size_mismatch

    subroutine only_accept_Nx1_matrix(this, shape_y, variable_name_y, function_name)
        class(error)     :: this
        integer(kind=8)  :: shape_y(2), one_i=1_8
        character(len=*) :: variable_name_y
        character(len=*) :: function_name
        character(len=512) :: err_msg

        if (shape_y(2) .ne. one_i) then
            err_msg = "ShapeMismatch: The number of columns for " // trim(variable_name_y)
            err_msg = trim(err_msg) // " must be '1' in " // trim(function_name) // "."
            stop trim(err_msg)
        end if
    end subroutine only_accept_Nx1_matrix

end module mod_error
