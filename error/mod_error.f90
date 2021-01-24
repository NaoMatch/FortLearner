module mod_error
    use mod_common
    implicit none
    
    type error
    contains
        procedure :: check_estimator_is_fitted
        procedure, pass :: check_number_of_features_mismatch_i4
        procedure, pass :: check_number_of_features_mismatch_i8
        generic :: check_number_of_features_mismatch => check_number_of_features_mismatch_i4, check_number_of_features_mismatch_i8
        procedure :: only_accept_Nx1_matrix
        procedure :: sample_size_mismatch
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


    subroutine check_number_of_features_mismatch_i4(this, n_columns_train, n_columns_transform, function_name)
        class(error)     :: this
        integer(kind=4)  :: n_columns_train, n_columns_transform
        character(len=*) :: function_name
        character(len=512) :: err_msg
        character(:), ALLOCATABLE :: char_train, char_transform

        if (n_columns_train .ne. n_columns_transform) then
            char_train = num2char(n_columns_train)
            char_transform = num2char(n_columns_transform)
            err_msg = "ValueError: The number of columns does not match the number of columns during training."
            err_msg = trim(err_msg) // " " // trim(char_train) // " in Train, " // trim(char_transform) // " in Transform."
            stop trim(err_msg)
        end if
    end subroutine check_number_of_features_mismatch_i4


    subroutine check_number_of_features_mismatch_i8(this, n_columns_train, n_columns_transform, function_name)
        class(error)     :: this
        integer(kind=8)  :: n_columns_train, n_columns_transform
        character(len=*) :: function_name
        character(len=512) :: err_msg
        character(:), ALLOCATABLE :: char_train, char_transform

        if (n_columns_train .ne. n_columns_transform) then
            char_train = num2char(n_columns_train)
            char_transform = num2char(n_columns_transform)
            err_msg = "ValueError: The number of columns does not match the number of columns during training."
            err_msg = trim(err_msg) // " " // trim(char_train) // " in Train, " // trim(char_transform) // " in Transform."
            stop trim(err_msg)
        end if
    end subroutine check_number_of_features_mismatch_i8


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

end module mod_error
