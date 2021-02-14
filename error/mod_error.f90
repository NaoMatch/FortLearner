module mod_error
    use mod_common
    implicit none
    
    type error
    contains
        procedure :: check_estimator_is_fitted
        procedure :: only_accept_Nx1_matrix

        procedure, pass :: check_number_of_features_mismatch_i4
        procedure, pass :: check_number_of_features_mismatch_i8
        generic :: check_number_of_features_mismatch => check_number_of_features_mismatch_i4, check_number_of_features_mismatch_i8

        procedure, pass :: sample_size_mismatch_matrix
        procedure, pass :: sample_size_mismatch_vector
        generic :: sample_size_mismatch => sample_size_mismatch_vector, sample_size_mismatch_matrix

        procedure, pass :: is_binary_labels_i4
        procedure, pass :: is_binary_labels_i8
        generic :: is_binary_labels => is_binary_labels_i4, is_binary_labels_i8
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


    subroutine sample_size_mismatch_vector(this, n_samples_x, variable_name_x, n_samples_y, variable_name_y, function_name)
        implicit none
        class(error)     :: this
        integer(kind=8)  :: n_samples_x, n_samples_y
        character(len=*) :: variable_name_x, variable_name_y
        character(len=*) :: function_name
        character(len=512) :: err_msg

        if (n_samples_x .ne. n_samples_y) then
            err_msg = "ShapeMismatch: The number of samples for " // trim(variable_name_x) // " and " // trim(variable_name_y)
            err_msg = trim(err_msg) // " must be the same in " // trim(function_name) // "."
            stop trim(err_msg)
        end if
    end subroutine sample_size_mismatch_vector

    subroutine sample_size_mismatch_matrix(this, shape_x, variable_name_x, shape_y, variable_name_y, function_name)
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
    end subroutine sample_size_mismatch_matrix

    subroutine is_binary_labels_i4(this, labels, n_samples, function_name)
        class(error)     :: this
        integer(kind=4)  :: labels(n_samples, 1_4)
        integer(kind=4)  :: n_samples
        character(len=*) :: function_name

        integer(kind=4)    :: n
        integer(kind=4)    :: min, max, val
        character(len=512) :: err_msg

        min =  huge(0_4)
        max = -huge(0_4)
        
        do n=1, n_samples, 1
            val = labels(n,1)
            min = minval((/min, val/))
            max = maxval((/max, val/))
        end do
        if (min .eq. 0_4 .and. max .eq. 1_4) return

        err_msg = "ValueError: The intput labels must be 0 and 1 for " // trim(function_name) // "."
        stop trim(err_msg)
    end subroutine is_binary_labels_i4

    subroutine is_binary_labels_i8(this, labels, n_samples, function_name)
        class(error)     :: this
        integer(kind=8)  :: labels(n_samples, 1_8)
        integer(kind=8)  :: n_samples
        character(len=*) :: function_name

        integer(kind=8)    :: n
        integer(kind=8)    :: min, max, val
        character(len=512) :: err_msg

        min =  huge(0_8)
        max = -huge(0_8)
        
        do n=1, n_samples, 1
            val = labels(n,1)
            min = minval((/min, val/))
            max = maxval((/max, val/))
        end do
        if (min .eq. 0_8 .and. max .eq. 1_8) return

        err_msg = "ValueError: The intput labels must be 0 and 1 for " // trim(function_name) // "."
        stop trim(err_msg)
    end subroutine is_binary_labels_i8

end module mod_error
