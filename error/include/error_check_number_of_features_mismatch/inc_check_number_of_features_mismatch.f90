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
