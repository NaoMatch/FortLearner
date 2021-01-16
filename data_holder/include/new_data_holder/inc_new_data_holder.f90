function new_data_holder_r8_i8(x, y)
    implicit none
    type(data_holder)       :: new_data_holder_r8_i8
    real(kind=8), target    :: x(:,:)
    integer(kind=8), target :: y(:,:)

    integer(kind=8) :: x_shape(2), y_shape(2)

    x_shape = shape(x)
    y_shape = shape(y)

    new_data_holder_r8_i8 % n_outputs         =  y_shape(2)
    new_data_holder_r8_i8 % n_samples         =  x_shape(1)
    new_data_holder_r8_i8 % n_columns         =  x_shape(2)
    new_data_holder_r8_i8 % x_ptr % x_r8_ptr  => x
    new_data_holder_r8_i8 % y_ptr % y_i8_ptr  => y

    new_data_holder_r8_i8 % is_preprocessed = f_
end function new_data_holder_r8_i8
