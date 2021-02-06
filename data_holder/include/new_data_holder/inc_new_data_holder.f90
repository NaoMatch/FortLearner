function new_data_holder_r8_i8(x, y)
    implicit none
    type(data_holder)       :: new_data_holder_r8_i8
    real(kind=8), target    :: x(:,:)
    integer(kind=8), target :: y(:,:)

    integer(kind=8) :: x_shape(2), y_shape(2)
    type(error)     :: err
    
    x_shape = shape(x)
    y_shape = shape(y)
    call err % sample_size_mismatch(x_shape, "x", y_shape, "y", "data_holder")

    new_data_holder_r8_i8 % n_outputs         =  y_shape(2)
    new_data_holder_r8_i8 % n_samples         =  x_shape(1)
    new_data_holder_r8_i8 % n_columns         =  x_shape(2)
    new_data_holder_r8_i8 % x_shape           =  x_shape
    new_data_holder_r8_i8 % y_shape           =  y_shape
    new_data_holder_r8_i8 % x_ptr % x_r8_ptr  => x
    new_data_holder_r8_i8 % y_ptr % y_i8_ptr  => y

    new_data_holder_r8_i8 % is_preprocessed = f_
end function new_data_holder_r8_i8

function new_data_holder_i4_r4(x, y)
    implicit none
    type(data_holder)       :: new_data_holder_i4_r4
    integer(kind=4), target :: x(:,:)
    real(kind=4), target    :: y(:,:)

    integer(kind=8) :: x_shape(2), y_shape(2)
    type(error)     :: err

    x_shape = shape(x)
    y_shape = shape(y)
    call err % sample_size_mismatch(x_shape, "x", y_shape, "y", "data_holder")

    new_data_holder_i4_r4 % n_outputs         =  y_shape(2)
    new_data_holder_i4_r4 % n_samples         =  x_shape(1)
    new_data_holder_i4_r4 % n_columns         =  x_shape(2)
    new_data_holder_i4_r4 % x_shape           =  x_shape
    new_data_holder_i4_r4 % y_shape           =  y_shape
    new_data_holder_i4_r4 % x_ptr % x_i4_ptr  => x
    new_data_holder_i4_r4 % y_ptr % y_r4_ptr  => y

    new_data_holder_i4_r4 % is_preprocessed = f_
end function new_data_holder_i4_r4
