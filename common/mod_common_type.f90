module mod_common_type

    implicit none
    
    type jagged_vector_i8
        integer(kind=8), allocatable :: vector(:)
    end type jagged_vector_i8
    
    type jagged_vector_r8
        real(kind=8), allocatable :: vector(:)
    end type jagged_vector_r8

    type jagged_matrix_r8
        real(kind=8), allocatable :: offsets(:) 
        real(kind=8), allocatable :: matrix(:,:)
    end type jagged_matrix_r8

    type jagged_array_i8
        integer(kind=8), allocatable :: indices(:)
    end type jagged_array_i8

    type jagged_array_of_array_i8
        integer(kind=8), allocatable :: hashval2id(:)
        type(jagged_array_i8), ALLOCATABLE :: arrays(:)
    end type jagged_array_of_array_i8

end module mod_common_type
