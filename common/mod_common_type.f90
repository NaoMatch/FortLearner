module mod_common_type

    implicit none
    
    type jagged_vector_i8
        integer(kind=8), allocatable :: vector(:)
    end type jagged_vector_i8
    
    type jagged_vector_r8
        real(kind=8), allocatable :: vector(:)
    end type jagged_vector_r8

end module mod_common_type
