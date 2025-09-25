module mod_is_positive_sized_array 
    use :: mod_kinds
    implicit none

    interface is_positive_sized_array
        module procedure is_positive_sized_vector_r64
        module procedure is_positive_sized_vector_i64
        module procedure is_positive_sized_matrix_r64
        module procedure is_positive_sized_matrix_i64
    end interface is_positive_sized_array
    
contains

    pure function is_positive_sized_vector_r64(vec) result(ok)
        implicit none
        real(r64), intent(in) :: vec(:)
        logical :: ok

        ok = size(vec) > 0_i64
    end function is_positive_sized_vector_r64

    pure function is_positive_sized_vector_i64(vec) result(ok)
        implicit none
        integer(i64), intent(in) :: vec(:)
        logical :: ok

        ok = size(vec) > 0_i64
    end function is_positive_sized_vector_i64

    pure function is_positive_sized_matrix_r64(mat) result(ok)
        implicit none
        real(r64), intent(in) :: mat(:,:)
        logical :: ok

        ok = all(int(shape(mat), kind=i64) > 0_i64)
    end function is_positive_sized_matrix_r64

    pure function is_positive_sized_matrix_i64(mat) result(ok)
        implicit none
        integer(i64), intent(in) :: mat(:,:)
        logical :: ok

        ok = all(int(shape(mat), kind=i64) > 0_i64)
    end function is_positive_sized_matrix_i64

end module mod_is_positive_sized_array 
