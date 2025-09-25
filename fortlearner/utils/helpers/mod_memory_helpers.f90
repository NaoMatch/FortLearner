module mod_memory_helpers
    use :: mod_kinds
    implicit none
    

contains

    subroutine force_deallocate_vec_i64(vec)
        integer(i64), allocatable, intent(inout) :: vec(:)
        if (allocated(vec)) deallocate(vec)
    end subroutine force_deallocate_vec_i64

    subroutine force_deallocate_vec_r64(vec)
        real(r64), allocatable, intent(inout) :: vec(:)
        if (allocated(vec)) deallocate(vec)
    end subroutine force_deallocate_vec_r64

    subroutine force_deallocate_mat_i64(mat)
        integer(i64), allocatable, intent(inout) :: mat(:,:)
        if (allocated(mat)) deallocate(mat)
    end subroutine force_deallocate_mat_i64

    subroutine force_deallocate_mat_r64(mat)
        real(r64), allocatable, intent(inout) :: mat(:,:)
        if (allocated(mat)) deallocate(mat)
    end subroutine force_deallocate_mat_r64


end module mod_memory_helpers
