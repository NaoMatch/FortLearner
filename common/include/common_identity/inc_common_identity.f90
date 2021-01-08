   subroutine identity_real64(matrix, n_dim)
        implicit none
        real(kind=8), intent(inout) :: matrix(n_dim,n_dim)
        integer(kind=8), intent(in) :: n_dim
        integer(kind=8) :: i
        real(kind=8)    :: val_1, val_0
        val_1 = 1d0; val_0 = 0d0
        include "./include/common_identity/inc_common_identity_detail.f90"
    end subroutine identity_real64

    subroutine identity_int32(matrix, n_dim)
        implicit none
        integer(kind=4), intent(inout) :: matrix(n_dim,n_dim)
        integer(kind=4), intent(in) :: n_dim
        integer(kind=4) :: i
        integer(kind=4) :: val_1, val_0
        val_1 = 1; val_0 = 0
        include "./include/common_identity/inc_common_identity_detail.f90"
    end subroutine identity_int32

    subroutine identity_int64(matrix, n_dim)
        implicit none
        integer(kind=8), intent(inout) :: matrix(n_dim,n_dim)
        integer(kind=8), intent(in) :: n_dim
        integer(kind=8) :: i
        integer(kind=8)    :: val_1, val_0
        val_1 = 1_8; val_0 = 0_8
        include "./include/common_identity/inc_common_identity_detail.f90"
    end subroutine identity_int64
