! gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_identity.f90 -o main_identity.out
program main_identity
    use mod_common, only: identity
    implicit none
    
    real(kind=4), allocatable    :: id_mat_r4(:,:)
    real(kind=8), allocatable    :: id_mat_r8(:,:)
    integer(kind=4), allocatable :: id_mat_i4(:,:)
    integer(kind=8), allocatable :: id_mat_i8(:,:)

    integer(kind=4) :: n_dim_i4
    integer(kind=8) :: n_dim_i8, i

    n_dim_i4 = 5
    n_dim_i8 = 5_8
    allocate(id_mat_r4(n_dim_i4, n_dim_i4))
    allocate(id_mat_r8(n_dim_i8, n_dim_i8))
    allocate(id_mat_i4(n_dim_i4, n_dim_i4))
    allocate(id_mat_i8(n_dim_i8, n_dim_i8))

    print*, "============================================================="
    print*, "real(kind=4)"
    id_mat_r4 = 0.0
    call identity(id_mat_r4, n_dim_i4)
    do i=1, n_dim_i4, 1
        print*, id_mat_r4(i,:)
    end do

    print*, "============================================================="
    print*, "real(kind=8)"
    id_mat_r8 = 0.0
    call identity(id_mat_r8, n_dim_i8)
    do i=1, n_dim_i8, 1
        print*, id_mat_r8(i,:)
    end do

    print*, "============================================================="
    print*, "integer(kind=4)"
    id_mat_i4 = 0.0
    call identity(id_mat_i4, n_dim_i4)
    do i=1, n_dim_i4, 1
        print*, id_mat_i4(i,:)
    end do

    print*, "============================================================="
    print*, "integer(kind=8)"
    id_mat_i8 = 0.0
    call identity(id_mat_i8, n_dim_i8)
    do i=1, n_dim_i8, 1
        print*, id_mat_i8(i,:)
    end do



end program main_identity
