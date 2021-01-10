program main_random_rotation
    use mod_linalg, only: random_rotation
    implicit none

    integer(kind=8)           :: n_dim
    real(kind=8), allocatable :: rotation_matrix(:,:)
    integer(kind=8)           :: i
    real(kind=8), allocatable :: id_mat(:,:)

    n_dim = 5
    allocate(rotation_matrix(n_dim, n_dim))
    allocate(id_mat(n_dim, n_dim))

    print*, '============================================================='
    call random_rotation(rotation_matrix, n_dim)
    do i=1, n_dim, 1
        print*, rotation_matrix(i,:)
    end do

    print*, '============================================================='
    print*, "CHECK: transpose(rotation_matrix) x rotation_matrix = I"
    id_mat = matmul(transpose(rotation_matrix), rotation_matrix)
    do i=1, n_dim, 1
        print*, id_mat(i,:)
    end do

    print*, '============================================================='
    print*, "CHECK: rotation_matrix x transpose(rotation_matrix) = I"
    id_mat = matmul(rotation_matrix, transpose(rotation_matrix))
    do i=1, n_dim, 1
        print*, id_mat(i,:)
    end do

end program main_random_rotation

