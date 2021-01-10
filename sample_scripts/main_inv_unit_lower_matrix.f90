program main_inv_unit_lower_matrix
    use mod_linalg, only: inv_unit_lower_matrix, cholesky_decomposition_modified
    implicit none

    integer(kind=8) :: n_dim
    real(kind=8), ALLOCATABLE :: matrix(:,:), matrix_lower(:,:)
    real(kind=8), ALLOCATABLE :: matrix_lower_inv(:,:)
    real(kind=8), ALLOCATABLE :: diagonal(:)
    real(kind=8), ALLOCATABLE :: id_mat(:,:)
    integer(kind=8) :: i

    n_dim = 5
    allocate(matrix(n_dim, n_dim))
    allocate(matrix_lower(n_dim, n_dim))
    allocate(matrix_lower_inv(n_dim, n_dim))
    allocate(diagonal(n_dim))
    allocate(id_mat(n_dim, n_dim))

    print*, '============================================================='
    print*, "Randomization"
    call random_number(matrix)

    print*, '============================================================='
    print*, "matrix = matmul(TRANSPOSE(matrix), matrix)"
    matrix = matmul(TRANSPOSE(matrix), matrix)
    do i=1, n_dim, 1
        print*, matrix(i,:)
    end do
    
    print*, '============================================================='
    print*, "cholesky_decomposition_modified"
    call cholesky_decomposition_modified(matrix_lower, diagonal, matrix, n_dim)
    print*, "diagonal matrix            :: matrix_lower"
    do i=1, n_dim, 1
        print*, diagonal(i), " :: ", matrix_lower(i,:)
    end do

    print*, '============================================================='
    print*, "Inversion of matrix_lower (unit lower matrix) "
    call inv_unit_lower_matrix(matrix_lower_inv, matrix_lower, n_dim)
    do i=1, n_dim, 1
        print*, matrix_lower_inv(i,:)
    end do

    print*, '============================================================='
    print*, "CHECK: inversion(matrix_lower) x matrix_lower = Identity"
    id_mat = MATMUL(matrix_lower_inv, matrix_lower)
    do i=1, n_dim, 1
        print*, id_mat(i,:)
    end do

    print*, '============================================================='
    print*, "CHECK: matrix_lower x inversion(matrix_lower)  = Identity"
    id_mat = MATMUL(matrix_lower, matrix_lower_inv)
    do i=1, n_dim, 1
        print*, id_mat(i,:)
    end do


end program main_inv_unit_lower_matrix
