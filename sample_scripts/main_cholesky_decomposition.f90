program main_cholesky_decomposition
    use mod_common, only: identity
    use mod_linalg, only: cholesky_decomposition, cholesky_decomposition_modified
    implicit none
    
    integer(kind=8) :: n_dim
    real(kind=8), ALLOCATABLE :: matrix(:,:), matrix_lower(:,:)
    real(kind=8), ALLOCATABLE :: diagonal(:)
    real(kind=8), ALLOCATABLE :: id_mat(:,:)
    integer(kind=8) :: i

    n_dim = 5
    allocate(matrix(n_dim, n_dim))
    allocate(matrix_lower(n_dim, n_dim))
    allocate(diagonal(n_dim))
    allocate(id_mat(n_dim, n_dim))

    print*, '============================================================='
    print*, "Randomization"
    call random_number(matrix)
    call identity(id_mat, n_dim)

    print*, '============================================================='
    print*, "matrix = matmul(TRANSPOSE(matrix), matrix)"
    matrix = matmul(TRANSPOSE(matrix), matrix)
    do i=1, n_dim, 1
        print*, matrix(i,:)
    end do

    print*, '============================================================='
    print*, "cholesky_decomposition"
    call cholesky_decomposition(matrix_lower, matrix, n_dim)
    do i=1, n_dim, 1
        print*, matrix_lower(i,:)
    end do


    print*, '============================================================='
    print*, "CHECK: MATMUL(matrix_lower, TRANSPOSE(matrix_lower)) = matrix"
    matrix_lower = MATMUL(matrix_lower, TRANSPOSE(matrix_lower))
    do i=1, n_dim, 1
        print*, matrix_lower(i,:)
    end do

    print*, '============================================================='
    print*, "cholesky_decomposition_modified"
    call cholesky_decomposition_modified(matrix_lower, diagonal, matrix, n_dim)
    print*, "diagonal matrix            :: matrix_lower"
    do i=1, n_dim, 1
        print*, diagonal(i), " :: ", matrix_lower(i,:)
    end do

    print*, '============================================================='
    print*, "CHECK: MATMUL(matmul(matrix_lower, id_mat), TRANSPOSE(matrix_lower)) = matrix"
    do i=1, n_dim, 1
        id_mat(i,i) = diagonal(i)
    end do
    matrix_lower = MATMUL(matmul(matrix_lower, id_mat), TRANSPOSE(matrix_lower))
    do i=1, n_dim, 1
        print*, matrix_lower(i,:)
    end do


end program main_cholesky_decomposition
