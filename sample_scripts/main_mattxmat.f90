program main_mattxmat
    use mod_const
    use mod_timer
    use mod_linalg, only: mattxmat
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    integer(kind=8)           :: n_samples, n_columns
    real(kind=8), allocatable :: matrix(:,:), matrix_t(:,:)
    real(kind=8), allocatable :: mat_sq(:,:)
    integer(kind=8)           :: i

    n_samples = 1000000
    n_columns = 5

    print*, '============================================================='
    print*, "Matrix shape: ", n_samples, n_columns
    allocate(matrix(n_samples, n_columns))
    allocate(matrix_t(n_columns, n_samples))
    allocate(mat_sq(n_columns, n_columns))

    print*, '============================================================='
    print*, "Randomization"
    call random_number(matrix)
    matrix_t = transpose(matrix)

    print*, '============================================================='
    print*, "matmul(transpose(matrix), matrix)"
    call date_and_time(values=date_value1)
    mat_sq = matmul(transpose(matrix), matrix)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    do i=1, n_columns, 1
        print*, mat_sq(i,:)
    end do

    print*, '============================================================='
    print*, "matmul(matrix_t, matrix)"
    call date_and_time(values=date_value1)
    mat_sq = matmul(matrix_t, matrix)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    do i=1, n_columns, 1
        print*, mat_sq(i,:)
    end do
    
    print*, '============================================================='
    print*, "mattxmat(matrix_t, matrix)"
    call date_and_time(values=date_value1)
    call mattxmat(mat_sq, matrix, n_samples, n_columns, with_intercept=f_)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    do i=1, n_columns, 1
        print*, mat_sq(i,:)
    end do

end program main_mattxmat
