program main_multi_mat_vec
    use mod_timer
    use mod_linalg, only: multi_mat_vec
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(kind=8)           :: n_samples, n_columns
    real(kind=8), allocatable :: vector_in(:), vector_out(:)
    real(kind=8), allocatable :: matrix(:,:)

    n_samples = 1000000
    n_columns = 50
    
    print*, '============================================================='
    print*, "Shape: matrix x vector_in = vector_out"
    print*, "    matrix:     ", n_samples, n_columns
    print*, "    vector_in:  ", n_columns
    print*, "    vector_out: ", n_samples
    allocate(matrix(n_samples, n_columns))
    allocate(vector_in(n_columns))
    allocate(vector_out(n_samples))

    print*, '============================================================='
    print*, "Randomization"
    call random_number(matrix)
    call random_number(vector_in)

    print*, '============================================================='
    print*, "matmul"
    call date_and_time(values=date_value1)
    vector_out = matmul(matrix, vector_in)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), "[msec]"

    print*, '============================================================='
    print*, "My Subroutine"
    call date_and_time(values=date_value1)
    call multi_mat_vec(matrix, vector_in, vector_out, n_samples, n_columns)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), "[msec]"

end program main_multi_mat_vec
