program main_sum_of_matrix
    use mod_const
    use mod_common
    use mod_timer
    use mod_stats
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    integer(kind=8) :: n_samples, n_columns
    real(kind=8), allocatable :: matrix(:,:), vector(:)    


    print*, '============================================================='
    n_samples = 1000000
    n_columns = 100
    print*, "SHAPE", n_samples, n_columns
    allocate(matrix(n_samples, n_columns))
    allocate(vector(n_columns))


    print*, '============================================================='
    print*, 'Randomization'
    call random_number(matrix)


    print*, '============================================================='
    print*, "NAIVE"
    call date_and_time(values=date_value1)
    vector = sum(matrix, dim=1)
    call date_and_time(values=date_value2)
    ! print*, vector
    print*, time_diff(date_value1, date_value2)


    print*, '============================================================='
    print*, "My Func (NO ADVANTAGE...)"
    call date_and_time(values=date_value1)
    call sum_of_matrix(vector, matrix, n_samples, n_columns, 1_8)
    call date_and_time(values=date_value2)
    ! print*, vector
    print*, time_diff(date_value1, date_value2)


end program main_sum_of_matrix
