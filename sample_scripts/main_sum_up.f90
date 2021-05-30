program main_sum_up_left
    use mod_const
    use mod_common
    use mod_timer
    use mod_stats
    implicit none

    integer(kind=8) :: iter, max_iter
    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(kind=8) :: n_samples, n_columns
    real(kind=8), allocatable :: matrix(:,:), vector_r8(:)
    integer(kind=8), allocatable :: vector_i8(:)
    real(kind=8) :: sumval_naive, sumval_branch
    real(kind=8) :: threshold_r8
    integer(kind=8) :: threshold_i8

    max_iter = 100
    n_samples = 20000000
    allocate(vector_r8(n_samples))
    allocate(vector_i8(n_samples))
    call RANDOM_NUMBER(vector_r8)
    vector_r8 = int(vector_r8 * 10d0, kind=8)
    vector_i8 = int(vector_r8, kind=8)

    print*, '============================================================='
    print*, "NAIVE"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_naive = sum(vector_r8)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_naive

    print*, '============================================================='
    print*, "Optimized Fortran"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_branch = sum_up_f(vector_r8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch

    print*, '============================================================='
    print*, "with Assembler"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_branch = sum_up(vector_r8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch


    print*, '============================================================='
    print*, "NAIVE"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_naive = sum(vector_i8)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_naive

    print*, '============================================================='
    print*, "Optimized Fortran"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_branch = sum_up_f(vector_i8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch

    print*, '============================================================='
    print*, "with Assembler"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_branch = sum_up(vector_i8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch

end program main_sum_up_left
