program main_groupby
    use mod_stats, only: groupby_sum, groupby_sq_sum, groupby_count
    implicit none
    
    integer(kind=8)              :: n_samples
    real(kind=8), allocatable    :: vector_x(:), vector_y(:)
    real(kind=8), allocatable    :: uniq_x(:), stat_x(:), stat_y(:)
    integer(kind=8), allocatable :: count_x(:)

    n_samples = 10
    allocate(vector_x(n_samples))
    allocate(vector_y(n_samples))
    allocate(count_x(n_samples))

    call random_number(vector_x)
    call random_number(vector_y)

    vector_x = int(vector_x*3_8, kind=8)

    print*, '============================================================='
    print*, "Get the unique value of vector_x"
    print*, " and sum vector y for each unique value of vector_x."
    print*, "vector_x:  ", vector_x
    print*, "vector_y:  ", vector_y
    call groupby_sum(uniq_x, vector_x, stat_y, vector_y, n_samples)
    print*, "uniq_of_x: ", uniq_x
    print*, "sum_of_y:  ", stat_y 

    print*, '============================================================='
    print*, "Get the unique value of vector_x"
    print*, " and sum for each unique value."
    print*, "vector_x:  ", vector_x
    print*, "vector_y:  ", vector_y
    call groupby_sum(uniq_x, stat_x, vector_x, n_samples)
    print*, "uniq_of_x: ", uniq_x
    print*, "sum_of_y:  ", stat_x 

    print*, '============================================================='
    print*, "Get the unique value of vector_x"
    print*, " and square sum vector y for each unique value of vector_x."
    print*, "vector_x:     ", vector_x
    print*, "vector_y:     ", vector_y
    call groupby_sq_sum(uniq_x, vector_x, stat_y, vector_y, n_samples)
    print*, "uniq_of_x:    ", uniq_x
    print*, "sq_sum_of_y:  ", stat_y 

    print*, '============================================================='
    print*, "Get the unique value of vector_x"
    print*, " and count each unique value."
    print*, "vector_x:     ", vector_x
    print*, "vector_y:     ", vector_y
    call groupby_count(uniq_x, count_x, vector_x, n_samples)
    print*, "uniq_of_x:    ", uniq_x
    print*, "count_of_y:   ", count_x 

end program main_groupby
