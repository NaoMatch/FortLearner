program main
    use mod_timer
    use mod_const
    use mod_error
    use mod_common
    use mod_common_type
    use mod_random
    use mod_sort
    use mod_hash_table
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    type(hash_table) :: htable
    type(index_sets) :: idx_sets

    integer(kind=8) :: n_samples, n_columns, n, counter
    real(kind=8), ALLOCATABLE    :: x_r8(:,:)
    integer(kind=8), ALLOCATABLE :: x_i8(:,:), y_i8(:,:)
    integer(kind=8), ALLOCATABLE :: m(:), o(:),m2(:,:), o2(:,:)
    integer(kind=8), ALLOCATABLE :: indices(:)
    integer(kind=8) :: n_idx, f, i, j, idx, cnt, iter, n_iter, u

    n_iter = 20
    n_samples = 1000000
    n_columns = 100
    allocate(x_r8(n_samples, n_columns))
    allocate(x_i8(n_samples, n_columns))

    call random_number(x_r8)
    x_i8 = 10*x_r8

    htable = hash_table()

    call date_and_time(values=date_value1)
    call htable%create_table(x_i8)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

    indices = htable%search(x_i8(2,:))
    print*, indices

    call date_and_time(values=date_value1)
    idx_sets = htable%search_all(x_i8)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

end program main
