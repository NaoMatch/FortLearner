program main
    use mod_hash
    use mod_timer
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(kind=8)              :: n_samples
    real(kind=8), ALLOCATABLE    :: vec_r(:)
    integer(kind=8), ALLOCATABLE :: vec_i(:)
    integer(kind=8)              :: hash
    integer(kind=8)              :: iter, n_iter

    n_samples = 100
    n_iter= 1000000
    allocate(vec_r(n_samples))
    allocate(vec_i(n_samples))

    call RANDOM_NUMBER(vec_r)

    vec_i = n_samples * vec_r

    print*, vec_i

    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        hash = one_at_a_time_hash(vec_i, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, time_diff(date_value1, date_value2) / dble(n_iter)
    print*, hash

    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        hash = one_at_a_time_hash(vec_i(n_samples:1:-1), n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, time_diff(date_value1, date_value2) / dble(n_iter)
    print*, hash

    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        hash = one_at_a_time_hash(12_8)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2) / dble(n_iter)
    print*, hash

end program main
