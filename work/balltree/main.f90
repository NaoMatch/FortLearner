program main
    use mod_timer
    use mod_nearest_neighbour, only: kdtree
    use mod_balltree, only: balltree
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)

    type(balltree) :: btree
    type(kdtree)   :: ktree

    integer(kind=8) :: n_samples, n_columns
    real(kind=8), allocatable :: x(:,:)

    n_samples = 100000
    n_columns = 5
    allocate(x(n_samples, n_columns))
    call RANDOM_NUMBER(x)

    btree = balltree(min_samples_in_leaf=128_8)
    call date_and_time(values=date_value1)
    call btree%build(x)
    call date_and_time(values=date_value2)
    print*, "btree%build(x): ", time_diff(date_value1, date_value2)

    ktree = kdtree(min_samples_in_leaf=128_8)
    call date_and_time(values=date_value1)
    call ktree%build(x)
    call date_and_time(values=date_value2)
    print*, "ktree%build(x): ", time_diff(date_value1, date_value2)

end program main
