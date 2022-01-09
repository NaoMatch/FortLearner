program main
    use mod_timer
    use mod_brute_force_search
    use mod_nearest_neighbour, only: kdtree, kdtree_results
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    type(brute_force_search) :: bfsearch
    type(kdtree)   :: ktree
    type(kdtree_results) :: res_bfsearch, res_ktree

    integer(kind=8) :: n_samples_x, n_samples_q, n_columns, n
    real(kind=8), allocatable :: x(:,:), q(:,:)

    n_samples_x = 100000
    n_samples_q = 100
    n_columns = 100
    allocate(x(n_samples_x, n_columns))
    allocate(q(n_samples_q, n_columns))
    call RANDOM_NUMBER(x)
    call RANDOM_NUMBER(q)

    bfsearch = brute_force_search()
    call date_and_time(values=date_value1)
    call bfsearch%build(x)
    call date_and_time(values=date_value2)
    print*, "bfsearch%build(x):                    ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    res_bfsearch = bfsearch%query(q, n_neighbors=1_8)
    call date_and_time(values=date_value2)
    print*, "bfsearch%query(q, n_neighbors=1_8):   ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    res_bfsearch = bfsearch%query(q, n_neighbors=10_8)
    call date_and_time(values=date_value2)
    print*, "bfsearch%query(q, n_neighbors=10_8):  ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    res_bfsearch = bfsearch%query(q, n_neighbors=50_8)
    call date_and_time(values=date_value2)
    print*, "bfsearch%query(q, n_neighbors=50_8):  ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    res_bfsearch = bfsearch%query(q, radius=4d0)
    call date_and_time(values=date_value2)
    print*, "bfsearch%query(q, radius=4d0):  ", time_diff(date_value1, date_value2)


    ktree = kdtree(min_samples_in_leaf=128_8)
    call date_and_time(values=date_value1)
    call ktree%build(x)
    call date_and_time(values=date_value2)
    print*, "ktree%build(x):                       ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    res_ktree = ktree%query(q, n_neighbors=1_8)
    call date_and_time(values=date_value2)
    print*, "ktree%query(q, n_neighbors=1_8):      ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    res_ktree = ktree%query(q, n_neighbors=10_8)
    call date_and_time(values=date_value2)
    print*, "ktree%query(q, n_neighbors=10_8):     ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    res_ktree = ktree%query(q, n_neighbors=50_8)
    call date_and_time(values=date_value2)
    print*, "ktree%query(q, n_neighbors=50_8):     ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    res_ktree = ktree%query(q, radius=4d0)
    call date_and_time(values=date_value2)
    print*, "ktree%query(q, radius=4d0):     ", time_diff(date_value1, date_value2)

    do n=1, 10, 1
        print*, "k: ", res_ktree%distances(n)%dst(1:3), "b: ", res_bfsearch%distances(n)%dst(1:3)
    end do



end program main
