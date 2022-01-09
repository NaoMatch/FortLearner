program main
    use mod_timer
    use mod_nearest_neighbour, only: neighbor_results
    use mod_kdtree, only: kdtree
    use mod_balltree, only: balltree
    use mod_brute_force_search
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)

    type(neighbor_results) :: res_kd, res_bl, res_bf

    type(balltree) :: btree0, btree1, btree2
    type(kdtree)   :: ktree
    type(brute_force_search)   :: bfsearch

    integer(kind=8) :: n_samples, n_columns, n_samples_q, n
    integer(kind=8) :: n_iter, iter
    real(kind=8), allocatable :: x(:,:), q(:,:)

    n_iter = 10

    n_samples = 100000
    n_samples_q = 100
    n_columns = 40
    allocate(x(n_samples, n_columns))
    allocate(q(n_samples_q, n_columns))
    call RANDOM_NUMBER(x)
    call RANDOM_NUMBER(q)

    btree0 = balltree(min_samples_in_leaf=128_8, split_algo="most_spread")
    call date_and_time(values=date_value1)
    call btree0%build(x)
    call date_and_time(values=date_value2)
    print*, "btree0%build(x):    ", time_diff(date_value1, date_value2)

    btree1 = balltree(min_samples_in_leaf=128_8, split_algo="farthest_two")
    call date_and_time(values=date_value1)
    call btree1%build(x)
    call date_and_time(values=date_value2)
    print*, "btree1%build(x):    ", time_diff(date_value1, date_value2)

    ktree = kdtree(min_samples_in_leaf=128_8)
    call date_and_time(values=date_value1)
    call ktree%build(x)
    call date_and_time(values=date_value2)
    print*, "ktree%build(x):    ", time_diff(date_value1, date_value2)

    bfsearch = brute_force_search()
    call date_and_time(values=date_value1)
    call bfsearch%build(x)
    call date_and_time(values=date_value2)
    print*, "bfsearch%build(x): ", time_diff(date_value1, date_value2)





    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        res_bl = btree0%query(q, n_neighbors=16_8)
    end do
    call date_and_time(values=date_value2)
    print*, "btree0%query(q, n_neighbors=16_8):       ", time_diff(date_value1, date_value2) / dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        res_bl = btree1%query(q, n_neighbors=16_8)
    end do
    call date_and_time(values=date_value2)
    print*, "btree1%query(q, n_neighbors=16_8):       ", time_diff(date_value1, date_value2) / dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        res_kd = ktree%query(q, n_neighbors=16_8)
    end do
    call date_and_time(values=date_value2)
    print*, "ktree%query(q, n_neighbors=16_8):       ", time_diff(date_value1, date_value2) / dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        res_bf = bfsearch%query(q, n_neighbors=16_8)
    end do
    call date_and_time(values=date_value2)
    print*, "bfsearch%query(q, n_neighbors=16_8):    ", time_diff(date_value1, date_value2) / dble(n_iter)




    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        res_bl = btree0%query(q, radius=1d0)
    end do
    call date_and_time(values=date_value2)
    print*, "btree0%query(q, radius=1d0):       ", time_diff(date_value1, date_value2) / dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        res_bl = btree1%query(q, radius=1d0)
    end do
    call date_and_time(values=date_value2)
    print*, "btree1%query(q, radius=1d0):       ", time_diff(date_value1, date_value2) / dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        res_kd = ktree%query(q, radius=1d0)
    end do
    call date_and_time(values=date_value2)
    print*, "ktree%query(q, radius=1d0):       ", time_diff(date_value1, date_value2) / dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter, 1
        res_bf = bfsearch%query(q, radius=1d0)
    end do
    call date_and_time(values=date_value2)
    print*, "bfsearch%query(q, radius=1d0):    ", time_diff(date_value1, date_value2) / dble(n_iter)

    ! do n=1, n_samples_q, 1
    !     print*, n, real(res_bl%distances(n)%dst(1:2)), real(res_kd%distances(n)%dst(1:2)), real(res_bf%distances(n)%dst(1:2))
    ! end do
end program main
