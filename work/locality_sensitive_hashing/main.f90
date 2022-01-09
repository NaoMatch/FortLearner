program main
    use mod_nearest_neighbour, only: neighbor_results
    use mod_kdtree
    use mod_balltree
    use mod_lsh
    use mod_timer
    use mod_math
    implicit none

    integer(kind=8)        :: date_value1(8), date_value2(8)
    type(neighbor_results) :: results_kdtree, results_btree, results_lshrp, results_lshrpp
    type(kdtree)           :: tree
    type(balltree)         :: btree
    type(lsh)              :: lshrp, lshrpp

    real(kind=8), allocatable :: mat(:,:)

    real(kind=8), ALLOCATABLE, target :: x(:,:), q(:,:)
    integer(kind=8) :: i, n_hash_functions

    allocate(x(100000,100))
    allocate(q(100, 100))

    call random_number(x)
    ! x = 100d0 * x - 50d0

    call random_number(q)
    ! q = 100d0 * q - 50d0
    ! q = x(1:100,:)

    print*, ""
    print*, '============================================================='
    print*, 'kdtree ------------------------------------------------------'
    print*, '============================================================='
    tree = kdtree(min_samples_in_leaf=128_8)
    call date_and_time(values=date_value1)
    call tree%build(x)
    call date_and_time(values=date_value2)
    print*, "KDTree, Build: ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    results_kdtree = tree%query(q, n_neighbors=100_8)
    call date_and_time(values=date_value2)
    print*, "KDTree, Query: ", time_diff(date_value1, date_value2)

    print*, ""
    print*, '============================================================='
    print*, 'balltree ------------------------------------------------------'
    print*, '============================================================='
    btree = balltree(min_samples_in_leaf=128_8)
    call date_and_time(values=date_value1)
    call btree%build(x)
    call date_and_time(values=date_value2)
    print*, "BallTree, Build: ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    results_btree = btree%query(q, n_neighbors=100_8)
    call date_and_time(values=date_value2)
    print*, "BallTree, Query: ", time_diff(date_value1, date_value2)

    print*, ""
    print*, '============================================================='
    print*, 'lsh - random projection -------------------------------------'
    print*, '============================================================='
    lshrp = lsh(n_hash_functions=31_8, n_hash_tables=63_8, algorithm="random_projection")
    call date_and_time(values=date_value1)
    call lshrp%fit(x)
    call date_and_time(values=date_value2)
    print*, "LSH, Build: ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    results_lshrp = lshrp%query(q, n_neighbors=100_8)
    call date_and_time(values=date_value2)
    print*, "LSH, Query: ", time_diff(date_value1, date_value2)


    print*, ""
    print*, '============================================================='
    print*, 'lsh - random projection, p-stable ---------------------------'
    print*, '============================================================='
    lshrpp = lsh(n_hash_functions=16_8, n_hash_tables=128_8, bit_length=16_8, algorithm="random_projection_pstable")
    call date_and_time(values=date_value1)
    call lshrpp%fit(x)
    call date_and_time(values=date_value2)
    print*, "LSH, Build: ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    results_lshrpp = lshrpp%query(q, n_neighbors=100_8)
    call date_and_time(values=date_value2)
    print*, "LSH, Query: ", time_diff(date_value1, date_value2)


    do i=1, 10, 1
        print*, &
            int(results_kdtree%indices(i)%idx(1:2)), real(results_kdtree%distances(i)%dst(1:2)), " :: ", &
            int(results_btree%indices(i)%idx(1:2)), real(results_kdtree%distances(i)%dst(1:2)), " :: ", &
            int(results_lshrp%indices(i)%idx(1:2)),  real(results_lshrp%distances(i)%dst(1:2)), " :: ", &
            int(results_lshrpp%indices(i)%idx(1:2)),  real(results_lshrpp%distances(i)%dst(1:2))
    end do

end program main
