program main
    use mod_nearest_neighbour
    use mod_lsh
    use mod_timer
    use mod_math
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)
    type(kdtree)         :: tree
    type(kdtree_results) :: results_kdtree, results_lshrp, results_lshrpp

    real(kind=8), allocatable :: mat(:,:)

    type(lsh) :: lshrp, lshrpp
    real(kind=8), ALLOCATABLE, target :: x(:,:), q(:,:)
    integer(kind=8) :: i, n_hash_functions

    allocate(x(100000,50))
    allocate(q(100, 50))

    call random_number(x)
    x = 100d0 * x - 50d0

    call random_number(q)
    q = 100d0 * q - 50d0

    lshrp = lsh(n_hash_functions=12_8, n_hash_tables=31_8, algorithm="random_projection")
    call date_and_time(values=date_value1)
    call lshrp%fit(x)
    call date_and_time(values=date_value2)
    print*, "LSH, Build: ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    results_lshrp = lshrp%query_lsh_nearest_neighbor_random_projection(q)
    call date_and_time(values=date_value2)
    print*, "LSH, Query: ", time_diff(date_value1, date_value2)

    lshrpp = lsh(n_hash_functions=12_8, n_hash_tables=31_8, bit_length=256_8, algorithm="random_projection_pstable")
    call date_and_time(values=date_value1)
    call lshrpp%fit(x)
    call date_and_time(values=date_value2)
    print*, "LSH, Build: ", time_diff(date_value1, date_value2)
    stop

    ! call date_and_time(values=date_value1)
    ! results_lshrpp = lshrpp%query_lsh_nearest_neighbor_random_projection_pstable(q)
    ! call date_and_time(values=date_value2)
    ! print*, "LSH, Query: ", time_diff(date_value1, date_value2)


    tree = kdtree(min_samples_in_leaf=128_8)
    call date_and_time(values=date_value1)
    call tree%build(x)
    call date_and_time(values=date_value2)
    print*, "KDTree, Build: ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    results_kdtree = tree%query(q, n_neighbors=1_8)
    call date_and_time(values=date_value2)
    print*, "KDTree, Query: ", time_diff(date_value1, date_value2)

    mat = matmul(x, transpose(q))
    do i=1, 10, 1
        print*, &
            results_kdtree%indices(i)%idx, results_kdtree%distances(i)%dst, " :: ", &
            results_lshrp%indices(i)%idx,  results_lshrp%distances(i)%dst, " :: ", &
            results_lshrpp%indices(i)%idx,  results_lshrpp%distances(i)%dst
    end do

end program main
