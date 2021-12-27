program main
    use mod_nearest_neighbour
    use mod_lsh
    use mod_timer
    use mod_math
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)
    type(kdtree)         :: tree
    type(kdtree_results) :: results_kdtree, results_lsh

    real(kind=8), allocatable :: mat(:,:)

    type(lsh) :: lshnns
    real(kind=8), ALLOCATABLE, target :: x(:,:), q(:,:)
    integer(kind=8) :: i, n_hash_functions

    allocate(x(100000,127))
    allocate(q(10,127))

    call random_number(x)
    x = 10d0 * x - 5d0

    call random_number(q)
    q = 10d0 * q - 5d0

    lshnns = lsh(n_hash_functions=16_8, n_hash_tables=32_8)

    call date_and_time(values=date_value1)
    call lshnns%fit_lsh_matmul(x)
    call date_and_time(values=date_value2)
    print*, "LSH, Build: ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    results_lsh = lshnns%query_lsh_nearest_neighbor_random_projection_matmul(q)
    call date_and_time(values=date_value2)
    print*, "LSH, Query: ", time_diff(date_value1, date_value2)


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
        print*, results_kdtree%indices(i)%idx, results_kdtree%distances(i)%dst, " :: ", &
            results_lsh%indices(i)%idx, results_lsh%distances(i)%dst, &
            abs(results_lsh%distances(i)%dst - results_kdtree%distances(i)%dst) / &
                results_kdtree%distances(i)%dst * 100
    end do

end program main
