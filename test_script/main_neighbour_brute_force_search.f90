program main_neighbour_brute_force_search
    use mod_nearest_neighbour
    use mod_brute_force_search
    implicit none

    integer(kind=8) :: n_samples_x, n_samples_q
    integer(kind=8) :: n_columns

    real(kind=8), allocatable :: x(:,:), q(:,:)
    type(brute_force_search) :: bfsearch, bfsearch2
    type(neighbor_results)   :: res, res2

    n_samples_x = 1000
    n_samples_q = 10
    n_columns   = 10

    allocate(x(n_samples_x, n_columns))
    allocate(q(n_samples_q, n_columns))

    call random_number(x)
    call random_number(q)


    bfsearch = brute_force_search()
    call bfsearch%build(x)
    call bfsearch%dump(file_name="bfsearch.bin")
    call bfsearch2%load(file_name="bfsearch.bin")

    
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    res = bfsearch%query(q, radius=.7d0)
    res2 = bfsearch2%query(q, radius=.7d0)
    print*, res%distances(1)%dst(:)
    print*, res2%distances(1)%dst(:)

    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    res = bfsearch%query(q, n_neighbors=10_8)
    res2 = bfsearch2%query(q, n_neighbors=10_8)
    print*, res%distances(1)%dst(:)
    print*, res2%distances(1)%dst(:)

end program main_neighbour_brute_force_search