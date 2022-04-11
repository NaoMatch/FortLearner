program main_neighbour_kdtree
    use mod_nearest_neighbour
    use mod_kdtree
    implicit none

    integer(kind=8) :: n_samples_x, n_samples_q
    integer(kind=8) :: n_columns

    real(kind=8), allocatable :: x(:,:), q(:,:)
    type(kdtree) :: ktree, ktree2
    type(neighbor_results)   :: res, res2

    n_samples_x = 1000
    n_samples_q = 10
    n_columns   = 10

    allocate(x(n_samples_x, n_columns))
    allocate(q(n_samples_q, n_columns))

    call random_number(x)
    call random_number(q)


    ktree = kdtree()
    print*, "    build start."
    call ktree%build(x)
    print*, "    build done."
    

    print*, "    dump start."
    call ktree%dump(file_name="ktree.bin")
    print*, "    dump done."


    print*, "    load start."
    call ktree2%load(file_name="ktree.bin")
    print*, "    load done."

    
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    res = ktree%query(q, radius=.8d0)
    res2 = ktree2%query(q, radius=.8d0)
    print*, res%distances(1)%dst(:)
    print*, res2%distances(1)%dst(:)

    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    res = ktree%query(q, n_neighbors=10_8)
    res2 = ktree2%query(q, n_neighbors=10_8)
    print*, res%distances(1)%dst(:)
    print*, res2%distances(1)%dst(:)

end program main_neighbour_kdtree