program main_neighbour_balltree
    use mod_nearest_neighbour
    use mod_balltree
    implicit none

    integer(kind=8) :: n_samples_x, n_samples_q
    integer(kind=8) :: n_columns

    real(kind=8), allocatable :: x(:,:), q(:,:)
    type(balltree) :: bt, bt2
    type(neighbor_results)   :: res, res2

    n_samples_x = 1000
    n_samples_q = 10
    n_columns   = 10

    allocate(x(n_samples_x, n_columns))
    allocate(q(n_samples_q, n_columns))

    call random_number(x)
    call random_number(q)


    bt = balltree()
    print*, "    build start."
    call bt%build(x)
    print*, "    build done."
    

    print*, "    dump start."
    call bt%dump(file_name="bt.bin")
    print*, "    dump done."


    print*, "    load start."
    call bt2%load(file_name="bt.bin")
    print*, "    load done."

    
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    res = bt%query(q, radius=.8d0)
    res2 = bt2%query(q, radius=.8d0)
    print*, res%distances(1)%dst(:)
    print*, res2%distances(1)%dst(:)

    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    res = bt%query(q, n_neighbors=10_8)
    res2 = bt2%query(q, n_neighbors=10_8)
    print*, res%distances(1)%dst(:)
    print*, res2%distances(1)%dst(:)

end program main_neighbour_balltree