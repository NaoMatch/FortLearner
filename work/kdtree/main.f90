program main
    !$ use omp_lib
    use mod_timer
    use mod_linalg
    use mod_nearest_neighbour
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(kind=8), allocatable :: time1(:), time2(:)
    type(kdtree) :: tree, tree_pointer
    type(kdtree_results) :: results
    
    real(kind=8), ALLOCATABLE, target :: x(:,:), x_sq_sum(:), dist(:,:)
    real(kind=8), ALLOCATABLE, target :: y(:,:), y_sq_sum(:)
    real(kind=8), ALLOCATABLE, target :: y_t(:,:)
    real(kind=8), ALLOCATABLE, target :: dist_fully(:,:), nearest_dist(:)
    integer(kind=8), ALLOCATABLE :: indices(:,:), indices_2(:,:), indices_true(:,:)

    integer(kind=8) :: n_rows_x, n_rows_y, n_cols, i, j, n, iter, n_iter, count_eq, flg, n_neighbors, pow_
    integer(kind=8) :: count_dup

    integer(kind=8) :: idx, fid
    real(kind=8) :: val, diff

    n_rows_x = 1000000
    n_cols = 32_8
    allocate(x(n_rows_x, n_cols))
    call RANDOM_NUMBER(x)
    x = int(10d0*x-10d0*.5d0)
    tree_pointer = kdtree(min_samples_in_leaf=128_8)

    call date_and_time(values=date_value1)
    call tree_pointer%build(x)
    call date_and_time(values=date_value2)
    print*, "BuildTree:                  ", time_diff(date_value1, date_value2)

    do i=0, 3, 1
        n_rows_y = 10**i
        call date_and_time(values=date_value1)
        ! results = tree_pointer%query(x(1:n_rows_y,:), radius=0d0)
        results = tree_pointer%query(x(1:n_rows_y,:), n_neighbors=3_8)
        call date_and_time(values=date_value2)

        count_dup = 0
        do n=1, n_rows_y, 1
            ! if ( size(results%indices(n)%idx) .ge. 1_8 ) then
            !     count_dup = count_dup + 1
            ! end if
            print*, n, size(results%indices(n)%idx), results%distances(n)%dst(:)
        end do

        print*, "QueryTree:                  ", n_rows_y, count_dup, time_diff(date_value1, date_value2)
    end do




    ! do n_cols=5, 100, 5
    !     n_rows_x = 100000
    !     n_rows_y = 200
    !     n_iter = 500 / n_cols

    !     allocate(x(n_rows_x, n_cols))
    !     allocate(y(n_rows_y, n_cols))

    !     call RANDOM_NUMBER(x)
    !     call RANDOM_NUMBER(y)

    !     x = 200d0*x-200d0*.5d0
    !     y = 200d0*y-200d0*.5d0

    !     tree_pointer = kdtree(min_samples_in_leaf=128_8)
    !     call date_and_time(values=date_value1)
    !     do iter=1, n_iter
    !         call tree_pointer%build(x)
    !     end do
    !     call date_and_time(values=date_value2)
    !     print*, "BuildTree:                  ", time_diff(date_value1, date_value2) / dble(n_iter)

    !     do pow_=0, 5, 1
    !         n_neighbors = 2**pow_
    !         call date_and_time(values=date_value1)
    !         do iter=1, n_iter
    !             results = tree_pointer%query(y, n_neighbors=n_neighbors)
    !         end do
    !         call date_and_time(values=date_value2)
    !         print*, "NearestNeighborSearch", n_cols, n_neighbors, time_diff(date_value1, date_value2)/dble(n_iter)
    !     end do

    !     deallocate(x)
    !     deallocate(y)
    ! end do

end program main
