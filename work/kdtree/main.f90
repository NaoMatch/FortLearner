program main
    use mod_timer
    use mod_linalg
    use mod_nearest_neighbour
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    type(kdtree) :: tree, tree_pointer
    
    real(kind=8), ALLOCATABLE :: x(:,:), x_sq_sum(:)
    real(kind=8), ALLOCATABLE :: y(:,:), y_sq_sum(:)
    real(kind=8), ALLOCATABLE :: dist_fully(:,:)
    integer(kind=8), ALLOCATABLE :: indices(:,:), indices_2(:,:)

    integer(kind=8) :: n_rows_x, n_rows_y, n_cols, i, j, iter, n_iter, count_eq, flg
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_x_train_bin
    CHARACTER(len=256) :: file_name_y_train_csv, file_name_y_train_bin

    file_name_x_train_csv = "../../../uci_data/dummy_data_for_kdtree_X.csv"
    file_name_x_train_bin = "../../../uci_data/dummy_data_for_kdtree_X.bin"
    file_name_y_train_csv = "../../../uci_data/dummy_data_for_kdtree_Y.csv"
    file_name_y_train_bin = "../../../uci_data/dummy_data_for_kdtree_Y.bin"

    n_rows_x = 1000
    n_cols = 10

    ! python 100000 
    ! build: 50[msec]
    ! nearest: 764[sec]
    n_rows_x = 10000
    n_rows_y = 2000
    n_cols = 10
    n_iter = 10

    call date_and_time(values=date_value1)
    allocate(x(n_rows_x, n_cols), x_sq_sum(n_rows_x))
    allocate(y(n_rows_y, n_cols), y_sq_sum(n_rows_y))
    allocate(dist_fully(n_rows_x, n_rows_y))
    call date_and_time(values=date_value2)
    print*, "allocate", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    call RANDOM_NUMBER(x)
    call RANDOM_NUMBER(y)
    call date_and_time(values=date_value2)
    print*, "RANDOM_NUMBER", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    call matrix_sqsum_row(x, x_sq_sum, n_rows_x, n_cols, parallel=f_)
    call matrix_sqsum_row(y, y_sq_sum, n_rows_y, n_cols, parallel=f_)
    call RANDOM_NUMBER(y)
    call date_and_time(values=date_value2)
    print*, "SQ_SUM", time_diff(date_value1, date_value2)

    x = 200d0*x-200d0*.5d0
    y = 200d0*y-200d0*.5d0

    ! print*, '============================================================='
    ! print*, "CSV to Binary"
    ! print*, "    x"
    ! call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
    !     n_rows_x, n_cols, f_, "r", "r")
    ! call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
    !     n_rows_y, n_cols, f_, "r", "r")

    ! print*, '============================================================='
    ! print*, "Read Binary"
    ! print*, "    x_train"
    ! call read_bin_2d(file_name_x_train_bin, x)
    ! call read_bin_2d(file_name_y_train_bin, y)
    ! call matrix_sqsum_row(x, x_sq_sum, n_rows_x, n_cols, parallel=f_)
    ! call matrix_sqsum_row(y, y_sq_sum, n_rows_y, n_cols, parallel=f_)

    ! tree_pointer = kdtree(min_samples_in_leaf=10_8)
    ! call date_and_time(values=date_value1)
    ! call tree_pointer%build(x)
    ! call date_and_time(values=date_value2)
    ! print*, "BuildTree: ", time_diff(date_value1, date_value2)

    print*, "Data Shape: x: ", shape(x)
    print*, "Data Shape: y: ", shape(y)

    tree_pointer = kdtree(min_samples_in_leaf=10_8)
    call date_and_time(values=date_value1)
    do iter=1, n_iter
        call tree_pointer%build_ver02(x)
    end do
    call date_and_time(values=date_value2)
    print*, "BuildTree:                  ", time_diff(date_value1, date_value2)/dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter
        indices = tree_pointer%query_nearest(y)
    end do
    call date_and_time(values=date_value2)
    print*, "NaiveNearestNeighborSearch: ", time_diff(date_value1, date_value2)/dble(n_iter)

    ! call date_and_time(values=date_value1)
    ! do iter=1, n_iter
    !     indices = tree_pointer%query_nearest(y)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "NaiveNearestNeighborSearch_Iter: ", time_diff(date_value1, date_value2)/dble(n_iter)

    call date_and_time(values=date_value1)
    ! dist_fully(:,:) = spread(x_sq_sum, dim=2, ncopies=n_rows_y) & 
    !                 + spread(y_sq_sum, dim=1, ncopies=n_rows_x)
    ! call dgemm("N", "T", & 
    !     n_rows_x, n_cols, n_rows_y, &
    !     -2d0, & 
    !     x, n_rows_x, &
    !     y, n_cols, &
    !     1d0, &
    !     dist_fully, n_rows_x)
    do j=1, n_rows_y, 1
        do i=1, n_rows_x, 1
            dist_fully(i,j) = sum( (x(i,:)-y(j,:))**2d0 )
            ! dist_fully(i,j) = x_sq_sum(i) + y_sq_sum(j) - 2d0*sum( (x(i,:)*y(j,:)) )
            ! dist_fully(i,j) = dist_fully(i,j) - 2d0*sum( (x(i,:)*y(j,:)) )
        end do
    end do

    allocate(indices_2(n_rows_y,1))
    indices_2(:,1) = minloc(dist_fully, dim=1)
    call date_and_time(values=date_value2)
    print*, "BruteForce: ",  time_diff(date_value1, date_value2)

    count_eq = 0
    do i=1, n_rows_y, 1
        flg = sqrt(sum((x(indices(i,1),:)-y(i,:))**2d0)) .eq. sqrt(sum((x(indices_2(i,1),:)-y(i,:))**2d0))
        count_eq = count_eq + 1
    end do
    print*, "COUNT_EQ", count_eq

    ! do i=1, n_rows_y, 1
    !     print*, indices(i,1), " :: ", indices_2(i,1), " ::::::: ", &
    !         sqrt(sum((x(indices(i,1),:)-y(i,:))**2d0)), & 
    !         sqrt(sum((x(indices_2(i,1),:)-y(i,:))**2d0)), &
    !         sqrt(sum((x(8599,:)-y(i,:))**2d0)), &
    !         sqrt(sum((x(indices(i,1),:)-y(i,:))**2d0)) <= sqrt(sum((x(indices_2(i,1),:)-y(i,:))**2d0))
    ! end do

end program main
