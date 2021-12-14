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
    
    real(kind=8), ALLOCATABLE :: x(:,:), x_sq_sum(:), dist(:,:)
    real(kind=8), ALLOCATABLE :: y(:,:), y_sq_sum(:)
    real(kind=8), ALLOCATABLE :: y_t(:,:)
    real(kind=8), ALLOCATABLE :: dist_fully(:,:)
    integer(kind=8), ALLOCATABLE :: indices(:,:), indices_2(:,:), indices_true(:,:)

    integer(kind=8) :: n_rows_x, n_rows_y, n_cols, i, j, iter, n_iter, count_eq, flg

    integer(kind=8) :: idx, fid
    real(kind=8) :: val, diff

    CHARACTER(len=256) :: file_name_x_train_csv, file_name_x_train_bin
    CHARACTER(len=256) :: file_name_y_train_csv, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_d_train_csv, file_name_d_train_bin
    CHARACTER(len=256) :: file_name_i_train_csv, file_name_i_train_bin

    file_name_x_train_csv = "../../../uci_data/dummy_data_for_kdtree_X_100000x10.csv"
    file_name_x_train_bin = "../../../uci_data/dummy_data_for_kdtree_X_100000x10.bin"
    file_name_y_train_csv = "../../../uci_data/dummy_data_for_kdtree_Y_200x10.csv"
    file_name_y_train_bin = "../../../uci_data/dummy_data_for_kdtree_Y_200x10.bin"

    file_name_d_train_csv = "../../../uci_data/dummy_data_for_kdtree_D.csv"
    file_name_d_train_bin = "../../../uci_data/dummy_data_for_kdtree_D.bin"
    file_name_i_train_csv = "../../../uci_data/dummy_data_for_kdtree_I.csv"
    file_name_i_train_bin = "../../../uci_data/dummy_data_for_kdtree_I.bin"

    n_rows_x = 100000
    n_rows_y = 200
    n_cols = 10
    n_iter = 500
    n_iter = 1

    ! call date_and_time(values=date_value1)
    ! allocate(x(n_rows_x, n_cols), x_sq_sum(n_rows_x))
    ! allocate(y(n_rows_y, n_cols), y_sq_sum(n_rows_y))
    allocate(y_t(n_cols, n_rows_y))
    allocate(dist_fully(n_rows_x, n_rows_y))
    ! call date_and_time(values=date_value2)
    ! print*, "allocate", time_diff(date_value1, date_value2)

    ! call date_and_time(values=date_value1)
    ! call RANDOM_NUMBER(x)
    ! call RANDOM_NUMBER(y)
    ! call date_and_time(values=date_value2)
    ! print*, "RANDOM_NUMBER", time_diff(date_value1, date_value2)

    ! call date_and_time(values=date_value1)
    ! call matrix_sqsum_row(x, x_sq_sum, n_rows_x, n_cols, parallel=f_)
    ! call matrix_sqsum_row(y, y_sq_sum, n_rows_y, n_cols, parallel=f_)
    ! call RANDOM_NUMBER(y)
    ! call date_and_time(values=date_value2)
    ! print*, "SQ_SUM", time_diff(date_value1, date_value2)

    ! x = 200d0*x-200d0*.5d0
    ! y = 200d0*y-200d0*.5d0
    ! y_t = transpose(y)

    ! print*, '============================================================='
    ! print*, "CSV to Binary"
    ! call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
    !     n_rows_x, n_cols, f_, "r", "r")
    ! call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
    !     n_rows_y, n_cols, f_, "r", "r")
    ! call read2bin_2d(file_name_d_train_csv, file_name_d_train_bin, &
    !     n_rows_y, 1_8, f_, "r", "r")
    ! call read2bin_2d(file_name_i_train_csv, file_name_i_train_bin, &
    !     n_rows_y, 1_8, f_, "i", "i")

    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train"
    call read_bin_2d(file_name_x_train_bin, x)
    call read_bin_2d(file_name_y_train_bin, y)
    call read_bin_2d(file_name_d_train_bin, dist)
    call read_bin_2d(file_name_i_train_bin, indices_true)
    call ifdealloc(x_sq_sum)
    call ifdealloc(y_sq_sum)
    allocate(x_sq_sum(n_rows_x), y_sq_sum(n_rows_y))
    call matrix_sqsum_row(x, x_sq_sum, n_rows_x, n_cols, parallel=f_)
    call matrix_sqsum_row(y, y_sq_sum, n_rows_y, n_cols, parallel=f_)

    y_t = transpose(y)

    ! tree_pointer = kdtree(min_samples_in_leaf=10_8)
    ! call date_and_time(values=date_value1)
    ! call tree_pointer%build(x)
    ! call date_and_time(values=date_value2)
    ! print*, "BuildTree: ", time_diff(date_value1, date_value2)

    print*, "Data Shape: x: ", shape(x)
    print*, "Data Shape: y: ", shape(y)

    tree_pointer = kdtree(min_samples_in_leaf=128_8)
    call date_and_time(values=date_value1)
    call tree_pointer%build(x)
    call date_and_time(values=date_value2)
    print*, "BuildTree:                  ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    do iter=1, n_iter
        results = tree_pointer%query(y, iter, n_iter, n_neighbors=2_8)
    end do
    call date_and_time(values=date_value2)
    print*, "NearestNeighborSearch, n_neighbors=2: ", time_diff(date_value1, date_value2)/dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter
        results = tree_pointer%query(y, iter, n_iter, n_neighbors=4_8)
    end do
    call date_and_time(values=date_value2)
    print*, "NearestNeighborSearch, n_neighbors=4: ", time_diff(date_value1, date_value2)/dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter
        results = tree_pointer%query(y, iter, n_iter, n_neighbors=8_8)
    end do
    call date_and_time(values=date_value2)
    print*, "NearestNeighborSearch, n_neighbors=8: ", time_diff(date_value1, date_value2)/dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter
        results = tree_pointer%query(y, iter, n_iter, n_neighbors=16_8)
    end do
    call date_and_time(values=date_value2)
    print*, "NearestNeighborSearch, n_neighbors=16: ", time_diff(date_value1, date_value2)/dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter
        results = tree_pointer%query(y, iter, n_iter, n_neighbors=32_8)
    end do
    call date_and_time(values=date_value2)
    print*, "NearestNeighborSearch, n_neighbors=32: ", time_diff(date_value1, date_value2)/dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter
        results = tree_pointer%query(y, iter, n_iter, n_neighbors=1_8)
    end do
    call date_and_time(values=date_value2)
    print*, "NearestNeighborSearch, n_neighbors=1: ", time_diff(date_value1, date_value2)/dble(n_iter)
    print*, "NearestNeighborSearch, n_neighbors=32: ", time_diff(date_value1, date_value2)/dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter
        results = tree_pointer%query(y, iter, n_iter, n_neighbors=128_8)
    end do
    call date_and_time(values=date_value2)
    print*, "NearestNeighborSearch, n_neighbors=128: ", time_diff(date_value1, date_value2)/dble(n_iter)

    call date_and_time(values=date_value1)
    do iter=1, n_iter
        results = tree_pointer%query(y, iter, n_iter, n_neighbors=10000_8)
    end do
    call date_and_time(values=date_value2)
    print*, "NearestNeighborSearch, n_neighbors=10000: ", time_diff(date_value1, date_value2)/dble(n_iter)

    print*, size(results%distances(1)%dst)


    ! call date_and_time(values=date_value1)
    ! do iter=1, n_iter
    !     results = tree_pointer%query(y, iter, n_iter, n_neighbors=10_8)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "NearestNeighborSearch: ", time_diff(date_value1, date_value2)/dble(n_iter)
    ! stop

    call date_and_time(values=date_value1)
    dist_fully(:,:) = spread(x_sq_sum, dim=2, ncopies=n_rows_y) & 
                    + spread(y_sq_sum, dim=1, ncopies=n_rows_x)
    call dgemm("N", "N", & 
        n_rows_x, n_rows_y, n_cols, &
        -2d0, & 
        x, n_rows_x, &
        y_t, n_cols, &
        1d0, &
        dist_fully, n_rows_x)
    call date_and_time(values=date_value2)
    print*, "BruteForce: ",  time_diff(date_value1, date_value2)

    dist_fully = sqrt(dist_fully)

    count_eq = 0
    do i=1, n_rows_y, 1
        diff = abs(results%distances(i)%dst(1) - dist(i,1))
        ! print*, results%distances(i)%dst(1:5), dist(i,1), diff / dist(i,1) * 100d0
        flg = diff / dist(i,1) * 100d0 .le. 10d-8
        count_eq = count_eq + flg
    end do
    print*, "COUNT_EQ", count_eq

end program main
