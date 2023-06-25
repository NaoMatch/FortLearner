program main_neighbour_lsh
    use mod_const
    use mod_common
    use mod_random
    use mod_product_quantization
    use mod_timer
    use mod_nearest_neighbour
    use mod_kdtree
    use mod_balltree
    use mod_brute_force_search
    use mod_nearest_neighbour
    use mod_data_holder
    use mod_lsh
    implicit none

    integer(kind=8)        :: date_value1(8), date_value2(8)

    character(len=256)           :: file_name_x
    real(kind=8), allocatable    :: x(:,:), x_small(:,:), dists(:,:), q(:,:)

    integer(kind=8), allocatable :: indices(:), indices_small(:), x_enc(:,:)
    integer(kind=8)              :: n, n_small, i, nnn, n_neighbors, n_neighbors_max, cnt_dup, l

    type(neighbor_results) :: res_bf, res_kd, res_bt
    type(balltree) :: btree
    type(kdtree) :: kdt
    type(brute_force_search) :: bf
    type(product_quantization) :: pq
    type(neighbor_results) :: res_lsh
    type(data_holder) :: dholder
    type(lsh) :: lsh_

    file_name_x = "../sample_data/make_regression_X_train_0000100000x00100.bin"
    call read_bin_2d(file_name_x, x)
    dholder = data_holder(x)

    allocate(q(100, 100))
    call random_number(q)
    do i=1, 100, 1
        q(i,:) = (maxval(x, dim=1) - minval(x, dim=1)) * q(i,:) + minval(x, dim=1)
    end do


    lsh_ = lsh()
    call lsh_%build(x)
    call lsh_%build(dholder)
    res_lsh = lsh_%query(q, n_neighbors=10_8)

end program main_neighbour_lsh