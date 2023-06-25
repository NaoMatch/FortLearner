program main_neighbour_product_quantization
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
    type(neighbor_indices) :: res_pq
    type(data_holder) :: dholder

    file_name_x = "../sample_data/make_regression_X_train_0000100000x00100.bin"
    call read_bin_2d(file_name_x, x)

    allocate(q(100, 100))
    call random_number(q)
    do i=1, 100, 1
        q(i,:) = (maxval(x, dim=1) - minval(x, dim=1)) * q(i,:) + minval(x, dim=1)
    end do

    n_neighbors = 10
    n_neighbors_max = minval([20_8, n_neighbors])
    n = size(x, dim=1)
    n_small = 1000
    allocate(indices(n), indices_small(n_small))
    do i=1, n, 1
        indices(i) = i
    end do
    call permutation(indices, n)
    indices_small(:) = indices(1:n_small)
    x_small = x(indices_small,:)    
    print*, "x:       ", shape(x)
    print*, "x_small: ", shape(x_small)

    call date_and_time(values=date_value1)
    pq = product_quantization(n_subspace=4_8, n_clusters=256_8)    
    call pq%fit(x_small)
    call date_and_time(values=date_value2)
    print*, "fit_pq: ", time_diff(date_value1, date_value2)
    
    call date_and_time(values=date_value1)
    pq = product_quantization(n_subspace=4_8, n_clusters=256_8)    
    dholder = data_holder(x_small)
    call pq%fit(dholder)
    call date_and_time(values=date_value2)
    print*, "fit_pq: ", time_diff(date_value1, date_value2)
    

    call date_and_time(values=date_value1)
    call pq%encode(x)
    call date_and_time(values=date_value2)
    print*, "encode_pq: ", time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    res_pq = pq%query(q, n_neighbors=n_neighbors)
    call date_and_time(values=date_value2)
    print*, "search_pq: ", time_diff(date_value1, date_value2)
    
    call date_and_time(values=date_value1)
    bf = brute_force_search()
    call bf%build(x)
    call date_and_time(values=date_value2)
    print*, "bf_build: ", time_diff(date_value1, date_value2)
    
    call date_and_time(values=date_value1)
    res_bf = bf%query(q, n_neighbors=n_neighbors)
    call date_and_time(values=date_value2)
    print*, "bf_query: ", time_diff(date_value1, date_value2)
    do nnn=1, 100, 1
        print*, '*********************************************************************************************'
        cnt_dup = count_dup(res_pq%indices(nnn)%idx, res_bf%indices(nnn)%idx, n_neighbors)
        print*, nnn, int(res_pq%indices(nnn)%idx(:n_neighbors_max))
        print*, nnn, int(res_bf%indices(nnn)%idx(:n_neighbors_max))
        print*, "         - ", cnt_dup
    end do

contains
    function count_dup(vec1, vec2, n_samples)
        implicit none
        integer(kind=8) :: count_dup
        integer(kind=8), intent(in) :: vec1(n_samples)
        integer(kind=8), intent(in) :: vec2(n_samples)
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8) :: n, val1

        count_dup = 0
        do n=1, n_samples, 1
            val1 = vec1(n)
            if (any(val1 == vec2)) then
                count_dup = count_dup + 1
            end if
        end do
    end function
    
end program main_neighbour_product_quantization