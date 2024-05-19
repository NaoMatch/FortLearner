program main
    use mod_random
    use mod_common
    use mod_sort
    use mod_timer
    use mod_random
    use mod_csr
    use mod_fixed_size_csr
    implicit none
    
    integer(kind=8)        :: date_value1(8), date_value2(8)
    integer(kind=8) :: n_samples, n_columns
    real(kind=8), allocatable :: a(:,:), a_t(:,:)
    type(fixed_size_csr_matrix) :: fcsr
    type(csr_matrix) :: csr

    integer(kind=8) :: n_iter, n, time_p, time_w, time_i


    call fix_random_seed(99_8)

    n_iter = 1000000

    n_samples = 10000

    n_iter = n_iter / n_samples

    n_columns = 128
    allocate(a(n_samples, n_columns))
    allocate(a_t(n_columns, n_samples))
    call random_number(a)
    print*, sum(a)
    a_t = transpose(a)

    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    do n=1, n_iter, 1
        fcsr = dense2fcsr_weighted_sampling_mat(&
            a, 32_8, dim=2_8, start_index=0_8, negative_weights="filter", n_jobs=1_8)
        ! fcsr = dense2fcsr_weighted_sampling_mat(a_t, 32_8, dim=1_8, start_index=0_8, negative_weights="filter")
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2) / dble(n_iter)

    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    do n=1, n_iter, 1
        fcsr = dense2fcsr_weighted_sampling_mat(&
            a, 32_8, dim=2_8, start_index=0_8, negative_weights="filter", n_jobs=2_8)
        ! fcsr = dense2fcsr_weighted_sampling_mat(a_t, 32_8, dim=1_8, start_index=0_8, negative_weights="filter")
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2) / dble(n_iter), 2

    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    do n=1, n_iter, 1
        fcsr = dense2fcsr_weighted_sampling_mat(&
            a, 32_8, dim=2_8, start_index=0_8, negative_weights="filter", n_jobs=4_8)
        ! fcsr = dense2fcsr_weighted_sampling_mat(a_t, 32_8, dim=1_8, start_index=0_8, negative_weights="filter")
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2) / dble(n_iter), 4


    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    do n=1, n_iter, 1
        fcsr = dense2fcsr_weighted_sampling_mat(&
            a, 32_8, dim=2_8, start_index=0_8, negative_weights="filter", n_jobs=8_8)
        ! fcsr = dense2fcsr_weighted_sampling_mat(a_t, 32_8, dim=1_8, start_index=0_8, negative_weights="filter")
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2) / dble(n_iter), 8


    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    do n=1, 1, 1
        csr = dense2csr_weighted_sampling_mat(a, 32_8, dim=2_8, start_index=0_8, negative_weights="filter")
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2) / dble(1)

    ! print*, '*********************************************************************************************'
    ! print*, fcsr%rows(:)
    ! print*, csr%rows(:)

    ! print*, '*********************************************************************************************'
    ! print*, fcsr%cols(:)
    ! print*, csr%cols(:)

    ! print*, '*********************************************************************************************'
    ! print*, sum(fcsr%vals(:))
    ! print*, sum(csr%vals(:))



end program main