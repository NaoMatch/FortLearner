program main
    use mod_timer
    use mod_csr
    use mod_common
    use mod_random
    use mod_common_type
    implicit none
    
    integer(kind=8)        :: date_value1(8), date_value2(8)
    integer(kind=8) :: n_samples, n_columns, n_top, l, s
    real(kind=8), allocatable :: mat(:,:)
    type(csr_matrix) :: csr_mat
    type(csr_matrix), allocatable :: csr_mats(:)
    type(jagged_vector_i8), allocatable :: indices(:)

    ! n_top = 2_8
    ! n_samples = 10
    ! n_columns = 4

    n_top = 32_8
    n_samples = 256
    n_columns = 128


    call fix_random_seed(42_8)
    allocate(mat(n_samples, n_columns))
    call random_number(mat)

    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    do l=1, 1000, 1
        csr_mat = dense2csr_weighted_sampling_mat(mat, n_top, dim=1_8, start_index=0_8)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

    print*, "Ver1"
    print*, sum(mat)
    print*, csr_mat%n_rows, csr_mat%n_cols
    print*, sum(csr_mat%rows)
    print*, sum(csr_mat%cols)
    print*, sum(csr_mat%vals)

    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    do l=1, 1000, 1
        csr_mat = csr_matrix(n_samples, start_index=0_8)
        allocate(indices(size(mat, dim=2)))
        !!$omp parallel private(mat, indices, s) shared(n_top) num_threads(4)
        do s=1, size(mat, dim=2), 1
            indices(s)%vector = dense2csr_weighted_sampling_vec(mat(:,s), n_top, start_index=0_8)
        end do
        !!$omp end parallel
        do s=1, size(mat, dim=2), 1
            ! print*, indices(s)%vector
            call csr_mat%insert(indices(s)%vector, mat(indices(s)%vector,s))
        end do
        deallocate(indices)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

    print*, csr_mat%n_rows, csr_mat%n_cols
    print*, sum(csr_mat%rows)
    print*, sum(csr_mat%cols)
    print*, sum(csr_mat%vals)

    call release_random_seed()    


end program main