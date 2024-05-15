program main
    use mod_random
    use mod_csr
    use mod_dense2csr
    use mod_csr2dense
    implicit none
    
    type(csr_matrix) :: csr_a
    type(variable) :: var_a, var_b, var_c
    integer(kind=8) :: n_samples, n_columns
    real(kind=8), allocatable :: a(:,:), b(:,:), a_rec(:,:), a_rec2(:,:)
    real(kind=8), allocatable :: vals(:)

    call init_stack()

    call fix_random_seed(999_8)
    n_samples = 128
    n_columns = 128

    allocate(a(n_samples, n_columns), b(n_samples, n_columns))
    allocate(a_rec(n_samples, n_columns))
    allocate(a_rec2(n_samples, n_columns))
    call random_number(a)
    a = 1d0
    b = a
    csr_a = dense2csr_weighted_sampling_mat(a, 6_8, dim=2_8, start_index=1_8, negative_weights="filter")

    print*, 1
    var_a = variable(a)
    print*, 2
    var_b = dense2csr_(var_a, top_k=16_8, how="filter")
    print*, 3
    var_c = csr2dense_(var_b)

    call var_c%backward()

contains


        

end program main