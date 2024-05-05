program main
    !$ use omp_lib
    use mod_sparse_dgemm
    use mod_random
    use mod_sort
    use mod_timer
    use mod_csr
    implicit none
    
    integer(kind=8)        :: date_value1(8), date_value2(8)
    integer(kind=8) :: n, m, k, l, s
    integer(kind=8) :: i, start, final
    integer(kind=8) :: iter, max_iter
    real(kind=8), allocatable :: a(:,:), b(:,:), c(:,:), vals(:)
    real(kind=8), allocatable :: b_t(:,:), c_t(:,:)
    integer(kind=8), allocatable :: rows(:), cols(:), idxs(:)
    type(csr_matrix) :: csr_F, csr_C, csr_a
    real(kind=8) :: sum_a
    real(kind=8), allocatable :: sum_a_rows(:), sum_a_cols(:)

    real(kind=8), allocatable :: r(:,:)

    max_iter = 10000
    ! max_iter = 1000
    ! max_iter = 1

    m = 16
    k = 32
    l = 32
    s = 8
    n = s * m

    ! m = 128
    ! k = 256
    ! l = 256
    ! s = 8
    ! n = s * m

    csr_F = csr_matrix(n_columns=k)
    csr_C = csr_matrix(n_columns=k, start_index=0_8)

    ! m = 8
    ! k = 16
    ! l = 16
    ! s = 10
    ! n = s * m

    ! Matrix
    allocate(a(m,k))
    allocate(b(k,l), b_t(l,k))
    allocate(c(m,l), c_t(l,m))

    ! COO
    allocate(rows(n))
    allocate(cols(n))
    allocate(vals(n))
    allocate(idxs(k))

    call fix_random_seed(100_8)
    call random_number(a)
    call random_number(b)
    ! a = 1
    ! b = 1

    b_t = transpose(b)
    c_t = transpose(c)

    idxs = (/(i, i=1, k, 1)/)
    do i=1, n, 1
        rows(i) = mod(i,m)+1
    end do
    call quick_sort(rows, n)
    start = 1
    final = start + s - 1
    do i=1, m, 1
        call permutation(idxs, k)
        cols(start:final) = idxs(1:s)
        call quick_sort(cols(start:final), s)

        call csr_F%insert(cols(start:final), a(i, cols(start:final)))
        call csr_C%insert(cols(start:final), a(i, cols(start:final)))

        start = final + 1
        final = start + s - 1
    end do

    do i=1, n, 1
        vals(i) = a(rows(i), cols(i))
    end do
    a = 0d0
    do i=1, n, 1
        a(rows(i), cols(i)) = vals(i)
    end do

    print*, "Offset C: ", csr_C%offset, csr_C%start_index
    print*, csr_C%rows(:10)
    r = csr_C%to_dense()
    print*, count(a == r), size(a), size(r)
    print*, sum(a-r), sum(a), sum(r)

    r = csr_F%to_dense()
    print*, count(a == r), size(a), size(r)
    print*, sum(a-r), sum(a), sum(r)

    csr_a = dense2csr_weighted_sampling(a, s, dim=2_8, start_index=0_8)
    print*, csr_a%rows - csr_C%rows
    print*, csr_a%cols - csr_C%cols
    print*, csr_a%vals - csr_C%vals
    print*, csr_a%n_rows, csr_C%n_rows
    print*, csr_a%n_cols, csr_C%n_cols

end program main