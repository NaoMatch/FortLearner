program main
    !$ use omp_lib
    use mod_sparse_dgemm_implementation
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
    type(csr_matrix) :: csr_a
    type(csr_matrix_for_c) :: cc
    real(kind=8) :: sum_a
    real(kind=8), allocatable :: sum_a_rows(:), sum_a_cols(:)
    csr_a = csr_matrix()
    cc = csr_matrix_for_c()

    max_iter = 10000
    ! max_iter = 1000
    ! max_iter = 1

    m = 128
    k = 256
    l = 256
    s = 8
    n = s * m

    ! m = 8
    ! k = 16
    ! l = 16
    ! s = 10
    ! n = s * m

    ! Matrix
    allocate(a(m,k))
    allocate(b(k,l))
    allocate(c(m,l))

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

        call csr_a%insert(cols(start:final), a(i, cols(start:final)))
        call cc%insert(cols(start:final), a(i, cols(start:final)))

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






    print*, '*********************************************************************************************'
    c = 0d0
    CALL OMP_SET_NUM_THREADS(1)
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call dgemm("N", "N", m, k, l, 1d0, a, m, b, k, 0d0, c, m) 
    end do
    call date_and_time(values=date_value2)
    print*, "dgemm:    ", sum(c), sum(a), time_diff(date_value1, date_value2)
    
    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     c = matmul(a,b)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "matmul: ", sum(c), sum(a), time_diff(date_value1, date_value2)
    
    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver0(c, csr_a, b, m, k, l)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver0:  ", sum(c), sum(a), time_diff(date_value1, date_value2)
    
    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver1(c, csr_a, b, m, k, l)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver1:  ", sum(c), sum(a), time_diff(date_value1, date_value2)
    
    print*, '*********************************************************************************************'
    c = 0d0
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call spmm_ver2(c, csr_a, b_t, m, k, l)
    end do
    call date_and_time(values=date_value2)
    print*, "my_ver2:  ", sum(c), sum(a), time_diff(date_value1, date_value2)
    
    print*, '*********************************************************************************************'
    c = 0d0
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call spmm_ver3(c_t, csr_a, b_t, m, k, l)
    end do
    call date_and_time(values=date_value2)
    print*, "my_ver3:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
        "Remove Second Loop + Transpose B + Transpose C"
    
    print*, '*********************************************************************************************'
    c = 0d0
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call spmm_ver4(c_t, csr_a, b_t, m, k, l)
    end do
    call date_and_time(values=date_value2)
    print*, "my_ver4:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
        "Remove Second Loop + Transpose B + Transpose C + OpenMP"
    
    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver5(c_t, csr_a, b_t, m, k, l)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver5:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2)
    
    print*, '*********************************************************************************************'
    c_t = 0d0
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call spmm_ver6(c_t, csr_a, b_t, m, k, l)
    end do
    call date_and_time(values=date_value2)
    print*, "my_ver6:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
        "Remove Second Loop + Transpose B + Transpose C + SIMD"
    
    print*, '*********************************************************************************************'
    c_t = 0d0
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call spmm_ver7(c_t, cc, b_t, m, k, l)
    end do
    call date_and_time(values=date_value2)
    print*, "my_ver7:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
        "All SIMD (not so optimized)"
    
    print*, '*********************************************************************************************'
    c_t = 0d0
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call spmm_ver8(c_t, cc, b_t, m, k, l, 8_8)
    end do
    call date_and_time(values=date_value2)
    print*, "my_ver8:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
        "All SIMD + OpenMP (not so optimized)"



contains

    subroutine spmm_ver0(c, csr_a, b, m, k, l)
        ! 最も単純な実装
        implicit none
        real(kind=8), intent(inout)  :: c(m,l)
        type(csr_matrix), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b(k,l)
        integer(kind=8), intent(in)  :: m, k, l

        integer(kind=8) :: i, j, n
        integer(kind=8) :: idx
        real(kind=8) :: val, tmp

        type(row_elems), pointer :: r_ptr

        ! c = 0d0
        do i=1, csr_a%n_rows, 1
            r_ptr => csr_a%cols(i)
            do j=1, l, 1
                tmp = 0d0
                do n=1, size(r_ptr%vals), 1
                    idx = r_ptr%cols(n)
                    val = r_ptr%vals(n)
                    tmp = tmp + b(idx, j) * val
                end do
                c(i,j) = tmp
            end do
        end do
    end subroutine spmm_ver0

    subroutine spmm_ver1(c, csr_a, b, m, k, l)
        ! 2番目のループを排除
        implicit none
        real(kind=8), intent(inout)  :: c(m,l)
        type(csr_matrix), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b(k,l)
        integer(kind=8), intent(in)  :: m, k, l

        integer(kind=8) :: i, j, n
        integer(kind=8) :: idx
        real(kind=8) :: val

        real(kind=8), allocatable :: tmp(:)
        type(row_elems), pointer :: r_ptr

        ! c = 0d0
        allocate(tmp(l))
        do i=1, csr_a%n_rows, 1
            r_ptr => csr_a%cols(i)
            tmp = 0d0
            do n=1, size(r_ptr%vals), 1
                idx = r_ptr%cols(n)
                val = r_ptr%vals(n)
                tmp = tmp + b(idx,:) * val
            end do
            c(i,:) = tmp
        end do
    end subroutine spmm_ver1

    subroutine spmm_ver2(c, csr_a, b_t, m, k, l)
        ! 2番目のループを排除 + bを転置
        implicit none
        real(kind=8), intent(inout)  :: c(m,l)
        type(csr_matrix), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b_t(l,k)
        integer(kind=8), intent(in)  :: m, k, l

        integer(kind=8) :: i, j, n
        integer(kind=8) :: idx
        real(kind=8) :: val

        real(kind=8), allocatable :: tmp(:)
        type(row_elems), pointer :: r_ptr

        ! c = 0d0
        allocate(tmp(l))
        do i=1, csr_a%n_rows, 1
            r_ptr => csr_a%cols(i)
            tmp = 0d0
            do n=1, size(r_ptr%vals), 1
                idx = r_ptr%cols(n)
                val = r_ptr%vals(n)
                tmp = tmp + b_t(:,idx) * val
            end do
            c(i,:) = tmp
        end do
    end subroutine spmm_ver2

    subroutine spmm_ver3(c_t, csr_a, b_t, m, k, l)
        ! 2番目のループを排除 + bを転置 + cを転置
        implicit none
        real(kind=8), intent(inout)  :: c_t(l,m)
        type(csr_matrix), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b_t(l,k)
        integer(kind=8), intent(in)  :: m, k, l

        integer(kind=8) :: i, j, n
        integer(kind=8) :: idx
        real(kind=8) :: val

        real(kind=8), allocatable :: tmp(:)
        type(row_elems), pointer :: r_ptr

        ! c = 0d0
        allocate(tmp(l))
        do i=1, csr_a%n_rows, 1
            r_ptr => csr_a%cols(i)
            tmp = 0d0
            do n=1, size(r_ptr%vals), 1
                idx = r_ptr%cols(n)
                val = r_ptr%vals(n)
                tmp = tmp + b_t(:,idx) * val
            end do
            c_t(:,i) = tmp
        end do
    end subroutine spmm_ver3

    subroutine spmm_ver4(c_t, csr_a, b_t, m, k, l)
        ! 2番目のループを排除 + bを転置 + cを転置 + OpenMP
        implicit none
        real(kind=8), intent(inout)  :: c_t(l,m)
        type(csr_matrix), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b_t(l,k)
        integer(kind=8), intent(in)  :: m, k, l

        integer(kind=8) :: i, j, n
        integer(kind=8) :: idx
        real(kind=8) :: val

        real(kind=8), allocatable :: tmp(:)
        type(row_elems), pointer :: r_ptr

        allocate(tmp(l))
        !$omp parallel num_threads(8)
        !$omp do private(i, r_ptr, tmp, n, idx, val)
        do i=1, csr_a%n_rows, 1
            r_ptr => csr_a%cols(i)
            tmp = 0d0
            do n=1, size(r_ptr%vals), 1
                idx = r_ptr%cols(n)
                val = r_ptr%vals(n)
                tmp = tmp + b_t(:,idx) * val
            end do
            c_t(:,i) = tmp
        end do
        !$omp end do
        !$omp end parallel
    end subroutine spmm_ver4

    subroutine spmm_ver5(c_t, csr_a, b_t, m, k, l)
        ! 2番目のループを排除 + bを転置 + cを転置 + OpenMP
        implicit none
        real(kind=8), intent(inout)  :: c_t(l,m)
        type(csr_matrix), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b_t(l,k)
        integer(kind=8), intent(in)  :: m, k, l

        integer(kind=8) :: i, j, n
        integer(kind=8) :: idx
        real(kind=8) :: val

        real(kind=8), allocatable :: tmp(:)
        type(row_elems), pointer :: r_ptr

        allocate(tmp(l))
        do i=1, csr_a%n_rows, 1
            r_ptr => csr_a%cols(i)
            tmp = 0d0
            !$omp parallel num_threads(4)
            !$omp do private(n, idx, val) reduction(+:tmp)
            do n=1, size(r_ptr%vals), 1
                idx = r_ptr%cols(n)
                val = r_ptr%vals(n)
                tmp = tmp + b_t(:,idx) * val
            end do
            !$omp end do
            !$omp end parallel
            c_t(:,i) = tmp
        end do
    end subroutine spmm_ver5

    subroutine spmm_ver6(c_t, csr_a, b_t, m, k, l)
        ! 2番目のループを排除 + bを転置 + cを転置 + SIMD
        implicit none
        real(kind=8), intent(inout)  :: c_t(l,m)
        type(csr_matrix), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b_t(l,k)
        integer(kind=8), intent(in)  :: m, k, l

        integer(kind=8) :: i, j, n
        integer(kind=8) :: idx
        real(kind=8) :: val

        real(kind=8), allocatable :: tmp(:)
        type(row_elems), pointer :: r_ptr

        allocate(tmp(l))
        do i=1, csr_a%n_rows, 1
            r_ptr => csr_a%cols(i)
            tmp = 0d0
            n = size(r_ptr%vals)
            call sparse_mv_ver0(tmp, r_ptr%cols, r_ptr%vals, b_t, n, m, k, l)
            c_t(:,i) = tmp
        end do
    end subroutine spmm_ver6

    subroutine spmm_ver7(c_t, csr_a, b_t, m, k, l)
        ! すべてCで記述
        implicit none
        real(kind=8), intent(inout)  :: c_t(l,m)
        type(csr_matrix_for_c), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b_t(l,k)
        integer(kind=8), intent(in)  :: m, k, l

        integer(kind=8) :: n

        n = size(csr_a%vals)
        call sparse_dgemm_ver0(c_t, csr_a%rows, csr_a%cols, csr_a%vals, b_t, n, m, k, l)
    end subroutine spmm_ver7

    subroutine spmm_ver8(c_t, csr_a, b_t, m, k, l, n_jobs)
        ! すべてCで記述
        implicit none
        real(kind=8), intent(inout)  :: c_t(l,m)
        type(csr_matrix_for_c), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b_t(l,k)
        integer(kind=8), intent(in)  :: m, k, l, n_jobs

        integer(kind=8) :: n

        n = size(csr_a%vals)
        call sparse_dgemm_ver1(c_t, csr_a%rows, csr_a%cols, csr_a%vals, b_t, n, m, k, l, n_jobs)
    end subroutine spmm_ver8


end program main