program main
    !$ use omp_lib
    use mod_sparse_dgemv
    use mod_sparse_dgemm
    use mod_random
    use mod_sort
    use mod_timer
    use mod_csr
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(kind=8) :: n, m, k, l, s
    integer(kind=8) :: i, start, final
    integer(kind=8) :: iter, max_iter
    integer(kind=8) :: n_jobs
    real(kind=8), allocatable :: a(:,:), b(:,:), c(:,:), vals(:), v(:), u(:)
    real(kind=8), allocatable :: b_t(:,:), c_t(:,:)
    integer(kind=8), allocatable :: rows(:), cols(:), idxs(:)
    integer(kind=8), allocatable :: counter(:)
    integer(kind=8), allocatable :: index_mat(:,:)
    type(csr_matrix) :: csr_F, csr_C
    real(kind=8) :: sum_a
    real(kind=8), allocatable :: sum_a_rows(:), sum_a_cols(:)


    m = 256
    k = 512
    l = 31
    s = 32
    n = s * m

    ! Matrix
    allocate(a(m,k), b(k,l), b_t(l,k), c(m,l), c_t(l,m))

    ! COO
    allocate(rows(n),cols(n),vals(n),idxs(k),u(m))

    call fix_random_seed(100_8)
    call random_number(a)
    call random_number(b)
    a = 2d0 * a - 1.9d0
    v = b(:,1)
    ! a = 1
    ! b = 1

    n_jobs = 2
    ! max_iter = 200000
    max_iter = 10000
    ! max_iter = 1000
    ! max_iter = 1

    b_t = transpose(b)
    c_t = transpose(c)

    print*, sum(a)
    csr_F = dense2csr_weighted_sampling_mat(a, s, dim=2_8, start_index=1_8, negative_weights="filter")
    csr_C = dense2csr_weighted_sampling_mat(a, s, dim=2_8, start_index=0_8, negative_weights="filter")
    a = csr_C%to_dense()
    print*, csr_C%n_elements_per_row()
    print*, sum(a)
    print*, sum(csr_F%vals)
    print*, sum(csr_C%vals)
    ! print*, csr_C%count_col_idx()

    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! CALL OMP_SET_NUM_THREADS(1)
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call dgemv("n", m, k, &
    !             1d0, a, m, &
    !             v, 1_8, 0d0, &
    !             u, 1_8)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "dgemv:    ", sum(u), sum(a), time_diff(date_value1, date_value2)

    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     c = matmul(a,b)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "matmul: ", sum(c), sum(a), time_diff(date_value1, date_value2)

    ! print*, '*********************************************************************************************'
    ! c_t = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call sparse_dgemv_ver0(c_t, csr_C%rows, csr_C%cols, csr_C%vals, b_t, n, m, k, n_jobs)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "sparse_dgemv_ver0: ", sum(c_t), sum(csr_C%vals), time_diff(date_value1, date_value2)

    ! print*, '*********************************************************************************************'
    ! c_t = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call sparse_dgemv_ver1(c_t, csr_C%rows, csr_C%cols, csr_C%vals, b_t, n, m, k, n_jobs)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "sparse_dgemv_ver1: ", sum(c_t), sum(csr_C%vals), time_diff(date_value1, date_value2)

    ! print*, '*********************************************************************************************'
    ! c_t = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call sparse_dgemv_ver2(c_t, csr_C%rows, csr_C%cols, csr_C%vals, b_t, n, m, k, n_jobs)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "sparse_dgemv_ver2: ", sum(c_t), sum(csr_C%vals), time_diff(date_value1, date_value2)

    ! print*, '*********************************************************************************************'
    ! c_t = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call sparse_dgemv_ver3(c_t, csr_C%rows, csr_C%cols, csr_C%vals, b_t, n, m, k, n_jobs)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "sparse_dgemv_ver3: ", sum(c_t), sum(csr_C%vals), time_diff(date_value1, date_value2)

    ! print*, '*********************************************************************************************'
    ! c_t = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call sparse_dgemv(c_t, csr_C%rows, csr_C%cols, csr_C%vals, b_t, n, m, k, n_jobs)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "sparse_dgemv: ", sum(c_t), sum(csr_C%vals), time_diff(date_value1, date_value2)

    ! print*, '*********************************************************************************************'
    ! c_t = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call sparse_dgemv_ver5(c_t, csr_C%rows, csr_C%cols, csr_C%vals, b_t, n, m, k, n_jobs)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "sparse_dgemv_ver5: ", sum(c_t), sum(csr_C%vals), time_diff(date_value1, date_value2)

    ! print*, '*********************************************************************************************'
    ! c_t = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call sparse_dgemv_ver6(c_t, csr_C%rows, csr_C%cols, csr_C%vals, b_t, n, m, k, n_jobs)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "sparse_dgemv_ver6: ", sum(c_t), sum(csr_C%vals), time_diff(date_value1, date_value2)
    ! stop















    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! CALL OMP_SET_NUM_THREADS(2)
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call dgemm("N", "N", m, k, l, 1d0, a, m, b, k, 0d0, c, m)
    !     ! call dgemm("N", "T", m, k, l, 1d0, a, m, b_t, l, 0d0, c, m) 
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "dgemm:    ", sum(c), sum(a), time_diff(date_value1, date_value2)
    
    print*, '*********************************************************************************************'
    c = 0d0
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        c = matmul(a,b)
    end do
    call date_and_time(values=date_value2)
    print*, "matmul: ", sum(c), sum(a), time_diff(date_value1, date_value2)
    
    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver0(c, csr_F, b, m, k, l)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver0:  ", sum(c), sum(a), time_diff(date_value1, date_value2)
    
    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver1(c, csr_F, b, m, k, l)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver1:  ", sum(c), sum(a), time_diff(date_value1, date_value2)
    
    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver2(c, csr_F, b_t, m, k, l)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver2:  ", sum(c), sum(a), time_diff(date_value1, date_value2)
    
    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver3(c_t, csr_F, b_t, m, k, l)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver3:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
    !     "Remove Second Loop + Transpose B + Transpose C"
    
    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver4(c_t, csr_F, b_t, m, k, l)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver4:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
    !     "Remove Second Loop + Transpose B + Transpose C + OpenMP"
    
    ! print*, '*********************************************************************************************'
    ! c = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver5(c_t, csr_F, b_t, m, k, l)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver5:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2)
    
    ! print*, '*********************************************************************************************'
    ! c_t = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver6(c_t, csr_F, b_t, m, k, l)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver6:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
    !     "Remove Second Loop + Transpose B + Transpose C + SIMD"
    
    print*, '*********************************************************************************************'
    c_t = 0d0
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call spmm_ver7(c_t, csr_C, b_t, m, k, l)
    end do
    call date_and_time(values=date_value2)
    print*, "my_ver7:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
        "All SIMD (not so optimized)"
    
    print*, '*********************************************************************************************'
    c_t = 0d0
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call spmm_ver8(c_t, csr_C, b_t, m, k, l, 8_8)
    end do
    call date_and_time(values=date_value2)
    print*, "my_ver8:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
        "All SIMD + OpenMP (not so optimized)"

    ! print*, '*********************************************************************************************'
    ! c_t = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver9(c_t, csr_C, b_t, m, k, l, 8_8)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver9:  ", sum(c_t), sum(a), time_diff(date_value1, date_value2), &
    !     "??????????????????????"

    ! print*, '*********************************************************************************************'
    ! c_t = 0d0
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     call spmm_ver10(c_t, csr_C, b_t, m, k, l, 8_8)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "my_ver10:  ", sum(c_t), sum(a), sum(csr_C%vals), time_diff(date_value1, date_value2), &
    !     "??????????????????????"


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

        integer(kind=8) :: ini, fin

        ! c = 0d0
        do i=1, csr_a%n_rows, 1
            ini = csr_a%rows(i)
            fin = csr_a%rows(i+1)
            do j=1, l, 1
                tmp = 0d0
                do n=ini, fin-1, 1
                    idx = csr_a%cols(n)
                    val = csr_a%vals(n)
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
        integer(kind=8) :: ini, fin

        ! c = 0d0
        allocate(tmp(l))
        do i=1, csr_a%n_rows, 1
            ini = csr_a%rows(i)
            fin = csr_a%rows(i+1)
            tmp = 0d0
            do n=ini, fin-1, 1
                idx = csr_a%cols(n)
                val = csr_a%vals(n)
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
        integer(kind=8) :: ini, fin

        ! c = 0d0
        allocate(tmp(l))
        do i=1, csr_a%n_rows, 1
            ini = csr_a%rows(i)
            fin = csr_a%rows(i+1)
            tmp = 0d0
            do n=ini, fin-1, 1
                idx = csr_a%cols(n)
                val = csr_a%vals(n)
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
        integer(kind=8) :: ini, fin


        ! c = 0d0
        allocate(tmp(l))
        do i=1, csr_a%n_rows, 1
            ini = csr_a%rows(i)
            fin = csr_a%rows(i+1)
            tmp = 0d0
            do n=ini, fin-1, 1
                idx = csr_a%cols(n)
                val = csr_a%vals(n)
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
        integer(kind=8) :: ini, fin

        allocate(tmp(l))
        !$omp parallel num_threads(8)
        !$omp do private(i, tmp, n, idx, val, ini, fin)
        do i=1, csr_a%n_rows, 1
            ini = csr_a%rows(i)
            fin = csr_a%rows(i+1)
            tmp = 0d0
            do n=ini, fin-1, 1
                idx = csr_a%cols(n)
                val = csr_a%vals(n)
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
        integer(kind=8) :: ini, fin

        allocate(tmp(l))
        do i=1, csr_a%n_rows, 1
            ini = csr_a%rows(i)
            fin = csr_a%rows(i+1)
            tmp = 0d0
            !$omp parallel num_threads(4)
            !$omp do private(n, idx, val) reduction(+:tmp)
            do n=ini, fin-1, 1
                idx = csr_a%cols(n)
                val = csr_a%vals(n)
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
        integer(kind=8) :: ini, fin

        allocate(tmp(l))
        do i=1, csr_a%n_rows, 1
            ini = csr_a%rows(i)
            fin = csr_a%rows(i+1)
            tmp = 0d0
            n = fin-ini
            call sparse_mv_ver0(tmp, csr_a%cols, csr_a%vals, b_t, n, m, k, l)
            c_t(:,i) = tmp
        end do
    end subroutine spmm_ver6

    subroutine spmm_ver7(c_t, csr_a, b_t, m, k, l)
        ! すべてCで記述
        implicit none
        real(kind=8), intent(inout)  :: c_t(l,m)
        type(csr_matrix), target, intent(in) :: csr_a
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
        type(csr_matrix), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b_t(l,k)
        integer(kind=8), intent(in)  :: m, k, l, n_jobs

        integer(kind=8) :: n

        n = size(csr_a%vals)
        call sparse_dgemm(c_t, csr_a%rows, csr_a%cols, csr_a%vals, b_t, n, m, k, l, n_jobs)
    end subroutine spmm_ver8

    subroutine spmm_ver9(c_t, csr_a, b_t, m, k, l, n_jobs)
        ! すべてCで記述
        implicit none
        real(kind=8), intent(inout)  :: c_t(l,m)
        type(csr_matrix), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b_t(l,k)
        integer(kind=8), intent(in)  :: m, k, l, n_jobs

        integer(kind=8) :: n

        n = size(csr_a%vals)
        call sparse_dgemm_ver2(c_t, csr_a%rows, csr_a%cols, csr_a%vals, b_t, n, m, k, l, n_jobs)
    end subroutine spmm_ver9

    subroutine spmm_ver10(c_t, csr_a, b_t, m, k, l, n_jobs)
        ! すべてCで記述
        implicit none
        real(kind=8), intent(inout)  :: c_t(l,m)
        type(csr_matrix), target, intent(in) :: csr_a
        real(kind=8), intent(in)     :: b_t(l,k)
        integer(kind=8), intent(in)  :: m, k, l, n_jobs

        integer(kind=8) :: n
        integer(kind=8), allocatable :: counter(:)

        n = size(csr_a%vals)

        counter = csr_a%count_col_idx()

        call sparse_dgemm_ver3(c_t, csr_a%rows, csr_a%cols, csr_a%vals, counter, b_t, n, m, k, l, n_jobs)
    end subroutine spmm_ver10


end program main