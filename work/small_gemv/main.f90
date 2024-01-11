program main
    use iso_c_binding
    use mod_timer
    use mod_small_gemv
    use mod_const
    use mod_random
    use mod_common
    implicit none

    procedure(mydgemv), pointer :: sub_ptr => null()
    procedure(mydgemv_X), pointer :: sub_ptr_X => null()

    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(kind=8), allocatable :: time_list(:)
    integer(kind=8) :: lda, ldx, ldy
    integer(kind=8) :: ldx_mod8, ldx_mod4
    integer(kind=8) :: lda_mod8, lda_mod4
    integer(kind=8) :: i, j, max_iter, max_iter_k
    real(kind=8), allocatable :: y(:)
    real(kind=4), allocatable :: w(:)
    real(kind=8), allocatable :: a(:,:), a_t(:,:), x(:)
    real(kind=8), allocatable :: a_mod8(:,:), a_mod8_t(:,:), x_mod8(:)
    real(kind=8), allocatable :: a_mod4(:,:), a_mod4_t(:,:), x_mod4(:)
    real(kind=8), allocatable :: tmp_mod8_t(:), tmp(:)
    real(kind=8) :: res

    integer(kind=8) :: count_rate_s, count_max_s, count_s
    integer(kind=8) :: count_rate_e, count_max_e, count_e
    integer(kind=8), allocatable :: ldxs(:), ldas(:)

    procedure(mydgemv), pointer :: sub_ptr_normal_ver0 => null()
    procedure(mydgemv), pointer :: sub_ptr_transposed_ver0 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v0_0 => null(), sub_ptr_t_v0_0 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v0_1_1 => null(), sub_ptr_t_v0_1_1 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v0_1_2 => null(), sub_ptr_t_v0_1_2 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v0_1_4 => null(), sub_ptr_t_v0_1_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v0_1_8 => null(), sub_ptr_t_v0_1_8 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v0_2_1 => null(), sub_ptr_t_v0_2_1 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v0_2_2 => null(), sub_ptr_t_v0_2_2 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v0_2_4 => null(), sub_ptr_t_v0_2_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v0_2_8 => null(), sub_ptr_t_v0_2_8 => null()
    procedure(mydgemv), pointer :: sub_ptr_t_v0_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_t_v0_5 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v1_0 => null(), sub_ptr_t_v1_0 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v1_1_1 => null(), sub_ptr_t_v1_1_1 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v1_1_2 => null(), sub_ptr_t_v1_1_2 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v1_1_4 => null(), sub_ptr_t_v1_1_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v1_1_8 => null(), sub_ptr_t_v1_1_8 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v1_2_1 => null(), sub_ptr_t_v1_2_1 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v1_2_2 => null(), sub_ptr_t_v1_2_2 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v1_2_4 => null(), sub_ptr_t_v1_2_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v1_2_8 => null(), sub_ptr_t_v1_2_8 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v1_4 => null(), sub_ptr_t_v1_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_t_v1_5 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v2_0 => null(), sub_ptr_t_v2_0 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v2_1_1 => null(), sub_ptr_t_v2_1_1 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v2_1_2 => null(), sub_ptr_t_v2_1_2 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v2_1_4 => null(), sub_ptr_t_v2_1_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v2_1_8 => null(), sub_ptr_t_v2_1_8 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v2_2_1 => null(), sub_ptr_t_v2_2_1 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v2_2_2 => null(), sub_ptr_t_v2_2_2 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v2_2_4 => null(), sub_ptr_t_v2_2_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v2_2_8 => null(), sub_ptr_t_v2_2_8 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v2_4 => null(), sub_ptr_t_v2_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_t_v2_5 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v3_0 => null(), sub_ptr_t_v3_0 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v3_1_1 => null(), sub_ptr_t_v3_1_1 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v3_1_2 => null(), sub_ptr_t_v3_1_2 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v3_1_4 => null(), sub_ptr_t_v3_1_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v3_1_8 => null(), sub_ptr_t_v3_1_8 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v3_2_1 => null(), sub_ptr_t_v3_2_1 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v3_2_2 => null(), sub_ptr_t_v3_2_2 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v3_2_4 => null(), sub_ptr_t_v3_2_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v3_2_8 => null(), sub_ptr_t_v3_2_8 => null()
    procedure(mydgemv), pointer :: sub_ptr_n_v3_4 => null(), sub_ptr_t_v3_4 => null()
    procedure(mydgemv), pointer :: sub_ptr_t_v3_5 => null()
    procedure(mydgemv_X), pointer :: sub_ptr_n_vX => null()
    procedure(mydgemv_X), pointer :: sub_ptr_t_vX => null()


    integer(kind=8) :: ttt, counter, total
    integer(kind=8), allocatable :: times(:)
    real(kind=8) :: sum_t, sum_b, sum_c, sum_d, sum_e
    logical(kind=4) :: print_sum

    ! Loop Count
    ! max_iter = 10240000000_8
    ! max_iter = 1024000000_8
    max_iter = 102400000_8 * 2_8

    ! Common
    call fix_random_seed(100_8)
    ldxs = [2,4,6,8,10,12,14,16,20,22,26,30,34,40,46,52,60,70,80,92,108,124,142,164,190,218,252,290,334,386,444,512]

    ldas = [2,3,4,5,6,7,8,10,12,14,16,19,22,26,30,36,42,49,58,68,79,93,109,128,150,176,207,243,285,334,392,460,&
        540,634,744,873,1024]

    sub_ptr_n_v0_0 => mydgemv_n_ver0_0
    sub_ptr_t_v0_0 => mydgemv_t_ver0_0

    sub_ptr_n_v0_1_1 => mydgemv_n_ver0_1_unroll1; sub_ptr_t_v0_1_1 => mydgemv_t_ver0_1_unroll1
    sub_ptr_n_v0_1_2 => mydgemv_n_ver0_1_unroll2; sub_ptr_t_v0_1_2 => mydgemv_t_ver0_1_unroll2
    sub_ptr_n_v0_1_4 => mydgemv_n_ver0_1_unroll4; sub_ptr_t_v0_1_4 => mydgemv_t_ver0_1_unroll4
    sub_ptr_n_v0_1_8 => mydgemv_n_ver0_1_unroll8; sub_ptr_t_v0_1_8 => mydgemv_t_ver0_1_unroll8

    sub_ptr_n_v0_2_1 => mydgemv_n_ver0_2_unroll1; sub_ptr_t_v0_2_1 => mydgemv_t_ver0_2_unroll1
    sub_ptr_n_v0_2_2 => mydgemv_n_ver0_2_unroll2; sub_ptr_t_v0_2_2 => mydgemv_t_ver0_2_unroll2
    sub_ptr_n_v0_2_4 => mydgemv_n_ver0_2_unroll4; sub_ptr_t_v0_2_4 => mydgemv_t_ver0_2_unroll4
    sub_ptr_n_v0_2_8 => mydgemv_n_ver0_2_unroll8; sub_ptr_t_v0_2_8 => mydgemv_t_ver0_2_unroll8
    
    sub_ptr_n_v1_0 => mydgemv_n_ver1_0; sub_ptr_t_v1_0 => mydgemv_t_ver1_0

    sub_ptr_n_v1_1_1 => mydgemv_n_ver1_1_unroll1; sub_ptr_t_v1_1_1 => mydgemv_t_ver1_1_unroll1
    sub_ptr_n_v1_1_2 => mydgemv_n_ver1_1_unroll2; sub_ptr_t_v1_1_2 => mydgemv_t_ver1_1_unroll2
    sub_ptr_n_v1_1_4 => mydgemv_n_ver1_1_unroll4; sub_ptr_t_v1_1_4 => mydgemv_t_ver1_1_unroll4
    sub_ptr_n_v1_1_8 => mydgemv_n_ver1_1_unroll8; sub_ptr_t_v1_1_8 => mydgemv_t_ver1_1_unroll8

    sub_ptr_n_v1_2_1 => mydgemv_n_ver1_2_unroll1; sub_ptr_t_v1_2_1 => mydgemv_t_ver1_2_unroll1
    sub_ptr_n_v1_2_2 => mydgemv_n_ver1_2_unroll2; sub_ptr_t_v1_2_2 => mydgemv_t_ver1_2_unroll2
    sub_ptr_n_v1_2_4 => mydgemv_n_ver1_2_unroll4; sub_ptr_t_v1_2_4 => mydgemv_t_ver1_2_unroll4
    sub_ptr_n_v1_2_8 => mydgemv_n_ver1_2_unroll8; sub_ptr_t_v1_2_8 => mydgemv_t_ver1_2_unroll8

    sub_ptr_n_v2_0 => mydgemv_n_ver2_0; sub_ptr_t_v2_0 => mydgemv_t_ver2_0

    sub_ptr_n_v2_1_1 => mydgemv_n_ver2_1_unroll1; sub_ptr_t_v2_1_1 => mydgemv_t_ver2_1_unroll1
    sub_ptr_n_v2_1_2 => mydgemv_n_ver2_1_unroll2; sub_ptr_t_v2_1_2 => mydgemv_t_ver2_1_unroll2
    sub_ptr_n_v2_1_4 => mydgemv_n_ver2_1_unroll4; sub_ptr_t_v2_1_4 => mydgemv_t_ver2_1_unroll4
    sub_ptr_n_v2_1_8 => mydgemv_n_ver2_1_unroll8; sub_ptr_t_v2_1_8 => mydgemv_t_ver2_1_unroll8

    sub_ptr_n_v2_2_1 => mydgemv_n_ver2_2_unroll1; sub_ptr_t_v2_2_1 => mydgemv_t_ver2_2_unroll1
    sub_ptr_n_v2_2_2 => mydgemv_n_ver2_2_unroll2; sub_ptr_t_v2_2_2 => mydgemv_t_ver2_2_unroll2
    sub_ptr_n_v2_2_4 => mydgemv_n_ver2_2_unroll4; sub_ptr_t_v2_2_4 => mydgemv_t_ver2_2_unroll4
    sub_ptr_n_v2_2_8 => mydgemv_n_ver2_2_unroll8; sub_ptr_t_v2_2_8 => mydgemv_t_ver2_2_unroll8

    sub_ptr_n_v3_0 => mydgemv_n_ver3_0; sub_ptr_t_v3_0 => mydgemv_t_ver3_0

    sub_ptr_n_v3_1_1 => mydgemv_n_ver3_1_unroll1; sub_ptr_t_v3_1_1 => mydgemv_t_ver3_1_unroll1
    sub_ptr_n_v3_1_2 => mydgemv_n_ver3_1_unroll2; sub_ptr_t_v3_1_2 => mydgemv_t_ver3_1_unroll2
    sub_ptr_n_v3_1_4 => mydgemv_n_ver3_1_unroll4; sub_ptr_t_v3_1_4 => mydgemv_t_ver3_1_unroll4
    sub_ptr_n_v3_1_8 => mydgemv_n_ver3_1_unroll8; sub_ptr_t_v3_1_8 => mydgemv_t_ver3_1_unroll8

    sub_ptr_n_v3_2_1 => mydgemv_n_ver3_2_unroll1; sub_ptr_t_v3_2_1 => mydgemv_t_ver3_2_unroll1
    sub_ptr_n_v3_2_2 => mydgemv_n_ver3_2_unroll2; sub_ptr_t_v3_2_2 => mydgemv_t_ver3_2_unroll2
    sub_ptr_n_v3_2_4 => mydgemv_n_ver3_2_unroll4; sub_ptr_t_v3_2_4 => mydgemv_t_ver3_2_unroll4
    sub_ptr_n_v3_2_8 => mydgemv_n_ver3_2_unroll8; sub_ptr_t_v3_2_8 => mydgemv_t_ver3_2_unroll8

    sub_ptr_n_vX => mydgemv_n_verX
    sub_ptr_t_vX => mydgemv_t_verX

    counter = size(ldas) * size(ldxs)
    total = counter
    print_sum = f_

    allocate(times(87))
    ! open(101, file="small_dgemv_20240101_ver3.log")
    do i=1, size(ldxs), 1
        ldx = ldxs(i)
        ldx_mod4 = find_next_multiple(ldx, 4_8)
        ldx_mod8 = find_next_multiple(ldx, 8_8)
        
        do j=1, size(ldas), 1
            
            lda = ldas(j)
            lda_mod4 = find_next_multiple(lda, 4_8)
            lda_mod8 = find_next_multiple(lda, 8_8)
            ! if (print_sum) print*, '*********************************************************************************************'
            ! if (print_sum) print*, " SHAPE = ", lda, ldx, ldx_mod4, ldx_mod8, max_iter_k

            call select_sub_ptr_t_v0_4(sub_ptr_t_v0_4, ldx)
            call select_sub_ptr_t_v0_5(sub_ptr_t_v0_5, ldx)

            call select_sub_ptr_n_v1_4(sub_ptr_n_v1_4, ldx)
            call select_sub_ptr_t_v1_4(sub_ptr_t_v1_4, ldx)
            call select_sub_ptr_t_v1_5(sub_ptr_t_v1_5, ldx)

            call select_sub_ptr_n_v2_4(sub_ptr_n_v2_4, ldx_mod4)
            call select_sub_ptr_t_v2_4(sub_ptr_t_v2_4, ldx_mod4)
            call select_sub_ptr_t_v2_5(sub_ptr_t_v2_5, ldx_mod4)

            call select_sub_ptr_n_v3_4(sub_ptr_n_v3_4, ldx_mod8)
            call select_sub_ptr_t_v3_4(sub_ptr_t_v3_4, ldx_mod8)
            call select_sub_ptr_t_v3_5(sub_ptr_t_v3_5, ldx_mod8)

            ldy = lda
            
            if (allocated(y)) then
                ! print*, "deallocate(y)"
                deallocate(y)
            end if
            if (allocated(a)) then
                ! print*, "deallocate(a)"
                deallocate(a)
            end if
            if (allocated(a_t)) then
                ! print*, "deallocate(a_t)"
                deallocate(a_t) 
            end if
            if (allocated(x)) then
                ! print*, "deallocate(x)"
                deallocate(x)
            end if
            if (allocated(a_mod8)) then
                ! print*, "deallocate(a_mod8)"
                deallocate(a_mod8) 
            end if
            if (allocated(a_mod8_t)) then
                ! print*, "deallocate(a_mod8_t)"
                deallocate(a_mod8_t) 
            end if
            if (allocated(x_mod8)) then
                ! print*, "deallocate(x_mod8)"
                deallocate(x_mod8)
            end if
            if (allocated(a_mod4)) then
                ! print*, "deallocate(a_mod4)"
                deallocate(a_mod4)
            end if
            if (allocated(a_mod4_t)) then
                ! print*, "deallocate(a_mod4_t)"
                deallocate(a_mod4_t) 
            end if
            if (allocated(x_mod4)) then
                ! print*, "deallocate(x_mod4)"
                deallocate(x_mod4)
            end if
            if (allocated(tmp_mod8_t)) then
                ! print*, "deallocate(tmp_mod8_t)"
                deallocate(tmp_mod8_t)
            end if
            if (allocated(tmp)) then
                ! print*, "deallocate(tmp)"
                deallocate(tmp)
            end if
            
            allocate(y(ldy))
            allocate(a(lda, ldx))
            allocate(a_t(ldx, lda)) 
            allocate(x(ldx))
            allocate(a_mod8(lda, ldx_mod8)) 
            allocate(a_mod8_t(ldx_mod8, lda)) 
            allocate(x_mod8(ldx_mod8))
            allocate(a_mod4(lda, ldx_mod4))
            allocate(a_mod4_t(ldx_mod4, lda)) 
            allocate(x_mod4(ldx_mod4))
            allocate(tmp_mod8_t(ldx_mod8))
            allocate(tmp(lda))
            call random_number(a)
            call random_number(x)
            ! ---------------------------------------------------------------------
            max_iter_k = maxval([max_iter / ldx / lda, 1_8])
            print*, max_iter_k
            cycle
            max_iter_k = 1_8
            ! a = 1; x = 1;
            ! print_sum = t_
            ! ---------------------------------------------------------------------

            a_mod4 = 0.0
            x_mod4 = 0.0
            a_mod8 = 0.0
            x_mod8 = 0.0

            a_mod4(:,1:ldx) = a(:,:)
            x_mod4(1:ldx) = x(:)
            a_mod8(:,1:ldx) = a(:,:)
            x_mod8(1:ldx) = x(:)

            a_t = transpose(a)
            a_mod4_t = transpose(a_mod4)
            a_mod8_t = transpose(a_mod8)
            
            print*, " SHAPE = ", lda, ldx, ldx_mod4, ldx_mod8, max_iter_k
            ttt = 1
            times(1) = run_dgemv_transposed(max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum); sum_t = sum(y)

            times(ttt) = run_dgemv_normal(max_iter_k, a, x, y, lda, ldx, ldy, print_sum); ttt = ttt + 1;
            times(ttt) = run_dgemv_transposed(max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum); ttt = ttt + 1; sum_t = sum(y)
            times(ttt) = run_mydgemv(sub_ptr_n_v0_0, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V0-0] Naive-C Normal: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_0, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-0] Naive-C Transposed: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v0_1_1, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V0-1] Naive-C Normal Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_1_1, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-1] Naive-C Transposed Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v0_1_2, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V0-1] Naive-C Normal Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_1_2, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-1] Naive-C Transposed Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v0_1_4, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V0-1] Naive-C Normal Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_1_4, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-1] Naive-C Transposed Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v0_1_8, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V0-1] Naive-C Normal Unroll-8: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_1_8, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-1] Naive-C Transposed Unroll-8: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v0_2_1, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V0-2] Naive-C Normal Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_1_1, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-2] Naive-C Transposed Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v0_2_2, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V0-2] Naive-C Normal Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_1_2, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-2] Naive-C Transposed Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v0_2_4, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V0-2] Naive-C Normal Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_1_2, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-2] Naive-C Transposed Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v0_2_8, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V0-2] Naive-C Normal Unroll-8: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_1_2, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-2] Naive-C Transposed Unroll-8: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_4, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-4] Naive-C Transposed UnlimVars: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v0_5, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V0-5] Naive-C Transposed 16Vars: ", sum_t); ttt = ttt + 1; 
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v1_0, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V1-0] Xmm Normal: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_0, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V1-0] Xmm Transposed: ", sum_t); ttt = ttt + 1; 
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v1_1_1, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V1-1] Xmm Normal Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_1_1, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V1-1] Xmm Transposed Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v1_1_2, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V1-1] Xmm Normal Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_1_2, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V1-1] Xmm Transposed Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v1_1_4, max_iter_k, a_mod4, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V1-1] Xmm Normal Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_1_4, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V1-1] Xmm Transposed Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v1_1_8, max_iter_k, a_mod8, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V1-1] Xmm Normal Unroll-8: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_1_8, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V1-1] Xmm Transposed Unroll-8: ", sum_t); ttt = ttt + 1; 
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v1_2_1, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V1-2] Xmm Normal Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_2_1, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V1-2] Xmm Transposed Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v1_2_2, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V1-2] Xmm Normal Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_2_2, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V1-2] Xmm Transposed Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v1_2_4, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V1-2] Xmm Normal Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_2_4, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V1-2] Xmm Transposed Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v1_2_8, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V1-2] Xmm Normal Unroll-8: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_2_8, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V1-2] Xmm Transposed Unroll-8: ", sum_t); ttt = ttt + 1; 
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v1_4, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V1-4] Xmm Normal UnlimVars: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_4, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V1-4] Xmm Transposed UnlimVars: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v1_5, max_iter_k, a_t, x, y, lda, ldx, ldy, print_sum, &
                "[V1-5] Xmm Transposed 16Vars: ", sum_t); ttt = ttt + 1; 
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v2_0, max_iter_k, a_mod4, x_mod4, y, lda, ldx, ldy, print_sum, &
                "[V2-0] Ymm Normal: ", sum_t); ttt = ttt + 1;
            times(ttt) = run_mydgemv(sub_ptr_t_v2_0, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-0] Ymm Transposed: ", sum_t); ttt = ttt + 1;
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v2_1_1, max_iter_k, a_mod4, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-1] Ymm Normal Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v2_1_1, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-1] Ymm Transposed Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v2_1_2, max_iter_k, a_mod4, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-1] Ymm Normal Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v2_1_2, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-1] Ymm Transposed Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v2_1_4, max_iter_k, a_mod4, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-1] Ymm Normal Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v2_1_4, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-1] Ymm Transposed Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v2_1_8, max_iter_k, a_mod4, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-1] Ymm Normal Unroll-8: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v2_1_8, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-1] Ymm Transposed Unroll-8: ", sum_t); ttt = ttt + 1; 
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v2_2_1, max_iter_k, a_mod4, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-2] Ymm Normal Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v2_2_1, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-2] Ymm Transposed Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v2_2_2, max_iter_k, a_mod4, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-2] Ymm Normal Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v2_2_2, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-2] Ymm Transposed Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v2_2_4, max_iter_k, a_mod4, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-2] Ymm Normal Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v2_2_4, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-2] Ymm Transposed Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v2_2_8, max_iter_k, a_mod4, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-2] Ymm Normal Unroll-8: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v2_2_8, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-2] Ymm Transposed Unroll-8: ", sum_t); ttt = ttt + 1; 
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v2_4, max_iter_k, a_mod4, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-4] Ymm Normal: ", sum_t); ttt = ttt + 1;
            times(ttt) = run_mydgemv(sub_ptr_t_v2_4, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-4] Ymm Transposed: ", sum_t); ttt = ttt + 1;
            times(ttt) = run_mydgemv(sub_ptr_t_v2_5, max_iter_k, a_mod4_t, x_mod4, y, lda, ldx_mod4, ldy, print_sum, &
                "[V2-5] Ymm Transposed: ", sum_t); ttt = ttt + 1;
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v3_0, max_iter_k, a, x, y, lda, ldx, ldy, print_sum, &
                "[V3-0] Zmm Normal: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_0, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-0] Zmm Transposed: ", sum_t); ttt = ttt + 1; 
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v3_1_1, max_iter_k, a_mod8, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-1] Zmm Normal Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_1_1, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-1] Zmm Transposed Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v3_1_2, max_iter_k, a_mod8, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-1] Zmm Normal Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_1_2, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-1] Zmm Transposed Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v3_1_4, max_iter_k, a_mod8, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-1] Zmm Normal Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_1_4, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-1] Zmm Transposed Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v3_1_8, max_iter_k, a_mod8, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-1] Zmm Normal Unroll-8: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_1_8, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-1] Zmm Transposed Unroll-8: ", sum_t); ttt = ttt + 1; 
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v3_2_1, max_iter_k, a_mod8, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-2] Zmm Normal Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_2_1, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-2] Zmm Transposed Unroll-1: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v3_2_2, max_iter_k, a_mod8, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-2] Zmm Normal Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_2_2, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-2] Zmm Transposed Unroll-2: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v3_2_4, max_iter_k, a_mod8, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-2] Zmm Normal Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_2_4, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-2] Zmm Transposed Unroll-4: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_n_v3_2_8, max_iter_k, a_mod8, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-2] Zmm Normal Unroll-8: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_2_8, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-2] Zmm Transposed Unroll-8: ", sum_t); ttt = ttt + 1; 
            ! -------------------------------------------------------------------------------------
            times(ttt) = run_mydgemv(sub_ptr_n_v3_4, max_iter_k, a_mod8, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-4] Zmm Normal UnlimVars: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_4, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-4] Zmm Transposed 16Vars: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv(sub_ptr_t_v3_5, max_iter_k, a_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[V3-5] Zmm Transposed 16Vars: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv_X(sub_ptr_n_vX, max_iter_k, a, tmp, x, y, lda, ldx, ldy, print_sum, &
                "[VX-0] Zmm Normal: ", sum_t); ttt = ttt + 1; 
            times(ttt) = run_mydgemv_X(sub_ptr_t_vX, max_iter_k, a_mod8_t, tmp_mod8_t, x_mod8, y, lda, ldx_mod8, ldy, print_sum, &
                "[VX-0] Zmm Transposed: ", sum_t); ttt = ttt + 1; 
            ! print*, "            done."
            
            if (print_sum) print*, "  TIME = ", int(times)
            ! write(101, *) lda, ", ", ldx, ", ", array_to_comma_separated_string(times)
            ! write(101, *) " SHAPE = ", lda, ldx, "  SUM = ", sum_t, sum_b, sum_c, sum_d
        end do
    end do
    ! close(101)

contains

    function array_to_comma_separated_string(array) result(comma_separated_string)
        integer(kind=8), intent(in) :: array(:)
        character(len=:), allocatable :: comma_separated_string
        character(len=32) :: temp_str
        integer :: i

        ! 最初の要素を文字列に変換
        write(temp_str, '(I32)') array(1)
        comma_separated_string = adjustl(temp_str)

        ! 残りの要素を処理
        do i = 2, size(array)
            write(temp_str, '(I32)') array(i)
            comma_separated_string = trim(comma_separated_string) // ',' // adjustl(temp_str)
        end do
    end function array_to_comma_separated_string

    subroutine left_align_str(inputString, outputString, fixedLength)
        character(len=*), intent(in) :: inputString
        integer, intent(in) :: fixedLength
        character(len=fixedLength), intent(out) :: outputString

        integer :: inputLength

        inputLength = len_trim(inputString)  ! 入力文字列の実際の長さを取得
        outputString = inputString           ! 出力文字列に入力文字列をコピー

        if (inputLength < fixedLength) then
            outputString(inputLength+1:) = ' '  ! 余った部分を空白で埋める
        endif
    end subroutine left_align_str

    subroutine select_sub_ptr_t_v0_4(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 2) sub_ptr => mydgemv_ver0_4_c_2
        if (ldx == 4) sub_ptr => mydgemv_ver0_4_c_4
        if (ldx == 6) sub_ptr => mydgemv_ver0_4_c_6
        if (ldx == 8) sub_ptr => mydgemv_ver0_4_c_8
        if (ldx == 10) sub_ptr => mydgemv_ver0_4_c_10
        if (ldx == 12) sub_ptr => mydgemv_ver0_4_c_12
        if (ldx == 14) sub_ptr => mydgemv_ver0_4_c_14
        if (ldx == 16) sub_ptr => mydgemv_ver0_4_c_16
        if (ldx == 18) sub_ptr => mydgemv_ver0_4_c_18
        if (ldx == 20) sub_ptr => mydgemv_ver0_4_c_20
        if (ldx == 22) sub_ptr => mydgemv_ver0_4_c_22
        if (ldx == 24) sub_ptr => mydgemv_ver0_4_c_24
        if (ldx == 26) sub_ptr => mydgemv_ver0_4_c_26
        if (ldx == 28) sub_ptr => mydgemv_ver0_4_c_28
        if (ldx == 30) sub_ptr => mydgemv_ver0_4_c_30
        if (ldx == 32) sub_ptr => mydgemv_ver0_4_c_32
        if (ldx == 34) sub_ptr => mydgemv_ver0_4_c_34
        if (ldx == 36) sub_ptr => mydgemv_ver0_4_c_36
        if (ldx == 38) sub_ptr => mydgemv_ver0_4_c_38
        if (ldx == 40) sub_ptr => mydgemv_ver0_4_c_40
        if (ldx == 42) sub_ptr => mydgemv_ver0_4_c_42
        if (ldx == 44) sub_ptr => mydgemv_ver0_4_c_44
        if (ldx == 46) sub_ptr => mydgemv_ver0_4_c_46
        if (ldx == 48) sub_ptr => mydgemv_ver0_4_c_48
        if (ldx == 50) sub_ptr => mydgemv_ver0_4_c_50
        if (ldx == 52) sub_ptr => mydgemv_ver0_4_c_52
        if (ldx == 54) sub_ptr => mydgemv_ver0_4_c_54
        if (ldx == 56) sub_ptr => mydgemv_ver0_4_c_56
        if (ldx == 58) sub_ptr => mydgemv_ver0_4_c_58
        if (ldx == 60) sub_ptr => mydgemv_ver0_4_c_60
        if (ldx == 62) sub_ptr => mydgemv_ver0_4_c_62
        if (ldx == 64) sub_ptr => mydgemv_ver0_4_c_64
        if (ldx == 66) sub_ptr => mydgemv_ver0_4_c_66
        if (ldx == 68) sub_ptr => mydgemv_ver0_4_c_68
        if (ldx == 70) sub_ptr => mydgemv_ver0_4_c_70
        if (ldx == 72) sub_ptr => mydgemv_ver0_4_c_72
        if (ldx == 74) sub_ptr => mydgemv_ver0_4_c_74
        if (ldx == 76) sub_ptr => mydgemv_ver0_4_c_76
        if (ldx == 78) sub_ptr => mydgemv_ver0_4_c_78
        if (ldx == 80) sub_ptr => mydgemv_ver0_4_c_80
        if (ldx == 82) sub_ptr => mydgemv_ver0_4_c_82
        if (ldx == 84) sub_ptr => mydgemv_ver0_4_c_84
        if (ldx == 86) sub_ptr => mydgemv_ver0_4_c_86
        if (ldx == 88) sub_ptr => mydgemv_ver0_4_c_88
        if (ldx == 90) sub_ptr => mydgemv_ver0_4_c_90
        if (ldx == 92) sub_ptr => mydgemv_ver0_4_c_92
        if (ldx == 94) sub_ptr => mydgemv_ver0_4_c_94
        if (ldx == 96) sub_ptr => mydgemv_ver0_4_c_96
        if (ldx == 98) sub_ptr => mydgemv_ver0_4_c_98
        if (ldx == 100) sub_ptr => mydgemv_ver0_4_c_100
        if (ldx == 102) sub_ptr => mydgemv_ver0_4_c_102
        if (ldx == 104) sub_ptr => mydgemv_ver0_4_c_104
        if (ldx == 106) sub_ptr => mydgemv_ver0_4_c_106
        if (ldx == 108) sub_ptr => mydgemv_ver0_4_c_108
        if (ldx == 110) sub_ptr => mydgemv_ver0_4_c_110
        if (ldx == 112) sub_ptr => mydgemv_ver0_4_c_112
        if (ldx == 114) sub_ptr => mydgemv_ver0_4_c_114
        if (ldx == 116) sub_ptr => mydgemv_ver0_4_c_116
        if (ldx == 118) sub_ptr => mydgemv_ver0_4_c_118
        if (ldx == 120) sub_ptr => mydgemv_ver0_4_c_120
        if (ldx == 122) sub_ptr => mydgemv_ver0_4_c_122
        if (ldx == 124) sub_ptr => mydgemv_ver0_4_c_124
        if (ldx == 126) sub_ptr => mydgemv_ver0_4_c_126
        if (ldx == 128) sub_ptr => mydgemv_ver0_4_c_128
        if (ldx == 130) sub_ptr => mydgemv_ver0_4_c_130
        if (ldx == 132) sub_ptr => mydgemv_ver0_4_c_132
        if (ldx == 134) sub_ptr => mydgemv_ver0_4_c_134
        if (ldx == 136) sub_ptr => mydgemv_ver0_4_c_136
        if (ldx == 138) sub_ptr => mydgemv_ver0_4_c_138
        if (ldx == 140) sub_ptr => mydgemv_ver0_4_c_140
        if (ldx == 142) sub_ptr => mydgemv_ver0_4_c_142
        if (ldx == 144) sub_ptr => mydgemv_ver0_4_c_144
        if (ldx == 146) sub_ptr => mydgemv_ver0_4_c_146
        if (ldx == 148) sub_ptr => mydgemv_ver0_4_c_148
        if (ldx == 150) sub_ptr => mydgemv_ver0_4_c_150
        if (ldx == 152) sub_ptr => mydgemv_ver0_4_c_152
        if (ldx == 154) sub_ptr => mydgemv_ver0_4_c_154
        if (ldx == 156) sub_ptr => mydgemv_ver0_4_c_156
        if (ldx == 158) sub_ptr => mydgemv_ver0_4_c_158
        if (ldx == 160) sub_ptr => mydgemv_ver0_4_c_160
        if (ldx == 162) sub_ptr => mydgemv_ver0_4_c_162
        if (ldx == 164) sub_ptr => mydgemv_ver0_4_c_164
        if (ldx == 166) sub_ptr => mydgemv_ver0_4_c_166
        if (ldx == 168) sub_ptr => mydgemv_ver0_4_c_168
        if (ldx == 170) sub_ptr => mydgemv_ver0_4_c_170
        if (ldx == 172) sub_ptr => mydgemv_ver0_4_c_172
        if (ldx == 174) sub_ptr => mydgemv_ver0_4_c_174
        if (ldx == 176) sub_ptr => mydgemv_ver0_4_c_176
        if (ldx == 178) sub_ptr => mydgemv_ver0_4_c_178
        if (ldx == 180) sub_ptr => mydgemv_ver0_4_c_180
        if (ldx == 182) sub_ptr => mydgemv_ver0_4_c_182
        if (ldx == 184) sub_ptr => mydgemv_ver0_4_c_184
        if (ldx == 186) sub_ptr => mydgemv_ver0_4_c_186
        if (ldx == 188) sub_ptr => mydgemv_ver0_4_c_188
        if (ldx == 190) sub_ptr => mydgemv_ver0_4_c_190
        if (ldx == 192) sub_ptr => mydgemv_ver0_4_c_192
        if (ldx == 194) sub_ptr => mydgemv_ver0_4_c_194
        if (ldx == 196) sub_ptr => mydgemv_ver0_4_c_196
        if (ldx == 198) sub_ptr => mydgemv_ver0_4_c_198
        if (ldx == 200) sub_ptr => mydgemv_ver0_4_c_200
        if (ldx == 202) sub_ptr => mydgemv_ver0_4_c_202
        if (ldx == 204) sub_ptr => mydgemv_ver0_4_c_204
        if (ldx == 206) sub_ptr => mydgemv_ver0_4_c_206
        if (ldx == 208) sub_ptr => mydgemv_ver0_4_c_208
        if (ldx == 210) sub_ptr => mydgemv_ver0_4_c_210
        if (ldx == 212) sub_ptr => mydgemv_ver0_4_c_212
        if (ldx == 214) sub_ptr => mydgemv_ver0_4_c_214
        if (ldx == 216) sub_ptr => mydgemv_ver0_4_c_216
        if (ldx == 218) sub_ptr => mydgemv_ver0_4_c_218
        if (ldx == 220) sub_ptr => mydgemv_ver0_4_c_220
        if (ldx == 222) sub_ptr => mydgemv_ver0_4_c_222
        if (ldx == 224) sub_ptr => mydgemv_ver0_4_c_224
        if (ldx == 226) sub_ptr => mydgemv_ver0_4_c_226
        if (ldx == 228) sub_ptr => mydgemv_ver0_4_c_228
        if (ldx == 230) sub_ptr => mydgemv_ver0_4_c_230
        if (ldx == 232) sub_ptr => mydgemv_ver0_4_c_232
        if (ldx == 234) sub_ptr => mydgemv_ver0_4_c_234
        if (ldx == 236) sub_ptr => mydgemv_ver0_4_c_236
        if (ldx == 238) sub_ptr => mydgemv_ver0_4_c_238
        if (ldx == 240) sub_ptr => mydgemv_ver0_4_c_240
        if (ldx == 242) sub_ptr => mydgemv_ver0_4_c_242
        if (ldx == 244) sub_ptr => mydgemv_ver0_4_c_244
        if (ldx == 246) sub_ptr => mydgemv_ver0_4_c_246
        if (ldx == 248) sub_ptr => mydgemv_ver0_4_c_248
        if (ldx == 250) sub_ptr => mydgemv_ver0_4_c_250
        if (ldx == 252) sub_ptr => mydgemv_ver0_4_c_252
        if (ldx == 254) sub_ptr => mydgemv_ver0_4_c_254
        if (ldx == 256) sub_ptr => mydgemv_ver0_4_c_256
        if (ldx == 258) sub_ptr => mydgemv_ver0_4_c_258
        if (ldx == 260) sub_ptr => mydgemv_ver0_4_c_260
        if (ldx == 262) sub_ptr => mydgemv_ver0_4_c_262
        if (ldx == 264) sub_ptr => mydgemv_ver0_4_c_264
        if (ldx == 266) sub_ptr => mydgemv_ver0_4_c_266
        if (ldx == 268) sub_ptr => mydgemv_ver0_4_c_268
        if (ldx == 270) sub_ptr => mydgemv_ver0_4_c_270
        if (ldx == 272) sub_ptr => mydgemv_ver0_4_c_272
        if (ldx == 274) sub_ptr => mydgemv_ver0_4_c_274
        if (ldx == 276) sub_ptr => mydgemv_ver0_4_c_276
        if (ldx == 278) sub_ptr => mydgemv_ver0_4_c_278
        if (ldx == 280) sub_ptr => mydgemv_ver0_4_c_280
        if (ldx == 282) sub_ptr => mydgemv_ver0_4_c_282
        if (ldx == 284) sub_ptr => mydgemv_ver0_4_c_284
        if (ldx == 286) sub_ptr => mydgemv_ver0_4_c_286
        if (ldx == 288) sub_ptr => mydgemv_ver0_4_c_288
        if (ldx == 290) sub_ptr => mydgemv_ver0_4_c_290
        if (ldx == 292) sub_ptr => mydgemv_ver0_4_c_292
        if (ldx == 294) sub_ptr => mydgemv_ver0_4_c_294
        if (ldx == 296) sub_ptr => mydgemv_ver0_4_c_296
        if (ldx == 298) sub_ptr => mydgemv_ver0_4_c_298
        if (ldx == 300) sub_ptr => mydgemv_ver0_4_c_300
        if (ldx == 302) sub_ptr => mydgemv_ver0_4_c_302
        if (ldx == 304) sub_ptr => mydgemv_ver0_4_c_304
        if (ldx == 306) sub_ptr => mydgemv_ver0_4_c_306
        if (ldx == 308) sub_ptr => mydgemv_ver0_4_c_308
        if (ldx == 310) sub_ptr => mydgemv_ver0_4_c_310
        if (ldx == 312) sub_ptr => mydgemv_ver0_4_c_312
        if (ldx == 314) sub_ptr => mydgemv_ver0_4_c_314
        if (ldx == 316) sub_ptr => mydgemv_ver0_4_c_316
        if (ldx == 318) sub_ptr => mydgemv_ver0_4_c_318
        if (ldx == 320) sub_ptr => mydgemv_ver0_4_c_320
        if (ldx == 322) sub_ptr => mydgemv_ver0_4_c_322
        if (ldx == 324) sub_ptr => mydgemv_ver0_4_c_324
        if (ldx == 326) sub_ptr => mydgemv_ver0_4_c_326
        if (ldx == 328) sub_ptr => mydgemv_ver0_4_c_328
        if (ldx == 330) sub_ptr => mydgemv_ver0_4_c_330
        if (ldx == 332) sub_ptr => mydgemv_ver0_4_c_332
        if (ldx == 334) sub_ptr => mydgemv_ver0_4_c_334
        if (ldx == 336) sub_ptr => mydgemv_ver0_4_c_336
        if (ldx == 338) sub_ptr => mydgemv_ver0_4_c_338
        if (ldx == 340) sub_ptr => mydgemv_ver0_4_c_340
        if (ldx == 342) sub_ptr => mydgemv_ver0_4_c_342
        if (ldx == 344) sub_ptr => mydgemv_ver0_4_c_344
        if (ldx == 346) sub_ptr => mydgemv_ver0_4_c_346
        if (ldx == 348) sub_ptr => mydgemv_ver0_4_c_348
        if (ldx == 350) sub_ptr => mydgemv_ver0_4_c_350
        if (ldx == 352) sub_ptr => mydgemv_ver0_4_c_352
        if (ldx == 354) sub_ptr => mydgemv_ver0_4_c_354
        if (ldx == 356) sub_ptr => mydgemv_ver0_4_c_356
        if (ldx == 358) sub_ptr => mydgemv_ver0_4_c_358
        if (ldx == 360) sub_ptr => mydgemv_ver0_4_c_360
        if (ldx == 362) sub_ptr => mydgemv_ver0_4_c_362
        if (ldx == 364) sub_ptr => mydgemv_ver0_4_c_364
        if (ldx == 366) sub_ptr => mydgemv_ver0_4_c_366
        if (ldx == 368) sub_ptr => mydgemv_ver0_4_c_368
        if (ldx == 370) sub_ptr => mydgemv_ver0_4_c_370
        if (ldx == 372) sub_ptr => mydgemv_ver0_4_c_372
        if (ldx == 374) sub_ptr => mydgemv_ver0_4_c_374
        if (ldx == 376) sub_ptr => mydgemv_ver0_4_c_376
        if (ldx == 378) sub_ptr => mydgemv_ver0_4_c_378
        if (ldx == 380) sub_ptr => mydgemv_ver0_4_c_380
        if (ldx == 382) sub_ptr => mydgemv_ver0_4_c_382
        if (ldx == 384) sub_ptr => mydgemv_ver0_4_c_384
        if (ldx == 386) sub_ptr => mydgemv_ver0_4_c_386
        if (ldx == 388) sub_ptr => mydgemv_ver0_4_c_388
        if (ldx == 390) sub_ptr => mydgemv_ver0_4_c_390
        if (ldx == 392) sub_ptr => mydgemv_ver0_4_c_392
        if (ldx == 394) sub_ptr => mydgemv_ver0_4_c_394
        if (ldx == 396) sub_ptr => mydgemv_ver0_4_c_396
        if (ldx == 398) sub_ptr => mydgemv_ver0_4_c_398
        if (ldx == 400) sub_ptr => mydgemv_ver0_4_c_400
        if (ldx == 402) sub_ptr => mydgemv_ver0_4_c_402
        if (ldx == 404) sub_ptr => mydgemv_ver0_4_c_404
        if (ldx == 406) sub_ptr => mydgemv_ver0_4_c_406
        if (ldx == 408) sub_ptr => mydgemv_ver0_4_c_408
        if (ldx == 410) sub_ptr => mydgemv_ver0_4_c_410
        if (ldx == 412) sub_ptr => mydgemv_ver0_4_c_412
        if (ldx == 414) sub_ptr => mydgemv_ver0_4_c_414
        if (ldx == 416) sub_ptr => mydgemv_ver0_4_c_416
        if (ldx == 418) sub_ptr => mydgemv_ver0_4_c_418
        if (ldx == 420) sub_ptr => mydgemv_ver0_4_c_420
        if (ldx == 422) sub_ptr => mydgemv_ver0_4_c_422
        if (ldx == 424) sub_ptr => mydgemv_ver0_4_c_424
        if (ldx == 426) sub_ptr => mydgemv_ver0_4_c_426
        if (ldx == 428) sub_ptr => mydgemv_ver0_4_c_428
        if (ldx == 430) sub_ptr => mydgemv_ver0_4_c_430
        if (ldx == 432) sub_ptr => mydgemv_ver0_4_c_432
        if (ldx == 434) sub_ptr => mydgemv_ver0_4_c_434
        if (ldx == 436) sub_ptr => mydgemv_ver0_4_c_436
        if (ldx == 438) sub_ptr => mydgemv_ver0_4_c_438
        if (ldx == 440) sub_ptr => mydgemv_ver0_4_c_440
        if (ldx == 442) sub_ptr => mydgemv_ver0_4_c_442
        if (ldx == 444) sub_ptr => mydgemv_ver0_4_c_444
        if (ldx == 446) sub_ptr => mydgemv_ver0_4_c_446
        if (ldx == 448) sub_ptr => mydgemv_ver0_4_c_448
        if (ldx == 450) sub_ptr => mydgemv_ver0_4_c_450
        if (ldx == 452) sub_ptr => mydgemv_ver0_4_c_452
        if (ldx == 454) sub_ptr => mydgemv_ver0_4_c_454
        if (ldx == 456) sub_ptr => mydgemv_ver0_4_c_456
        if (ldx == 458) sub_ptr => mydgemv_ver0_4_c_458
        if (ldx == 460) sub_ptr => mydgemv_ver0_4_c_460
        if (ldx == 462) sub_ptr => mydgemv_ver0_4_c_462
        if (ldx == 464) sub_ptr => mydgemv_ver0_4_c_464
        if (ldx == 466) sub_ptr => mydgemv_ver0_4_c_466
        if (ldx == 468) sub_ptr => mydgemv_ver0_4_c_468
        if (ldx == 470) sub_ptr => mydgemv_ver0_4_c_470
        if (ldx == 472) sub_ptr => mydgemv_ver0_4_c_472
        if (ldx == 474) sub_ptr => mydgemv_ver0_4_c_474
        if (ldx == 476) sub_ptr => mydgemv_ver0_4_c_476
        if (ldx == 478) sub_ptr => mydgemv_ver0_4_c_478
        if (ldx == 480) sub_ptr => mydgemv_ver0_4_c_480
        if (ldx == 482) sub_ptr => mydgemv_ver0_4_c_482
        if (ldx == 484) sub_ptr => mydgemv_ver0_4_c_484
        if (ldx == 486) sub_ptr => mydgemv_ver0_4_c_486
        if (ldx == 488) sub_ptr => mydgemv_ver0_4_c_488
        if (ldx == 490) sub_ptr => mydgemv_ver0_4_c_490
        if (ldx == 492) sub_ptr => mydgemv_ver0_4_c_492
        if (ldx == 494) sub_ptr => mydgemv_ver0_4_c_494
        if (ldx == 496) sub_ptr => mydgemv_ver0_4_c_496
        if (ldx == 498) sub_ptr => mydgemv_ver0_4_c_498
        if (ldx == 500) sub_ptr => mydgemv_ver0_4_c_500
        if (ldx == 502) sub_ptr => mydgemv_ver0_4_c_502
        if (ldx == 504) sub_ptr => mydgemv_ver0_4_c_504
        if (ldx == 506) sub_ptr => mydgemv_ver0_4_c_506
        if (ldx == 508) sub_ptr => mydgemv_ver0_4_c_508
        if (ldx == 510) sub_ptr => mydgemv_ver0_4_c_510
        if (ldx == 512) sub_ptr => mydgemv_ver0_4_c_512        
    end subroutine select_sub_ptr_t_v0_4

    subroutine select_sub_ptr_t_v0_5(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 2) sub_ptr => mydgemv_ver0_5_c_2
        if (ldx == 4) sub_ptr => mydgemv_ver0_5_c_4
        if (ldx == 6) sub_ptr => mydgemv_ver0_5_c_6
        if (ldx == 8) sub_ptr => mydgemv_ver0_5_c_8
        if (ldx == 10) sub_ptr => mydgemv_ver0_5_c_10
        if (ldx == 12) sub_ptr => mydgemv_ver0_5_c_12
        if (ldx == 14) sub_ptr => mydgemv_ver0_5_c_14
        if (ldx == 16) sub_ptr => mydgemv_ver0_5_c_16
        if (ldx == 18) sub_ptr => mydgemv_ver0_5_c_18
        if (ldx == 20) sub_ptr => mydgemv_ver0_5_c_20
        if (ldx == 22) sub_ptr => mydgemv_ver0_5_c_22
        if (ldx == 24) sub_ptr => mydgemv_ver0_5_c_24
        if (ldx == 26) sub_ptr => mydgemv_ver0_5_c_26
        if (ldx == 28) sub_ptr => mydgemv_ver0_5_c_28
        if (ldx == 30) sub_ptr => mydgemv_ver0_5_c_30
        if (ldx == 32) sub_ptr => mydgemv_ver0_5_c_32
        if (ldx == 34) sub_ptr => mydgemv_ver0_5_c_34
        if (ldx == 36) sub_ptr => mydgemv_ver0_5_c_36
        if (ldx == 38) sub_ptr => mydgemv_ver0_5_c_38
        if (ldx == 40) sub_ptr => mydgemv_ver0_5_c_40
        if (ldx == 42) sub_ptr => mydgemv_ver0_5_c_42
        if (ldx == 44) sub_ptr => mydgemv_ver0_5_c_44
        if (ldx == 46) sub_ptr => mydgemv_ver0_5_c_46
        if (ldx == 48) sub_ptr => mydgemv_ver0_5_c_48
        if (ldx == 50) sub_ptr => mydgemv_ver0_5_c_50
        if (ldx == 52) sub_ptr => mydgemv_ver0_5_c_52
        if (ldx == 54) sub_ptr => mydgemv_ver0_5_c_54
        if (ldx == 56) sub_ptr => mydgemv_ver0_5_c_56
        if (ldx == 58) sub_ptr => mydgemv_ver0_5_c_58
        if (ldx == 60) sub_ptr => mydgemv_ver0_5_c_60
        if (ldx == 62) sub_ptr => mydgemv_ver0_5_c_62
        if (ldx == 64) sub_ptr => mydgemv_ver0_5_c_64
        if (ldx == 66) sub_ptr => mydgemv_ver0_5_c_66
        if (ldx == 68) sub_ptr => mydgemv_ver0_5_c_68
        if (ldx == 70) sub_ptr => mydgemv_ver0_5_c_70
        if (ldx == 72) sub_ptr => mydgemv_ver0_5_c_72
        if (ldx == 74) sub_ptr => mydgemv_ver0_5_c_74
        if (ldx == 76) sub_ptr => mydgemv_ver0_5_c_76
        if (ldx == 78) sub_ptr => mydgemv_ver0_5_c_78
        if (ldx == 80) sub_ptr => mydgemv_ver0_5_c_80
        if (ldx == 82) sub_ptr => mydgemv_ver0_5_c_82
        if (ldx == 84) sub_ptr => mydgemv_ver0_5_c_84
        if (ldx == 86) sub_ptr => mydgemv_ver0_5_c_86
        if (ldx == 88) sub_ptr => mydgemv_ver0_5_c_88
        if (ldx == 90) sub_ptr => mydgemv_ver0_5_c_90
        if (ldx == 92) sub_ptr => mydgemv_ver0_5_c_92
        if (ldx == 94) sub_ptr => mydgemv_ver0_5_c_94
        if (ldx == 96) sub_ptr => mydgemv_ver0_5_c_96
        if (ldx == 98) sub_ptr => mydgemv_ver0_5_c_98
        if (ldx == 100) sub_ptr => mydgemv_ver0_5_c_100
        if (ldx == 102) sub_ptr => mydgemv_ver0_5_c_102
        if (ldx == 104) sub_ptr => mydgemv_ver0_5_c_104
        if (ldx == 106) sub_ptr => mydgemv_ver0_5_c_106
        if (ldx == 108) sub_ptr => mydgemv_ver0_5_c_108
        if (ldx == 110) sub_ptr => mydgemv_ver0_5_c_110
        if (ldx == 112) sub_ptr => mydgemv_ver0_5_c_112
        if (ldx == 114) sub_ptr => mydgemv_ver0_5_c_114
        if (ldx == 116) sub_ptr => mydgemv_ver0_5_c_116
        if (ldx == 118) sub_ptr => mydgemv_ver0_5_c_118
        if (ldx == 120) sub_ptr => mydgemv_ver0_5_c_120
        if (ldx == 122) sub_ptr => mydgemv_ver0_5_c_122
        if (ldx == 124) sub_ptr => mydgemv_ver0_5_c_124
        if (ldx == 126) sub_ptr => mydgemv_ver0_5_c_126
        if (ldx == 128) sub_ptr => mydgemv_ver0_5_c_128
        if (ldx == 130) sub_ptr => mydgemv_ver0_5_c_130
        if (ldx == 132) sub_ptr => mydgemv_ver0_5_c_132
        if (ldx == 134) sub_ptr => mydgemv_ver0_5_c_134
        if (ldx == 136) sub_ptr => mydgemv_ver0_5_c_136
        if (ldx == 138) sub_ptr => mydgemv_ver0_5_c_138
        if (ldx == 140) sub_ptr => mydgemv_ver0_5_c_140
        if (ldx == 142) sub_ptr => mydgemv_ver0_5_c_142
        if (ldx == 144) sub_ptr => mydgemv_ver0_5_c_144
        if (ldx == 146) sub_ptr => mydgemv_ver0_5_c_146
        if (ldx == 148) sub_ptr => mydgemv_ver0_5_c_148
        if (ldx == 150) sub_ptr => mydgemv_ver0_5_c_150
        if (ldx == 152) sub_ptr => mydgemv_ver0_5_c_152
        if (ldx == 154) sub_ptr => mydgemv_ver0_5_c_154
        if (ldx == 156) sub_ptr => mydgemv_ver0_5_c_156
        if (ldx == 158) sub_ptr => mydgemv_ver0_5_c_158
        if (ldx == 160) sub_ptr => mydgemv_ver0_5_c_160
        if (ldx == 162) sub_ptr => mydgemv_ver0_5_c_162
        if (ldx == 164) sub_ptr => mydgemv_ver0_5_c_164
        if (ldx == 166) sub_ptr => mydgemv_ver0_5_c_166
        if (ldx == 168) sub_ptr => mydgemv_ver0_5_c_168
        if (ldx == 170) sub_ptr => mydgemv_ver0_5_c_170
        if (ldx == 172) sub_ptr => mydgemv_ver0_5_c_172
        if (ldx == 174) sub_ptr => mydgemv_ver0_5_c_174
        if (ldx == 176) sub_ptr => mydgemv_ver0_5_c_176
        if (ldx == 178) sub_ptr => mydgemv_ver0_5_c_178
        if (ldx == 180) sub_ptr => mydgemv_ver0_5_c_180
        if (ldx == 182) sub_ptr => mydgemv_ver0_5_c_182
        if (ldx == 184) sub_ptr => mydgemv_ver0_5_c_184
        if (ldx == 186) sub_ptr => mydgemv_ver0_5_c_186
        if (ldx == 188) sub_ptr => mydgemv_ver0_5_c_188
        if (ldx == 190) sub_ptr => mydgemv_ver0_5_c_190
        if (ldx == 192) sub_ptr => mydgemv_ver0_5_c_192
        if (ldx == 194) sub_ptr => mydgemv_ver0_5_c_194
        if (ldx == 196) sub_ptr => mydgemv_ver0_5_c_196
        if (ldx == 198) sub_ptr => mydgemv_ver0_5_c_198
        if (ldx == 200) sub_ptr => mydgemv_ver0_5_c_200
        if (ldx == 202) sub_ptr => mydgemv_ver0_5_c_202
        if (ldx == 204) sub_ptr => mydgemv_ver0_5_c_204
        if (ldx == 206) sub_ptr => mydgemv_ver0_5_c_206
        if (ldx == 208) sub_ptr => mydgemv_ver0_5_c_208
        if (ldx == 210) sub_ptr => mydgemv_ver0_5_c_210
        if (ldx == 212) sub_ptr => mydgemv_ver0_5_c_212
        if (ldx == 214) sub_ptr => mydgemv_ver0_5_c_214
        if (ldx == 216) sub_ptr => mydgemv_ver0_5_c_216
        if (ldx == 218) sub_ptr => mydgemv_ver0_5_c_218
        if (ldx == 220) sub_ptr => mydgemv_ver0_5_c_220
        if (ldx == 222) sub_ptr => mydgemv_ver0_5_c_222
        if (ldx == 224) sub_ptr => mydgemv_ver0_5_c_224
        if (ldx == 226) sub_ptr => mydgemv_ver0_5_c_226
        if (ldx == 228) sub_ptr => mydgemv_ver0_5_c_228
        if (ldx == 230) sub_ptr => mydgemv_ver0_5_c_230
        if (ldx == 232) sub_ptr => mydgemv_ver0_5_c_232
        if (ldx == 234) sub_ptr => mydgemv_ver0_5_c_234
        if (ldx == 236) sub_ptr => mydgemv_ver0_5_c_236
        if (ldx == 238) sub_ptr => mydgemv_ver0_5_c_238
        if (ldx == 240) sub_ptr => mydgemv_ver0_5_c_240
        if (ldx == 242) sub_ptr => mydgemv_ver0_5_c_242
        if (ldx == 244) sub_ptr => mydgemv_ver0_5_c_244
        if (ldx == 246) sub_ptr => mydgemv_ver0_5_c_246
        if (ldx == 248) sub_ptr => mydgemv_ver0_5_c_248
        if (ldx == 250) sub_ptr => mydgemv_ver0_5_c_250
        if (ldx == 252) sub_ptr => mydgemv_ver0_5_c_252
        if (ldx == 254) sub_ptr => mydgemv_ver0_5_c_254
        if (ldx == 256) sub_ptr => mydgemv_ver0_5_c_256
        if (ldx == 258) sub_ptr => mydgemv_ver0_5_c_258
        if (ldx == 260) sub_ptr => mydgemv_ver0_5_c_260
        if (ldx == 262) sub_ptr => mydgemv_ver0_5_c_262
        if (ldx == 264) sub_ptr => mydgemv_ver0_5_c_264
        if (ldx == 266) sub_ptr => mydgemv_ver0_5_c_266
        if (ldx == 268) sub_ptr => mydgemv_ver0_5_c_268
        if (ldx == 270) sub_ptr => mydgemv_ver0_5_c_270
        if (ldx == 272) sub_ptr => mydgemv_ver0_5_c_272
        if (ldx == 274) sub_ptr => mydgemv_ver0_5_c_274
        if (ldx == 276) sub_ptr => mydgemv_ver0_5_c_276
        if (ldx == 278) sub_ptr => mydgemv_ver0_5_c_278
        if (ldx == 280) sub_ptr => mydgemv_ver0_5_c_280
        if (ldx == 282) sub_ptr => mydgemv_ver0_5_c_282
        if (ldx == 284) sub_ptr => mydgemv_ver0_5_c_284
        if (ldx == 286) sub_ptr => mydgemv_ver0_5_c_286
        if (ldx == 288) sub_ptr => mydgemv_ver0_5_c_288
        if (ldx == 290) sub_ptr => mydgemv_ver0_5_c_290
        if (ldx == 292) sub_ptr => mydgemv_ver0_5_c_292
        if (ldx == 294) sub_ptr => mydgemv_ver0_5_c_294
        if (ldx == 296) sub_ptr => mydgemv_ver0_5_c_296
        if (ldx == 298) sub_ptr => mydgemv_ver0_5_c_298
        if (ldx == 300) sub_ptr => mydgemv_ver0_5_c_300
        if (ldx == 302) sub_ptr => mydgemv_ver0_5_c_302
        if (ldx == 304) sub_ptr => mydgemv_ver0_5_c_304
        if (ldx == 306) sub_ptr => mydgemv_ver0_5_c_306
        if (ldx == 308) sub_ptr => mydgemv_ver0_5_c_308
        if (ldx == 310) sub_ptr => mydgemv_ver0_5_c_310
        if (ldx == 312) sub_ptr => mydgemv_ver0_5_c_312
        if (ldx == 314) sub_ptr => mydgemv_ver0_5_c_314
        if (ldx == 316) sub_ptr => mydgemv_ver0_5_c_316
        if (ldx == 318) sub_ptr => mydgemv_ver0_5_c_318
        if (ldx == 320) sub_ptr => mydgemv_ver0_5_c_320
        if (ldx == 322) sub_ptr => mydgemv_ver0_5_c_322
        if (ldx == 324) sub_ptr => mydgemv_ver0_5_c_324
        if (ldx == 326) sub_ptr => mydgemv_ver0_5_c_326
        if (ldx == 328) sub_ptr => mydgemv_ver0_5_c_328
        if (ldx == 330) sub_ptr => mydgemv_ver0_5_c_330
        if (ldx == 332) sub_ptr => mydgemv_ver0_5_c_332
        if (ldx == 334) sub_ptr => mydgemv_ver0_5_c_334
        if (ldx == 336) sub_ptr => mydgemv_ver0_5_c_336
        if (ldx == 338) sub_ptr => mydgemv_ver0_5_c_338
        if (ldx == 340) sub_ptr => mydgemv_ver0_5_c_340
        if (ldx == 342) sub_ptr => mydgemv_ver0_5_c_342
        if (ldx == 344) sub_ptr => mydgemv_ver0_5_c_344
        if (ldx == 346) sub_ptr => mydgemv_ver0_5_c_346
        if (ldx == 348) sub_ptr => mydgemv_ver0_5_c_348
        if (ldx == 350) sub_ptr => mydgemv_ver0_5_c_350
        if (ldx == 352) sub_ptr => mydgemv_ver0_5_c_352
        if (ldx == 354) sub_ptr => mydgemv_ver0_5_c_354
        if (ldx == 356) sub_ptr => mydgemv_ver0_5_c_356
        if (ldx == 358) sub_ptr => mydgemv_ver0_5_c_358
        if (ldx == 360) sub_ptr => mydgemv_ver0_5_c_360
        if (ldx == 362) sub_ptr => mydgemv_ver0_5_c_362
        if (ldx == 364) sub_ptr => mydgemv_ver0_5_c_364
        if (ldx == 366) sub_ptr => mydgemv_ver0_5_c_366
        if (ldx == 368) sub_ptr => mydgemv_ver0_5_c_368
        if (ldx == 370) sub_ptr => mydgemv_ver0_5_c_370
        if (ldx == 372) sub_ptr => mydgemv_ver0_5_c_372
        if (ldx == 374) sub_ptr => mydgemv_ver0_5_c_374
        if (ldx == 376) sub_ptr => mydgemv_ver0_5_c_376
        if (ldx == 378) sub_ptr => mydgemv_ver0_5_c_378
        if (ldx == 380) sub_ptr => mydgemv_ver0_5_c_380
        if (ldx == 382) sub_ptr => mydgemv_ver0_5_c_382
        if (ldx == 384) sub_ptr => mydgemv_ver0_5_c_384
        if (ldx == 386) sub_ptr => mydgemv_ver0_5_c_386
        if (ldx == 388) sub_ptr => mydgemv_ver0_5_c_388
        if (ldx == 390) sub_ptr => mydgemv_ver0_5_c_390
        if (ldx == 392) sub_ptr => mydgemv_ver0_5_c_392
        if (ldx == 394) sub_ptr => mydgemv_ver0_5_c_394
        if (ldx == 396) sub_ptr => mydgemv_ver0_5_c_396
        if (ldx == 398) sub_ptr => mydgemv_ver0_5_c_398
        if (ldx == 400) sub_ptr => mydgemv_ver0_5_c_400
        if (ldx == 402) sub_ptr => mydgemv_ver0_5_c_402
        if (ldx == 404) sub_ptr => mydgemv_ver0_5_c_404
        if (ldx == 406) sub_ptr => mydgemv_ver0_5_c_406
        if (ldx == 408) sub_ptr => mydgemv_ver0_5_c_408
        if (ldx == 410) sub_ptr => mydgemv_ver0_5_c_410
        if (ldx == 412) sub_ptr => mydgemv_ver0_5_c_412
        if (ldx == 414) sub_ptr => mydgemv_ver0_5_c_414
        if (ldx == 416) sub_ptr => mydgemv_ver0_5_c_416
        if (ldx == 418) sub_ptr => mydgemv_ver0_5_c_418
        if (ldx == 420) sub_ptr => mydgemv_ver0_5_c_420
        if (ldx == 422) sub_ptr => mydgemv_ver0_5_c_422
        if (ldx == 424) sub_ptr => mydgemv_ver0_5_c_424
        if (ldx == 426) sub_ptr => mydgemv_ver0_5_c_426
        if (ldx == 428) sub_ptr => mydgemv_ver0_5_c_428
        if (ldx == 430) sub_ptr => mydgemv_ver0_5_c_430
        if (ldx == 432) sub_ptr => mydgemv_ver0_5_c_432
        if (ldx == 434) sub_ptr => mydgemv_ver0_5_c_434
        if (ldx == 436) sub_ptr => mydgemv_ver0_5_c_436
        if (ldx == 438) sub_ptr => mydgemv_ver0_5_c_438
        if (ldx == 440) sub_ptr => mydgemv_ver0_5_c_440
        if (ldx == 442) sub_ptr => mydgemv_ver0_5_c_442
        if (ldx == 444) sub_ptr => mydgemv_ver0_5_c_444
        if (ldx == 446) sub_ptr => mydgemv_ver0_5_c_446
        if (ldx == 448) sub_ptr => mydgemv_ver0_5_c_448
        if (ldx == 450) sub_ptr => mydgemv_ver0_5_c_450
        if (ldx == 452) sub_ptr => mydgemv_ver0_5_c_452
        if (ldx == 454) sub_ptr => mydgemv_ver0_5_c_454
        if (ldx == 456) sub_ptr => mydgemv_ver0_5_c_456
        if (ldx == 458) sub_ptr => mydgemv_ver0_5_c_458
        if (ldx == 460) sub_ptr => mydgemv_ver0_5_c_460
        if (ldx == 462) sub_ptr => mydgemv_ver0_5_c_462
        if (ldx == 464) sub_ptr => mydgemv_ver0_5_c_464
        if (ldx == 466) sub_ptr => mydgemv_ver0_5_c_466
        if (ldx == 468) sub_ptr => mydgemv_ver0_5_c_468
        if (ldx == 470) sub_ptr => mydgemv_ver0_5_c_470
        if (ldx == 472) sub_ptr => mydgemv_ver0_5_c_472
        if (ldx == 474) sub_ptr => mydgemv_ver0_5_c_474
        if (ldx == 476) sub_ptr => mydgemv_ver0_5_c_476
        if (ldx == 478) sub_ptr => mydgemv_ver0_5_c_478
        if (ldx == 480) sub_ptr => mydgemv_ver0_5_c_480
        if (ldx == 482) sub_ptr => mydgemv_ver0_5_c_482
        if (ldx == 484) sub_ptr => mydgemv_ver0_5_c_484
        if (ldx == 486) sub_ptr => mydgemv_ver0_5_c_486
        if (ldx == 488) sub_ptr => mydgemv_ver0_5_c_488
        if (ldx == 490) sub_ptr => mydgemv_ver0_5_c_490
        if (ldx == 492) sub_ptr => mydgemv_ver0_5_c_492
        if (ldx == 494) sub_ptr => mydgemv_ver0_5_c_494
        if (ldx == 496) sub_ptr => mydgemv_ver0_5_c_496
        if (ldx == 498) sub_ptr => mydgemv_ver0_5_c_498
        if (ldx == 500) sub_ptr => mydgemv_ver0_5_c_500
        if (ldx == 502) sub_ptr => mydgemv_ver0_5_c_502
        if (ldx == 504) sub_ptr => mydgemv_ver0_5_c_504
        if (ldx == 506) sub_ptr => mydgemv_ver0_5_c_506
        if (ldx == 508) sub_ptr => mydgemv_ver0_5_c_508
        if (ldx == 510) sub_ptr => mydgemv_ver0_5_c_510
        if (ldx == 512) sub_ptr => mydgemv_ver0_5_c_512        
    end subroutine select_sub_ptr_t_v0_5

    subroutine select_sub_ptr_t_v1_4(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 2) sub_ptr => mydgemv_t_ver1_4_2
        if (ldx == 4) sub_ptr => mydgemv_t_ver1_4_4
        if (ldx == 6) sub_ptr => mydgemv_t_ver1_4_6
        if (ldx == 8) sub_ptr => mydgemv_t_ver1_4_8
        if (ldx == 10) sub_ptr => mydgemv_t_ver1_4_10
        if (ldx == 12) sub_ptr => mydgemv_t_ver1_4_12
        if (ldx == 14) sub_ptr => mydgemv_t_ver1_4_14
        if (ldx == 16) sub_ptr => mydgemv_t_ver1_4_16
        if (ldx == 18) sub_ptr => mydgemv_t_ver1_4_18
        if (ldx == 20) sub_ptr => mydgemv_t_ver1_4_20
        if (ldx == 22) sub_ptr => mydgemv_t_ver1_4_22
        if (ldx == 24) sub_ptr => mydgemv_t_ver1_4_24
        if (ldx == 26) sub_ptr => mydgemv_t_ver1_4_26
        if (ldx == 28) sub_ptr => mydgemv_t_ver1_4_28
        if (ldx == 30) sub_ptr => mydgemv_t_ver1_4_30
        if (ldx == 32) sub_ptr => mydgemv_t_ver1_4_32
        if (ldx == 34) sub_ptr => mydgemv_t_ver1_4_34
        if (ldx == 36) sub_ptr => mydgemv_t_ver1_4_36
        if (ldx == 38) sub_ptr => mydgemv_t_ver1_4_38
        if (ldx == 40) sub_ptr => mydgemv_t_ver1_4_40
        if (ldx == 42) sub_ptr => mydgemv_t_ver1_4_42
        if (ldx == 44) sub_ptr => mydgemv_t_ver1_4_44
        if (ldx == 46) sub_ptr => mydgemv_t_ver1_4_46
        if (ldx == 48) sub_ptr => mydgemv_t_ver1_4_48
        if (ldx == 50) sub_ptr => mydgemv_t_ver1_4_50
        if (ldx == 52) sub_ptr => mydgemv_t_ver1_4_52
        if (ldx == 54) sub_ptr => mydgemv_t_ver1_4_54
        if (ldx == 56) sub_ptr => mydgemv_t_ver1_4_56
        if (ldx == 58) sub_ptr => mydgemv_t_ver1_4_58
        if (ldx == 60) sub_ptr => mydgemv_t_ver1_4_60
        if (ldx == 62) sub_ptr => mydgemv_t_ver1_4_62
        if (ldx == 64) sub_ptr => mydgemv_t_ver1_4_64
        if (ldx == 66) sub_ptr => mydgemv_t_ver1_4_66
        if (ldx == 68) sub_ptr => mydgemv_t_ver1_4_68
        if (ldx == 70) sub_ptr => mydgemv_t_ver1_4_70
        if (ldx == 72) sub_ptr => mydgemv_t_ver1_4_72
        if (ldx == 74) sub_ptr => mydgemv_t_ver1_4_74
        if (ldx == 76) sub_ptr => mydgemv_t_ver1_4_76
        if (ldx == 78) sub_ptr => mydgemv_t_ver1_4_78
        if (ldx == 80) sub_ptr => mydgemv_t_ver1_4_80
        if (ldx == 82) sub_ptr => mydgemv_t_ver1_4_82
        if (ldx == 84) sub_ptr => mydgemv_t_ver1_4_84
        if (ldx == 86) sub_ptr => mydgemv_t_ver1_4_86
        if (ldx == 88) sub_ptr => mydgemv_t_ver1_4_88
        if (ldx == 90) sub_ptr => mydgemv_t_ver1_4_90
        if (ldx == 92) sub_ptr => mydgemv_t_ver1_4_92
        if (ldx == 94) sub_ptr => mydgemv_t_ver1_4_94
        if (ldx == 96) sub_ptr => mydgemv_t_ver1_4_96
        if (ldx == 98) sub_ptr => mydgemv_t_ver1_4_98
        if (ldx == 100) sub_ptr => mydgemv_t_ver1_4_100
        if (ldx == 102) sub_ptr => mydgemv_t_ver1_4_102
        if (ldx == 104) sub_ptr => mydgemv_t_ver1_4_104
        if (ldx == 106) sub_ptr => mydgemv_t_ver1_4_106
        if (ldx == 108) sub_ptr => mydgemv_t_ver1_4_108
        if (ldx == 110) sub_ptr => mydgemv_t_ver1_4_110
        if (ldx == 112) sub_ptr => mydgemv_t_ver1_4_112
        if (ldx == 114) sub_ptr => mydgemv_t_ver1_4_114
        if (ldx == 116) sub_ptr => mydgemv_t_ver1_4_116
        if (ldx == 118) sub_ptr => mydgemv_t_ver1_4_118
        if (ldx == 120) sub_ptr => mydgemv_t_ver1_4_120
        if (ldx == 122) sub_ptr => mydgemv_t_ver1_4_122
        if (ldx == 124) sub_ptr => mydgemv_t_ver1_4_124
        if (ldx == 126) sub_ptr => mydgemv_t_ver1_4_126
        if (ldx == 128) sub_ptr => mydgemv_t_ver1_4_128
        if (ldx == 130) sub_ptr => mydgemv_t_ver1_4_130
        if (ldx == 132) sub_ptr => mydgemv_t_ver1_4_132
        if (ldx == 134) sub_ptr => mydgemv_t_ver1_4_134
        if (ldx == 136) sub_ptr => mydgemv_t_ver1_4_136
        if (ldx == 138) sub_ptr => mydgemv_t_ver1_4_138
        if (ldx == 140) sub_ptr => mydgemv_t_ver1_4_140
        if (ldx == 142) sub_ptr => mydgemv_t_ver1_4_142
        if (ldx == 144) sub_ptr => mydgemv_t_ver1_4_144
        if (ldx == 146) sub_ptr => mydgemv_t_ver1_4_146
        if (ldx == 148) sub_ptr => mydgemv_t_ver1_4_148
        if (ldx == 150) sub_ptr => mydgemv_t_ver1_4_150
        if (ldx == 152) sub_ptr => mydgemv_t_ver1_4_152
        if (ldx == 154) sub_ptr => mydgemv_t_ver1_4_154
        if (ldx == 156) sub_ptr => mydgemv_t_ver1_4_156
        if (ldx == 158) sub_ptr => mydgemv_t_ver1_4_158
        if (ldx == 160) sub_ptr => mydgemv_t_ver1_4_160
        if (ldx == 162) sub_ptr => mydgemv_t_ver1_4_162
        if (ldx == 164) sub_ptr => mydgemv_t_ver1_4_164
        if (ldx == 166) sub_ptr => mydgemv_t_ver1_4_166
        if (ldx == 168) sub_ptr => mydgemv_t_ver1_4_168
        if (ldx == 170) sub_ptr => mydgemv_t_ver1_4_170
        if (ldx == 172) sub_ptr => mydgemv_t_ver1_4_172
        if (ldx == 174) sub_ptr => mydgemv_t_ver1_4_174
        if (ldx == 176) sub_ptr => mydgemv_t_ver1_4_176
        if (ldx == 178) sub_ptr => mydgemv_t_ver1_4_178
        if (ldx == 180) sub_ptr => mydgemv_t_ver1_4_180
        if (ldx == 182) sub_ptr => mydgemv_t_ver1_4_182
        if (ldx == 184) sub_ptr => mydgemv_t_ver1_4_184
        if (ldx == 186) sub_ptr => mydgemv_t_ver1_4_186
        if (ldx == 188) sub_ptr => mydgemv_t_ver1_4_188
        if (ldx == 190) sub_ptr => mydgemv_t_ver1_4_190
        if (ldx == 192) sub_ptr => mydgemv_t_ver1_4_192
        if (ldx == 194) sub_ptr => mydgemv_t_ver1_4_194
        if (ldx == 196) sub_ptr => mydgemv_t_ver1_4_196
        if (ldx == 198) sub_ptr => mydgemv_t_ver1_4_198
        if (ldx == 200) sub_ptr => mydgemv_t_ver1_4_200
        if (ldx == 202) sub_ptr => mydgemv_t_ver1_4_202
        if (ldx == 204) sub_ptr => mydgemv_t_ver1_4_204
        if (ldx == 206) sub_ptr => mydgemv_t_ver1_4_206
        if (ldx == 208) sub_ptr => mydgemv_t_ver1_4_208
        if (ldx == 210) sub_ptr => mydgemv_t_ver1_4_210
        if (ldx == 212) sub_ptr => mydgemv_t_ver1_4_212
        if (ldx == 214) sub_ptr => mydgemv_t_ver1_4_214
        if (ldx == 216) sub_ptr => mydgemv_t_ver1_4_216
        if (ldx == 218) sub_ptr => mydgemv_t_ver1_4_218
        if (ldx == 220) sub_ptr => mydgemv_t_ver1_4_220
        if (ldx == 222) sub_ptr => mydgemv_t_ver1_4_222
        if (ldx == 224) sub_ptr => mydgemv_t_ver1_4_224
        if (ldx == 226) sub_ptr => mydgemv_t_ver1_4_226
        if (ldx == 228) sub_ptr => mydgemv_t_ver1_4_228
        if (ldx == 230) sub_ptr => mydgemv_t_ver1_4_230
        if (ldx == 232) sub_ptr => mydgemv_t_ver1_4_232
        if (ldx == 234) sub_ptr => mydgemv_t_ver1_4_234
        if (ldx == 236) sub_ptr => mydgemv_t_ver1_4_236
        if (ldx == 238) sub_ptr => mydgemv_t_ver1_4_238
        if (ldx == 240) sub_ptr => mydgemv_t_ver1_4_240
        if (ldx == 242) sub_ptr => mydgemv_t_ver1_4_242
        if (ldx == 244) sub_ptr => mydgemv_t_ver1_4_244
        if (ldx == 246) sub_ptr => mydgemv_t_ver1_4_246
        if (ldx == 248) sub_ptr => mydgemv_t_ver1_4_248
        if (ldx == 250) sub_ptr => mydgemv_t_ver1_4_250
        if (ldx == 252) sub_ptr => mydgemv_t_ver1_4_252
        if (ldx == 254) sub_ptr => mydgemv_t_ver1_4_254
        if (ldx == 256) sub_ptr => mydgemv_t_ver1_4_256
        if (ldx == 258) sub_ptr => mydgemv_t_ver1_4_258
        if (ldx == 260) sub_ptr => mydgemv_t_ver1_4_260
        if (ldx == 262) sub_ptr => mydgemv_t_ver1_4_262
        if (ldx == 264) sub_ptr => mydgemv_t_ver1_4_264
        if (ldx == 266) sub_ptr => mydgemv_t_ver1_4_266
        if (ldx == 268) sub_ptr => mydgemv_t_ver1_4_268
        if (ldx == 270) sub_ptr => mydgemv_t_ver1_4_270
        if (ldx == 272) sub_ptr => mydgemv_t_ver1_4_272
        if (ldx == 274) sub_ptr => mydgemv_t_ver1_4_274
        if (ldx == 276) sub_ptr => mydgemv_t_ver1_4_276
        if (ldx == 278) sub_ptr => mydgemv_t_ver1_4_278
        if (ldx == 280) sub_ptr => mydgemv_t_ver1_4_280
        if (ldx == 282) sub_ptr => mydgemv_t_ver1_4_282
        if (ldx == 284) sub_ptr => mydgemv_t_ver1_4_284
        if (ldx == 286) sub_ptr => mydgemv_t_ver1_4_286
        if (ldx == 288) sub_ptr => mydgemv_t_ver1_4_288
        if (ldx == 290) sub_ptr => mydgemv_t_ver1_4_290
        if (ldx == 292) sub_ptr => mydgemv_t_ver1_4_292
        if (ldx == 294) sub_ptr => mydgemv_t_ver1_4_294
        if (ldx == 296) sub_ptr => mydgemv_t_ver1_4_296
        if (ldx == 298) sub_ptr => mydgemv_t_ver1_4_298
        if (ldx == 300) sub_ptr => mydgemv_t_ver1_4_300
        if (ldx == 302) sub_ptr => mydgemv_t_ver1_4_302
        if (ldx == 304) sub_ptr => mydgemv_t_ver1_4_304
        if (ldx == 306) sub_ptr => mydgemv_t_ver1_4_306
        if (ldx == 308) sub_ptr => mydgemv_t_ver1_4_308
        if (ldx == 310) sub_ptr => mydgemv_t_ver1_4_310
        if (ldx == 312) sub_ptr => mydgemv_t_ver1_4_312
        if (ldx == 314) sub_ptr => mydgemv_t_ver1_4_314
        if (ldx == 316) sub_ptr => mydgemv_t_ver1_4_316
        if (ldx == 318) sub_ptr => mydgemv_t_ver1_4_318
        if (ldx == 320) sub_ptr => mydgemv_t_ver1_4_320
        if (ldx == 322) sub_ptr => mydgemv_t_ver1_4_322
        if (ldx == 324) sub_ptr => mydgemv_t_ver1_4_324
        if (ldx == 326) sub_ptr => mydgemv_t_ver1_4_326
        if (ldx == 328) sub_ptr => mydgemv_t_ver1_4_328
        if (ldx == 330) sub_ptr => mydgemv_t_ver1_4_330
        if (ldx == 332) sub_ptr => mydgemv_t_ver1_4_332
        if (ldx == 334) sub_ptr => mydgemv_t_ver1_4_334
        if (ldx == 336) sub_ptr => mydgemv_t_ver1_4_336
        if (ldx == 338) sub_ptr => mydgemv_t_ver1_4_338
        if (ldx == 340) sub_ptr => mydgemv_t_ver1_4_340
        if (ldx == 342) sub_ptr => mydgemv_t_ver1_4_342
        if (ldx == 344) sub_ptr => mydgemv_t_ver1_4_344
        if (ldx == 346) sub_ptr => mydgemv_t_ver1_4_346
        if (ldx == 348) sub_ptr => mydgemv_t_ver1_4_348
        if (ldx == 350) sub_ptr => mydgemv_t_ver1_4_350
        if (ldx == 352) sub_ptr => mydgemv_t_ver1_4_352
        if (ldx == 354) sub_ptr => mydgemv_t_ver1_4_354
        if (ldx == 356) sub_ptr => mydgemv_t_ver1_4_356
        if (ldx == 358) sub_ptr => mydgemv_t_ver1_4_358
        if (ldx == 360) sub_ptr => mydgemv_t_ver1_4_360
        if (ldx == 362) sub_ptr => mydgemv_t_ver1_4_362
        if (ldx == 364) sub_ptr => mydgemv_t_ver1_4_364
        if (ldx == 366) sub_ptr => mydgemv_t_ver1_4_366
        if (ldx == 368) sub_ptr => mydgemv_t_ver1_4_368
        if (ldx == 370) sub_ptr => mydgemv_t_ver1_4_370
        if (ldx == 372) sub_ptr => mydgemv_t_ver1_4_372
        if (ldx == 374) sub_ptr => mydgemv_t_ver1_4_374
        if (ldx == 376) sub_ptr => mydgemv_t_ver1_4_376
        if (ldx == 378) sub_ptr => mydgemv_t_ver1_4_378
        if (ldx == 380) sub_ptr => mydgemv_t_ver1_4_380
        if (ldx == 382) sub_ptr => mydgemv_t_ver1_4_382
        if (ldx == 384) sub_ptr => mydgemv_t_ver1_4_384
        if (ldx == 386) sub_ptr => mydgemv_t_ver1_4_386
        if (ldx == 388) sub_ptr => mydgemv_t_ver1_4_388
        if (ldx == 390) sub_ptr => mydgemv_t_ver1_4_390
        if (ldx == 392) sub_ptr => mydgemv_t_ver1_4_392
        if (ldx == 394) sub_ptr => mydgemv_t_ver1_4_394
        if (ldx == 396) sub_ptr => mydgemv_t_ver1_4_396
        if (ldx == 398) sub_ptr => mydgemv_t_ver1_4_398
        if (ldx == 400) sub_ptr => mydgemv_t_ver1_4_400
        if (ldx == 402) sub_ptr => mydgemv_t_ver1_4_402
        if (ldx == 404) sub_ptr => mydgemv_t_ver1_4_404
        if (ldx == 406) sub_ptr => mydgemv_t_ver1_4_406
        if (ldx == 408) sub_ptr => mydgemv_t_ver1_4_408
        if (ldx == 410) sub_ptr => mydgemv_t_ver1_4_410
        if (ldx == 412) sub_ptr => mydgemv_t_ver1_4_412
        if (ldx == 414) sub_ptr => mydgemv_t_ver1_4_414
        if (ldx == 416) sub_ptr => mydgemv_t_ver1_4_416
        if (ldx == 418) sub_ptr => mydgemv_t_ver1_4_418
        if (ldx == 420) sub_ptr => mydgemv_t_ver1_4_420
        if (ldx == 422) sub_ptr => mydgemv_t_ver1_4_422
        if (ldx == 424) sub_ptr => mydgemv_t_ver1_4_424
        if (ldx == 426) sub_ptr => mydgemv_t_ver1_4_426
        if (ldx == 428) sub_ptr => mydgemv_t_ver1_4_428
        if (ldx == 430) sub_ptr => mydgemv_t_ver1_4_430
        if (ldx == 432) sub_ptr => mydgemv_t_ver1_4_432
        if (ldx == 434) sub_ptr => mydgemv_t_ver1_4_434
        if (ldx == 436) sub_ptr => mydgemv_t_ver1_4_436
        if (ldx == 438) sub_ptr => mydgemv_t_ver1_4_438
        if (ldx == 440) sub_ptr => mydgemv_t_ver1_4_440
        if (ldx == 442) sub_ptr => mydgemv_t_ver1_4_442
        if (ldx == 444) sub_ptr => mydgemv_t_ver1_4_444
        if (ldx == 446) sub_ptr => mydgemv_t_ver1_4_446
        if (ldx == 448) sub_ptr => mydgemv_t_ver1_4_448
        if (ldx == 450) sub_ptr => mydgemv_t_ver1_4_450
        if (ldx == 452) sub_ptr => mydgemv_t_ver1_4_452
        if (ldx == 454) sub_ptr => mydgemv_t_ver1_4_454
        if (ldx == 456) sub_ptr => mydgemv_t_ver1_4_456
        if (ldx == 458) sub_ptr => mydgemv_t_ver1_4_458
        if (ldx == 460) sub_ptr => mydgemv_t_ver1_4_460
        if (ldx == 462) sub_ptr => mydgemv_t_ver1_4_462
        if (ldx == 464) sub_ptr => mydgemv_t_ver1_4_464
        if (ldx == 466) sub_ptr => mydgemv_t_ver1_4_466
        if (ldx == 468) sub_ptr => mydgemv_t_ver1_4_468
        if (ldx == 470) sub_ptr => mydgemv_t_ver1_4_470
        if (ldx == 472) sub_ptr => mydgemv_t_ver1_4_472
        if (ldx == 474) sub_ptr => mydgemv_t_ver1_4_474
        if (ldx == 476) sub_ptr => mydgemv_t_ver1_4_476
        if (ldx == 478) sub_ptr => mydgemv_t_ver1_4_478
        if (ldx == 480) sub_ptr => mydgemv_t_ver1_4_480
        if (ldx == 482) sub_ptr => mydgemv_t_ver1_4_482
        if (ldx == 484) sub_ptr => mydgemv_t_ver1_4_484
        if (ldx == 486) sub_ptr => mydgemv_t_ver1_4_486
        if (ldx == 488) sub_ptr => mydgemv_t_ver1_4_488
        if (ldx == 490) sub_ptr => mydgemv_t_ver1_4_490
        if (ldx == 492) sub_ptr => mydgemv_t_ver1_4_492
        if (ldx == 494) sub_ptr => mydgemv_t_ver1_4_494
        if (ldx == 496) sub_ptr => mydgemv_t_ver1_4_496
        if (ldx == 498) sub_ptr => mydgemv_t_ver1_4_498
        if (ldx == 500) sub_ptr => mydgemv_t_ver1_4_500
        if (ldx == 502) sub_ptr => mydgemv_t_ver1_4_502
        if (ldx == 504) sub_ptr => mydgemv_t_ver1_4_504
        if (ldx == 506) sub_ptr => mydgemv_t_ver1_4_506
        if (ldx == 508) sub_ptr => mydgemv_t_ver1_4_508
        if (ldx == 510) sub_ptr => mydgemv_t_ver1_4_510
        if (ldx == 512) sub_ptr => mydgemv_t_ver1_4_512        
    end subroutine select_sub_ptr_t_v1_4

    subroutine select_sub_ptr_n_v1_4(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 2) sub_ptr => mydgemv_n_ver1_4_2
        if (ldx == 4) sub_ptr => mydgemv_n_ver1_4_4
        if (ldx == 6) sub_ptr => mydgemv_n_ver1_4_6
        if (ldx == 8) sub_ptr => mydgemv_n_ver1_4_8
        if (ldx == 10) sub_ptr => mydgemv_n_ver1_4_10
        if (ldx == 12) sub_ptr => mydgemv_n_ver1_4_12
        if (ldx == 14) sub_ptr => mydgemv_n_ver1_4_14
        if (ldx == 16) sub_ptr => mydgemv_n_ver1_4_16
        if (ldx == 18) sub_ptr => mydgemv_n_ver1_4_18
        if (ldx == 20) sub_ptr => mydgemv_n_ver1_4_20
        if (ldx == 22) sub_ptr => mydgemv_n_ver1_4_22
        if (ldx == 24) sub_ptr => mydgemv_n_ver1_4_24
        if (ldx == 26) sub_ptr => mydgemv_n_ver1_4_26
        if (ldx == 28) sub_ptr => mydgemv_n_ver1_4_28
        if (ldx == 30) sub_ptr => mydgemv_n_ver1_4_30
        if (ldx == 32) sub_ptr => mydgemv_n_ver1_4_32
        if (ldx == 34) sub_ptr => mydgemv_n_ver1_4_34
        if (ldx == 36) sub_ptr => mydgemv_n_ver1_4_36
        if (ldx == 38) sub_ptr => mydgemv_n_ver1_4_38
        if (ldx == 40) sub_ptr => mydgemv_n_ver1_4_40
        if (ldx == 42) sub_ptr => mydgemv_n_ver1_4_42
        if (ldx == 44) sub_ptr => mydgemv_n_ver1_4_44
        if (ldx == 46) sub_ptr => mydgemv_n_ver1_4_46
        if (ldx == 48) sub_ptr => mydgemv_n_ver1_4_48
        if (ldx == 50) sub_ptr => mydgemv_n_ver1_4_50
        if (ldx == 52) sub_ptr => mydgemv_n_ver1_4_52
        if (ldx == 54) sub_ptr => mydgemv_n_ver1_4_54
        if (ldx == 56) sub_ptr => mydgemv_n_ver1_4_56
        if (ldx == 58) sub_ptr => mydgemv_n_ver1_4_58
        if (ldx == 60) sub_ptr => mydgemv_n_ver1_4_60
        if (ldx == 62) sub_ptr => mydgemv_n_ver1_4_62
        if (ldx == 64) sub_ptr => mydgemv_n_ver1_4_64
        if (ldx == 66) sub_ptr => mydgemv_n_ver1_4_66
        if (ldx == 68) sub_ptr => mydgemv_n_ver1_4_68
        if (ldx == 70) sub_ptr => mydgemv_n_ver1_4_70
        if (ldx == 72) sub_ptr => mydgemv_n_ver1_4_72
        if (ldx == 74) sub_ptr => mydgemv_n_ver1_4_74
        if (ldx == 76) sub_ptr => mydgemv_n_ver1_4_76
        if (ldx == 78) sub_ptr => mydgemv_n_ver1_4_78
        if (ldx == 80) sub_ptr => mydgemv_n_ver1_4_80
        if (ldx == 82) sub_ptr => mydgemv_n_ver1_4_82
        if (ldx == 84) sub_ptr => mydgemv_n_ver1_4_84
        if (ldx == 86) sub_ptr => mydgemv_n_ver1_4_86
        if (ldx == 88) sub_ptr => mydgemv_n_ver1_4_88
        if (ldx == 90) sub_ptr => mydgemv_n_ver1_4_90
        if (ldx == 92) sub_ptr => mydgemv_n_ver1_4_92
        if (ldx == 94) sub_ptr => mydgemv_n_ver1_4_94
        if (ldx == 96) sub_ptr => mydgemv_n_ver1_4_96
        if (ldx == 98) sub_ptr => mydgemv_n_ver1_4_98
        if (ldx == 100) sub_ptr => mydgemv_n_ver1_4_100
        if (ldx == 102) sub_ptr => mydgemv_n_ver1_4_102
        if (ldx == 104) sub_ptr => mydgemv_n_ver1_4_104
        if (ldx == 106) sub_ptr => mydgemv_n_ver1_4_106
        if (ldx == 108) sub_ptr => mydgemv_n_ver1_4_108
        if (ldx == 110) sub_ptr => mydgemv_n_ver1_4_110
        if (ldx == 112) sub_ptr => mydgemv_n_ver1_4_112
        if (ldx == 114) sub_ptr => mydgemv_n_ver1_4_114
        if (ldx == 116) sub_ptr => mydgemv_n_ver1_4_116
        if (ldx == 118) sub_ptr => mydgemv_n_ver1_4_118
        if (ldx == 120) sub_ptr => mydgemv_n_ver1_4_120
        if (ldx == 122) sub_ptr => mydgemv_n_ver1_4_122
        if (ldx == 124) sub_ptr => mydgemv_n_ver1_4_124
        if (ldx == 126) sub_ptr => mydgemv_n_ver1_4_126
        if (ldx == 128) sub_ptr => mydgemv_n_ver1_4_128
        if (ldx == 130) sub_ptr => mydgemv_n_ver1_4_130
        if (ldx == 132) sub_ptr => mydgemv_n_ver1_4_132
        if (ldx == 134) sub_ptr => mydgemv_n_ver1_4_134
        if (ldx == 136) sub_ptr => mydgemv_n_ver1_4_136
        if (ldx == 138) sub_ptr => mydgemv_n_ver1_4_138
        if (ldx == 140) sub_ptr => mydgemv_n_ver1_4_140
        if (ldx == 142) sub_ptr => mydgemv_n_ver1_4_142
        if (ldx == 144) sub_ptr => mydgemv_n_ver1_4_144
        if (ldx == 146) sub_ptr => mydgemv_n_ver1_4_146
        if (ldx == 148) sub_ptr => mydgemv_n_ver1_4_148
        if (ldx == 150) sub_ptr => mydgemv_n_ver1_4_150
        if (ldx == 152) sub_ptr => mydgemv_n_ver1_4_152
        if (ldx == 154) sub_ptr => mydgemv_n_ver1_4_154
        if (ldx == 156) sub_ptr => mydgemv_n_ver1_4_156
        if (ldx == 158) sub_ptr => mydgemv_n_ver1_4_158
        if (ldx == 160) sub_ptr => mydgemv_n_ver1_4_160
        if (ldx == 162) sub_ptr => mydgemv_n_ver1_4_162
        if (ldx == 164) sub_ptr => mydgemv_n_ver1_4_164
        if (ldx == 166) sub_ptr => mydgemv_n_ver1_4_166
        if (ldx == 168) sub_ptr => mydgemv_n_ver1_4_168
        if (ldx == 170) sub_ptr => mydgemv_n_ver1_4_170
        if (ldx == 172) sub_ptr => mydgemv_n_ver1_4_172
        if (ldx == 174) sub_ptr => mydgemv_n_ver1_4_174
        if (ldx == 176) sub_ptr => mydgemv_n_ver1_4_176
        if (ldx == 178) sub_ptr => mydgemv_n_ver1_4_178
        if (ldx == 180) sub_ptr => mydgemv_n_ver1_4_180
        if (ldx == 182) sub_ptr => mydgemv_n_ver1_4_182
        if (ldx == 184) sub_ptr => mydgemv_n_ver1_4_184
        if (ldx == 186) sub_ptr => mydgemv_n_ver1_4_186
        if (ldx == 188) sub_ptr => mydgemv_n_ver1_4_188
        if (ldx == 190) sub_ptr => mydgemv_n_ver1_4_190
        if (ldx == 192) sub_ptr => mydgemv_n_ver1_4_192
        if (ldx == 194) sub_ptr => mydgemv_n_ver1_4_194
        if (ldx == 196) sub_ptr => mydgemv_n_ver1_4_196
        if (ldx == 198) sub_ptr => mydgemv_n_ver1_4_198
        if (ldx == 200) sub_ptr => mydgemv_n_ver1_4_200
        if (ldx == 202) sub_ptr => mydgemv_n_ver1_4_202
        if (ldx == 204) sub_ptr => mydgemv_n_ver1_4_204
        if (ldx == 206) sub_ptr => mydgemv_n_ver1_4_206
        if (ldx == 208) sub_ptr => mydgemv_n_ver1_4_208
        if (ldx == 210) sub_ptr => mydgemv_n_ver1_4_210
        if (ldx == 212) sub_ptr => mydgemv_n_ver1_4_212
        if (ldx == 214) sub_ptr => mydgemv_n_ver1_4_214
        if (ldx == 216) sub_ptr => mydgemv_n_ver1_4_216
        if (ldx == 218) sub_ptr => mydgemv_n_ver1_4_218
        if (ldx == 220) sub_ptr => mydgemv_n_ver1_4_220
        if (ldx == 222) sub_ptr => mydgemv_n_ver1_4_222
        if (ldx == 224) sub_ptr => mydgemv_n_ver1_4_224
        if (ldx == 226) sub_ptr => mydgemv_n_ver1_4_226
        if (ldx == 228) sub_ptr => mydgemv_n_ver1_4_228
        if (ldx == 230) sub_ptr => mydgemv_n_ver1_4_230
        if (ldx == 232) sub_ptr => mydgemv_n_ver1_4_232
        if (ldx == 234) sub_ptr => mydgemv_n_ver1_4_234
        if (ldx == 236) sub_ptr => mydgemv_n_ver1_4_236
        if (ldx == 238) sub_ptr => mydgemv_n_ver1_4_238
        if (ldx == 240) sub_ptr => mydgemv_n_ver1_4_240
        if (ldx == 242) sub_ptr => mydgemv_n_ver1_4_242
        if (ldx == 244) sub_ptr => mydgemv_n_ver1_4_244
        if (ldx == 246) sub_ptr => mydgemv_n_ver1_4_246
        if (ldx == 248) sub_ptr => mydgemv_n_ver1_4_248
        if (ldx == 250) sub_ptr => mydgemv_n_ver1_4_250
        if (ldx == 252) sub_ptr => mydgemv_n_ver1_4_252
        if (ldx == 254) sub_ptr => mydgemv_n_ver1_4_254
        if (ldx == 256) sub_ptr => mydgemv_n_ver1_4_256
        if (ldx == 258) sub_ptr => mydgemv_n_ver1_4_258
        if (ldx == 260) sub_ptr => mydgemv_n_ver1_4_260
        if (ldx == 262) sub_ptr => mydgemv_n_ver1_4_262
        if (ldx == 264) sub_ptr => mydgemv_n_ver1_4_264
        if (ldx == 266) sub_ptr => mydgemv_n_ver1_4_266
        if (ldx == 268) sub_ptr => mydgemv_n_ver1_4_268
        if (ldx == 270) sub_ptr => mydgemv_n_ver1_4_270
        if (ldx == 272) sub_ptr => mydgemv_n_ver1_4_272
        if (ldx == 274) sub_ptr => mydgemv_n_ver1_4_274
        if (ldx == 276) sub_ptr => mydgemv_n_ver1_4_276
        if (ldx == 278) sub_ptr => mydgemv_n_ver1_4_278
        if (ldx == 280) sub_ptr => mydgemv_n_ver1_4_280
        if (ldx == 282) sub_ptr => mydgemv_n_ver1_4_282
        if (ldx == 284) sub_ptr => mydgemv_n_ver1_4_284
        if (ldx == 286) sub_ptr => mydgemv_n_ver1_4_286
        if (ldx == 288) sub_ptr => mydgemv_n_ver1_4_288
        if (ldx == 290) sub_ptr => mydgemv_n_ver1_4_290
        if (ldx == 292) sub_ptr => mydgemv_n_ver1_4_292
        if (ldx == 294) sub_ptr => mydgemv_n_ver1_4_294
        if (ldx == 296) sub_ptr => mydgemv_n_ver1_4_296
        if (ldx == 298) sub_ptr => mydgemv_n_ver1_4_298
        if (ldx == 300) sub_ptr => mydgemv_n_ver1_4_300
        if (ldx == 302) sub_ptr => mydgemv_n_ver1_4_302
        if (ldx == 304) sub_ptr => mydgemv_n_ver1_4_304
        if (ldx == 306) sub_ptr => mydgemv_n_ver1_4_306
        if (ldx == 308) sub_ptr => mydgemv_n_ver1_4_308
        if (ldx == 310) sub_ptr => mydgemv_n_ver1_4_310
        if (ldx == 312) sub_ptr => mydgemv_n_ver1_4_312
        if (ldx == 314) sub_ptr => mydgemv_n_ver1_4_314
        if (ldx == 316) sub_ptr => mydgemv_n_ver1_4_316
        if (ldx == 318) sub_ptr => mydgemv_n_ver1_4_318
        if (ldx == 320) sub_ptr => mydgemv_n_ver1_4_320
        if (ldx == 322) sub_ptr => mydgemv_n_ver1_4_322
        if (ldx == 324) sub_ptr => mydgemv_n_ver1_4_324
        if (ldx == 326) sub_ptr => mydgemv_n_ver1_4_326
        if (ldx == 328) sub_ptr => mydgemv_n_ver1_4_328
        if (ldx == 330) sub_ptr => mydgemv_n_ver1_4_330
        if (ldx == 332) sub_ptr => mydgemv_n_ver1_4_332
        if (ldx == 334) sub_ptr => mydgemv_n_ver1_4_334
        if (ldx == 336) sub_ptr => mydgemv_n_ver1_4_336
        if (ldx == 338) sub_ptr => mydgemv_n_ver1_4_338
        if (ldx == 340) sub_ptr => mydgemv_n_ver1_4_340
        if (ldx == 342) sub_ptr => mydgemv_n_ver1_4_342
        if (ldx == 344) sub_ptr => mydgemv_n_ver1_4_344
        if (ldx == 346) sub_ptr => mydgemv_n_ver1_4_346
        if (ldx == 348) sub_ptr => mydgemv_n_ver1_4_348
        if (ldx == 350) sub_ptr => mydgemv_n_ver1_4_350
        if (ldx == 352) sub_ptr => mydgemv_n_ver1_4_352
        if (ldx == 354) sub_ptr => mydgemv_n_ver1_4_354
        if (ldx == 356) sub_ptr => mydgemv_n_ver1_4_356
        if (ldx == 358) sub_ptr => mydgemv_n_ver1_4_358
        if (ldx == 360) sub_ptr => mydgemv_n_ver1_4_360
        if (ldx == 362) sub_ptr => mydgemv_n_ver1_4_362
        if (ldx == 364) sub_ptr => mydgemv_n_ver1_4_364
        if (ldx == 366) sub_ptr => mydgemv_n_ver1_4_366
        if (ldx == 368) sub_ptr => mydgemv_n_ver1_4_368
        if (ldx == 370) sub_ptr => mydgemv_n_ver1_4_370
        if (ldx == 372) sub_ptr => mydgemv_n_ver1_4_372
        if (ldx == 374) sub_ptr => mydgemv_n_ver1_4_374
        if (ldx == 376) sub_ptr => mydgemv_n_ver1_4_376
        if (ldx == 378) sub_ptr => mydgemv_n_ver1_4_378
        if (ldx == 380) sub_ptr => mydgemv_n_ver1_4_380
        if (ldx == 382) sub_ptr => mydgemv_n_ver1_4_382
        if (ldx == 384) sub_ptr => mydgemv_n_ver1_4_384
        if (ldx == 386) sub_ptr => mydgemv_n_ver1_4_386
        if (ldx == 388) sub_ptr => mydgemv_n_ver1_4_388
        if (ldx == 390) sub_ptr => mydgemv_n_ver1_4_390
        if (ldx == 392) sub_ptr => mydgemv_n_ver1_4_392
        if (ldx == 394) sub_ptr => mydgemv_n_ver1_4_394
        if (ldx == 396) sub_ptr => mydgemv_n_ver1_4_396
        if (ldx == 398) sub_ptr => mydgemv_n_ver1_4_398
        if (ldx == 400) sub_ptr => mydgemv_n_ver1_4_400
        if (ldx == 402) sub_ptr => mydgemv_n_ver1_4_402
        if (ldx == 404) sub_ptr => mydgemv_n_ver1_4_404
        if (ldx == 406) sub_ptr => mydgemv_n_ver1_4_406
        if (ldx == 408) sub_ptr => mydgemv_n_ver1_4_408
        if (ldx == 410) sub_ptr => mydgemv_n_ver1_4_410
        if (ldx == 412) sub_ptr => mydgemv_n_ver1_4_412
        if (ldx == 414) sub_ptr => mydgemv_n_ver1_4_414
        if (ldx == 416) sub_ptr => mydgemv_n_ver1_4_416
        if (ldx == 418) sub_ptr => mydgemv_n_ver1_4_418
        if (ldx == 420) sub_ptr => mydgemv_n_ver1_4_420
        if (ldx == 422) sub_ptr => mydgemv_n_ver1_4_422
        if (ldx == 424) sub_ptr => mydgemv_n_ver1_4_424
        if (ldx == 426) sub_ptr => mydgemv_n_ver1_4_426
        if (ldx == 428) sub_ptr => mydgemv_n_ver1_4_428
        if (ldx == 430) sub_ptr => mydgemv_n_ver1_4_430
        if (ldx == 432) sub_ptr => mydgemv_n_ver1_4_432
        if (ldx == 434) sub_ptr => mydgemv_n_ver1_4_434
        if (ldx == 436) sub_ptr => mydgemv_n_ver1_4_436
        if (ldx == 438) sub_ptr => mydgemv_n_ver1_4_438
        if (ldx == 440) sub_ptr => mydgemv_n_ver1_4_440
        if (ldx == 442) sub_ptr => mydgemv_n_ver1_4_442
        if (ldx == 444) sub_ptr => mydgemv_n_ver1_4_444
        if (ldx == 446) sub_ptr => mydgemv_n_ver1_4_446
        if (ldx == 448) sub_ptr => mydgemv_n_ver1_4_448
        if (ldx == 450) sub_ptr => mydgemv_n_ver1_4_450
        if (ldx == 452) sub_ptr => mydgemv_n_ver1_4_452
        if (ldx == 454) sub_ptr => mydgemv_n_ver1_4_454
        if (ldx == 456) sub_ptr => mydgemv_n_ver1_4_456
        if (ldx == 458) sub_ptr => mydgemv_n_ver1_4_458
        if (ldx == 460) sub_ptr => mydgemv_n_ver1_4_460
        if (ldx == 462) sub_ptr => mydgemv_n_ver1_4_462
        if (ldx == 464) sub_ptr => mydgemv_n_ver1_4_464
        if (ldx == 466) sub_ptr => mydgemv_n_ver1_4_466
        if (ldx == 468) sub_ptr => mydgemv_n_ver1_4_468
        if (ldx == 470) sub_ptr => mydgemv_n_ver1_4_470
        if (ldx == 472) sub_ptr => mydgemv_n_ver1_4_472
        if (ldx == 474) sub_ptr => mydgemv_n_ver1_4_474
        if (ldx == 476) sub_ptr => mydgemv_n_ver1_4_476
        if (ldx == 478) sub_ptr => mydgemv_n_ver1_4_478
        if (ldx == 480) sub_ptr => mydgemv_n_ver1_4_480
        if (ldx == 482) sub_ptr => mydgemv_n_ver1_4_482
        if (ldx == 484) sub_ptr => mydgemv_n_ver1_4_484
        if (ldx == 486) sub_ptr => mydgemv_n_ver1_4_486
        if (ldx == 488) sub_ptr => mydgemv_n_ver1_4_488
        if (ldx == 490) sub_ptr => mydgemv_n_ver1_4_490
        if (ldx == 492) sub_ptr => mydgemv_n_ver1_4_492
        if (ldx == 494) sub_ptr => mydgemv_n_ver1_4_494
        if (ldx == 496) sub_ptr => mydgemv_n_ver1_4_496
        if (ldx == 498) sub_ptr => mydgemv_n_ver1_4_498
        if (ldx == 500) sub_ptr => mydgemv_n_ver1_4_500
        if (ldx == 502) sub_ptr => mydgemv_n_ver1_4_502
        if (ldx == 504) sub_ptr => mydgemv_n_ver1_4_504
        if (ldx == 506) sub_ptr => mydgemv_n_ver1_4_506
        if (ldx == 508) sub_ptr => mydgemv_n_ver1_4_508
        if (ldx == 510) sub_ptr => mydgemv_n_ver1_4_510
        if (ldx == 512) sub_ptr => mydgemv_n_ver1_4_512        
    end subroutine select_sub_ptr_n_v1_4

    subroutine select_sub_ptr_t_v1_5(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 2) sub_ptr => mydgemv_t_ver1_5_2
        if (ldx == 4) sub_ptr => mydgemv_t_ver1_5_4
        if (ldx == 6) sub_ptr => mydgemv_t_ver1_5_6
        if (ldx == 8) sub_ptr => mydgemv_t_ver1_5_8
        if (ldx == 10) sub_ptr => mydgemv_t_ver1_5_10
        if (ldx == 12) sub_ptr => mydgemv_t_ver1_5_12
        if (ldx == 14) sub_ptr => mydgemv_t_ver1_5_14
        if (ldx == 16) sub_ptr => mydgemv_t_ver1_5_16
        if (ldx == 18) sub_ptr => mydgemv_t_ver1_5_18
        if (ldx == 20) sub_ptr => mydgemv_t_ver1_5_20
        if (ldx == 22) sub_ptr => mydgemv_t_ver1_5_22
        if (ldx == 24) sub_ptr => mydgemv_t_ver1_5_24
        if (ldx == 26) sub_ptr => mydgemv_t_ver1_5_26
        if (ldx == 28) sub_ptr => mydgemv_t_ver1_5_28
        if (ldx == 30) sub_ptr => mydgemv_t_ver1_5_30
        if (ldx == 32) sub_ptr => mydgemv_t_ver1_5_32
        if (ldx == 34) sub_ptr => mydgemv_t_ver1_5_34
        if (ldx == 36) sub_ptr => mydgemv_t_ver1_5_36
        if (ldx == 38) sub_ptr => mydgemv_t_ver1_5_38
        if (ldx == 40) sub_ptr => mydgemv_t_ver1_5_40
        if (ldx == 42) sub_ptr => mydgemv_t_ver1_5_42
        if (ldx == 44) sub_ptr => mydgemv_t_ver1_5_44
        if (ldx == 46) sub_ptr => mydgemv_t_ver1_5_46
        if (ldx == 48) sub_ptr => mydgemv_t_ver1_5_48
        if (ldx == 50) sub_ptr => mydgemv_t_ver1_5_50
        if (ldx == 52) sub_ptr => mydgemv_t_ver1_5_52
        if (ldx == 54) sub_ptr => mydgemv_t_ver1_5_54
        if (ldx == 56) sub_ptr => mydgemv_t_ver1_5_56
        if (ldx == 58) sub_ptr => mydgemv_t_ver1_5_58
        if (ldx == 60) sub_ptr => mydgemv_t_ver1_5_60
        if (ldx == 62) sub_ptr => mydgemv_t_ver1_5_62
        if (ldx == 64) sub_ptr => mydgemv_t_ver1_5_64
        if (ldx == 66) sub_ptr => mydgemv_t_ver1_5_66
        if (ldx == 68) sub_ptr => mydgemv_t_ver1_5_68
        if (ldx == 70) sub_ptr => mydgemv_t_ver1_5_70
        if (ldx == 72) sub_ptr => mydgemv_t_ver1_5_72
        if (ldx == 74) sub_ptr => mydgemv_t_ver1_5_74
        if (ldx == 76) sub_ptr => mydgemv_t_ver1_5_76
        if (ldx == 78) sub_ptr => mydgemv_t_ver1_5_78
        if (ldx == 80) sub_ptr => mydgemv_t_ver1_5_80
        if (ldx == 82) sub_ptr => mydgemv_t_ver1_5_82
        if (ldx == 84) sub_ptr => mydgemv_t_ver1_5_84
        if (ldx == 86) sub_ptr => mydgemv_t_ver1_5_86
        if (ldx == 88) sub_ptr => mydgemv_t_ver1_5_88
        if (ldx == 90) sub_ptr => mydgemv_t_ver1_5_90
        if (ldx == 92) sub_ptr => mydgemv_t_ver1_5_92
        if (ldx == 94) sub_ptr => mydgemv_t_ver1_5_94
        if (ldx == 96) sub_ptr => mydgemv_t_ver1_5_96
        if (ldx == 98) sub_ptr => mydgemv_t_ver1_5_98
        if (ldx == 100) sub_ptr => mydgemv_t_ver1_5_100
        if (ldx == 102) sub_ptr => mydgemv_t_ver1_5_102
        if (ldx == 104) sub_ptr => mydgemv_t_ver1_5_104
        if (ldx == 106) sub_ptr => mydgemv_t_ver1_5_106
        if (ldx == 108) sub_ptr => mydgemv_t_ver1_5_108
        if (ldx == 110) sub_ptr => mydgemv_t_ver1_5_110
        if (ldx == 112) sub_ptr => mydgemv_t_ver1_5_112
        if (ldx == 114) sub_ptr => mydgemv_t_ver1_5_114
        if (ldx == 116) sub_ptr => mydgemv_t_ver1_5_116
        if (ldx == 118) sub_ptr => mydgemv_t_ver1_5_118
        if (ldx == 120) sub_ptr => mydgemv_t_ver1_5_120
        if (ldx == 122) sub_ptr => mydgemv_t_ver1_5_122
        if (ldx == 124) sub_ptr => mydgemv_t_ver1_5_124
        if (ldx == 126) sub_ptr => mydgemv_t_ver1_5_126
        if (ldx == 128) sub_ptr => mydgemv_t_ver1_5_128
        if (ldx == 130) sub_ptr => mydgemv_t_ver1_5_130
        if (ldx == 132) sub_ptr => mydgemv_t_ver1_5_132
        if (ldx == 134) sub_ptr => mydgemv_t_ver1_5_134
        if (ldx == 136) sub_ptr => mydgemv_t_ver1_5_136
        if (ldx == 138) sub_ptr => mydgemv_t_ver1_5_138
        if (ldx == 140) sub_ptr => mydgemv_t_ver1_5_140
        if (ldx == 142) sub_ptr => mydgemv_t_ver1_5_142
        if (ldx == 144) sub_ptr => mydgemv_t_ver1_5_144
        if (ldx == 146) sub_ptr => mydgemv_t_ver1_5_146
        if (ldx == 148) sub_ptr => mydgemv_t_ver1_5_148
        if (ldx == 150) sub_ptr => mydgemv_t_ver1_5_150
        if (ldx == 152) sub_ptr => mydgemv_t_ver1_5_152
        if (ldx == 154) sub_ptr => mydgemv_t_ver1_5_154
        if (ldx == 156) sub_ptr => mydgemv_t_ver1_5_156
        if (ldx == 158) sub_ptr => mydgemv_t_ver1_5_158
        if (ldx == 160) sub_ptr => mydgemv_t_ver1_5_160
        if (ldx == 162) sub_ptr => mydgemv_t_ver1_5_162
        if (ldx == 164) sub_ptr => mydgemv_t_ver1_5_164
        if (ldx == 166) sub_ptr => mydgemv_t_ver1_5_166
        if (ldx == 168) sub_ptr => mydgemv_t_ver1_5_168
        if (ldx == 170) sub_ptr => mydgemv_t_ver1_5_170
        if (ldx == 172) sub_ptr => mydgemv_t_ver1_5_172
        if (ldx == 174) sub_ptr => mydgemv_t_ver1_5_174
        if (ldx == 176) sub_ptr => mydgemv_t_ver1_5_176
        if (ldx == 178) sub_ptr => mydgemv_t_ver1_5_178
        if (ldx == 180) sub_ptr => mydgemv_t_ver1_5_180
        if (ldx == 182) sub_ptr => mydgemv_t_ver1_5_182
        if (ldx == 184) sub_ptr => mydgemv_t_ver1_5_184
        if (ldx == 186) sub_ptr => mydgemv_t_ver1_5_186
        if (ldx == 188) sub_ptr => mydgemv_t_ver1_5_188
        if (ldx == 190) sub_ptr => mydgemv_t_ver1_5_190
        if (ldx == 192) sub_ptr => mydgemv_t_ver1_5_192
        if (ldx == 194) sub_ptr => mydgemv_t_ver1_5_194
        if (ldx == 196) sub_ptr => mydgemv_t_ver1_5_196
        if (ldx == 198) sub_ptr => mydgemv_t_ver1_5_198
        if (ldx == 200) sub_ptr => mydgemv_t_ver1_5_200
        if (ldx == 202) sub_ptr => mydgemv_t_ver1_5_202
        if (ldx == 204) sub_ptr => mydgemv_t_ver1_5_204
        if (ldx == 206) sub_ptr => mydgemv_t_ver1_5_206
        if (ldx == 208) sub_ptr => mydgemv_t_ver1_5_208
        if (ldx == 210) sub_ptr => mydgemv_t_ver1_5_210
        if (ldx == 212) sub_ptr => mydgemv_t_ver1_5_212
        if (ldx == 214) sub_ptr => mydgemv_t_ver1_5_214
        if (ldx == 216) sub_ptr => mydgemv_t_ver1_5_216
        if (ldx == 218) sub_ptr => mydgemv_t_ver1_5_218
        if (ldx == 220) sub_ptr => mydgemv_t_ver1_5_220
        if (ldx == 222) sub_ptr => mydgemv_t_ver1_5_222
        if (ldx == 224) sub_ptr => mydgemv_t_ver1_5_224
        if (ldx == 226) sub_ptr => mydgemv_t_ver1_5_226
        if (ldx == 228) sub_ptr => mydgemv_t_ver1_5_228
        if (ldx == 230) sub_ptr => mydgemv_t_ver1_5_230
        if (ldx == 232) sub_ptr => mydgemv_t_ver1_5_232
        if (ldx == 234) sub_ptr => mydgemv_t_ver1_5_234
        if (ldx == 236) sub_ptr => mydgemv_t_ver1_5_236
        if (ldx == 238) sub_ptr => mydgemv_t_ver1_5_238
        if (ldx == 240) sub_ptr => mydgemv_t_ver1_5_240
        if (ldx == 242) sub_ptr => mydgemv_t_ver1_5_242
        if (ldx == 244) sub_ptr => mydgemv_t_ver1_5_244
        if (ldx == 246) sub_ptr => mydgemv_t_ver1_5_246
        if (ldx == 248) sub_ptr => mydgemv_t_ver1_5_248
        if (ldx == 250) sub_ptr => mydgemv_t_ver1_5_250
        if (ldx == 252) sub_ptr => mydgemv_t_ver1_5_252
        if (ldx == 254) sub_ptr => mydgemv_t_ver1_5_254
        if (ldx == 256) sub_ptr => mydgemv_t_ver1_5_256
        if (ldx == 258) sub_ptr => mydgemv_t_ver1_5_258
        if (ldx == 260) sub_ptr => mydgemv_t_ver1_5_260
        if (ldx == 262) sub_ptr => mydgemv_t_ver1_5_262
        if (ldx == 264) sub_ptr => mydgemv_t_ver1_5_264
        if (ldx == 266) sub_ptr => mydgemv_t_ver1_5_266
        if (ldx == 268) sub_ptr => mydgemv_t_ver1_5_268
        if (ldx == 270) sub_ptr => mydgemv_t_ver1_5_270
        if (ldx == 272) sub_ptr => mydgemv_t_ver1_5_272
        if (ldx == 274) sub_ptr => mydgemv_t_ver1_5_274
        if (ldx == 276) sub_ptr => mydgemv_t_ver1_5_276
        if (ldx == 278) sub_ptr => mydgemv_t_ver1_5_278
        if (ldx == 280) sub_ptr => mydgemv_t_ver1_5_280
        if (ldx == 282) sub_ptr => mydgemv_t_ver1_5_282
        if (ldx == 284) sub_ptr => mydgemv_t_ver1_5_284
        if (ldx == 286) sub_ptr => mydgemv_t_ver1_5_286
        if (ldx == 288) sub_ptr => mydgemv_t_ver1_5_288
        if (ldx == 290) sub_ptr => mydgemv_t_ver1_5_290
        if (ldx == 292) sub_ptr => mydgemv_t_ver1_5_292
        if (ldx == 294) sub_ptr => mydgemv_t_ver1_5_294
        if (ldx == 296) sub_ptr => mydgemv_t_ver1_5_296
        if (ldx == 298) sub_ptr => mydgemv_t_ver1_5_298
        if (ldx == 300) sub_ptr => mydgemv_t_ver1_5_300
        if (ldx == 302) sub_ptr => mydgemv_t_ver1_5_302
        if (ldx == 304) sub_ptr => mydgemv_t_ver1_5_304
        if (ldx == 306) sub_ptr => mydgemv_t_ver1_5_306
        if (ldx == 308) sub_ptr => mydgemv_t_ver1_5_308
        if (ldx == 310) sub_ptr => mydgemv_t_ver1_5_310
        if (ldx == 312) sub_ptr => mydgemv_t_ver1_5_312
        if (ldx == 314) sub_ptr => mydgemv_t_ver1_5_314
        if (ldx == 316) sub_ptr => mydgemv_t_ver1_5_316
        if (ldx == 318) sub_ptr => mydgemv_t_ver1_5_318
        if (ldx == 320) sub_ptr => mydgemv_t_ver1_5_320
        if (ldx == 322) sub_ptr => mydgemv_t_ver1_5_322
        if (ldx == 324) sub_ptr => mydgemv_t_ver1_5_324
        if (ldx == 326) sub_ptr => mydgemv_t_ver1_5_326
        if (ldx == 328) sub_ptr => mydgemv_t_ver1_5_328
        if (ldx == 330) sub_ptr => mydgemv_t_ver1_5_330
        if (ldx == 332) sub_ptr => mydgemv_t_ver1_5_332
        if (ldx == 334) sub_ptr => mydgemv_t_ver1_5_334
        if (ldx == 336) sub_ptr => mydgemv_t_ver1_5_336
        if (ldx == 338) sub_ptr => mydgemv_t_ver1_5_338
        if (ldx == 340) sub_ptr => mydgemv_t_ver1_5_340
        if (ldx == 342) sub_ptr => mydgemv_t_ver1_5_342
        if (ldx == 344) sub_ptr => mydgemv_t_ver1_5_344
        if (ldx == 346) sub_ptr => mydgemv_t_ver1_5_346
        if (ldx == 348) sub_ptr => mydgemv_t_ver1_5_348
        if (ldx == 350) sub_ptr => mydgemv_t_ver1_5_350
        if (ldx == 352) sub_ptr => mydgemv_t_ver1_5_352
        if (ldx == 354) sub_ptr => mydgemv_t_ver1_5_354
        if (ldx == 356) sub_ptr => mydgemv_t_ver1_5_356
        if (ldx == 358) sub_ptr => mydgemv_t_ver1_5_358
        if (ldx == 360) sub_ptr => mydgemv_t_ver1_5_360
        if (ldx == 362) sub_ptr => mydgemv_t_ver1_5_362
        if (ldx == 364) sub_ptr => mydgemv_t_ver1_5_364
        if (ldx == 366) sub_ptr => mydgemv_t_ver1_5_366
        if (ldx == 368) sub_ptr => mydgemv_t_ver1_5_368
        if (ldx == 370) sub_ptr => mydgemv_t_ver1_5_370
        if (ldx == 372) sub_ptr => mydgemv_t_ver1_5_372
        if (ldx == 374) sub_ptr => mydgemv_t_ver1_5_374
        if (ldx == 376) sub_ptr => mydgemv_t_ver1_5_376
        if (ldx == 378) sub_ptr => mydgemv_t_ver1_5_378
        if (ldx == 380) sub_ptr => mydgemv_t_ver1_5_380
        if (ldx == 382) sub_ptr => mydgemv_t_ver1_5_382
        if (ldx == 384) sub_ptr => mydgemv_t_ver1_5_384
        if (ldx == 386) sub_ptr => mydgemv_t_ver1_5_386
        if (ldx == 388) sub_ptr => mydgemv_t_ver1_5_388
        if (ldx == 390) sub_ptr => mydgemv_t_ver1_5_390
        if (ldx == 392) sub_ptr => mydgemv_t_ver1_5_392
        if (ldx == 394) sub_ptr => mydgemv_t_ver1_5_394
        if (ldx == 396) sub_ptr => mydgemv_t_ver1_5_396
        if (ldx == 398) sub_ptr => mydgemv_t_ver1_5_398
        if (ldx == 400) sub_ptr => mydgemv_t_ver1_5_400
        if (ldx == 402) sub_ptr => mydgemv_t_ver1_5_402
        if (ldx == 404) sub_ptr => mydgemv_t_ver1_5_404
        if (ldx == 406) sub_ptr => mydgemv_t_ver1_5_406
        if (ldx == 408) sub_ptr => mydgemv_t_ver1_5_408
        if (ldx == 410) sub_ptr => mydgemv_t_ver1_5_410
        if (ldx == 412) sub_ptr => mydgemv_t_ver1_5_412
        if (ldx == 414) sub_ptr => mydgemv_t_ver1_5_414
        if (ldx == 416) sub_ptr => mydgemv_t_ver1_5_416
        if (ldx == 418) sub_ptr => mydgemv_t_ver1_5_418
        if (ldx == 420) sub_ptr => mydgemv_t_ver1_5_420
        if (ldx == 422) sub_ptr => mydgemv_t_ver1_5_422
        if (ldx == 424) sub_ptr => mydgemv_t_ver1_5_424
        if (ldx == 426) sub_ptr => mydgemv_t_ver1_5_426
        if (ldx == 428) sub_ptr => mydgemv_t_ver1_5_428
        if (ldx == 430) sub_ptr => mydgemv_t_ver1_5_430
        if (ldx == 432) sub_ptr => mydgemv_t_ver1_5_432
        if (ldx == 434) sub_ptr => mydgemv_t_ver1_5_434
        if (ldx == 436) sub_ptr => mydgemv_t_ver1_5_436
        if (ldx == 438) sub_ptr => mydgemv_t_ver1_5_438
        if (ldx == 440) sub_ptr => mydgemv_t_ver1_5_440
        if (ldx == 442) sub_ptr => mydgemv_t_ver1_5_442
        if (ldx == 444) sub_ptr => mydgemv_t_ver1_5_444
        if (ldx == 446) sub_ptr => mydgemv_t_ver1_5_446
        if (ldx == 448) sub_ptr => mydgemv_t_ver1_5_448
        if (ldx == 450) sub_ptr => mydgemv_t_ver1_5_450
        if (ldx == 452) sub_ptr => mydgemv_t_ver1_5_452
        if (ldx == 454) sub_ptr => mydgemv_t_ver1_5_454
        if (ldx == 456) sub_ptr => mydgemv_t_ver1_5_456
        if (ldx == 458) sub_ptr => mydgemv_t_ver1_5_458
        if (ldx == 460) sub_ptr => mydgemv_t_ver1_5_460
        if (ldx == 462) sub_ptr => mydgemv_t_ver1_5_462
        if (ldx == 464) sub_ptr => mydgemv_t_ver1_5_464
        if (ldx == 466) sub_ptr => mydgemv_t_ver1_5_466
        if (ldx == 468) sub_ptr => mydgemv_t_ver1_5_468
        if (ldx == 470) sub_ptr => mydgemv_t_ver1_5_470
        if (ldx == 472) sub_ptr => mydgemv_t_ver1_5_472
        if (ldx == 474) sub_ptr => mydgemv_t_ver1_5_474
        if (ldx == 476) sub_ptr => mydgemv_t_ver1_5_476
        if (ldx == 478) sub_ptr => mydgemv_t_ver1_5_478
        if (ldx == 480) sub_ptr => mydgemv_t_ver1_5_480
        if (ldx == 482) sub_ptr => mydgemv_t_ver1_5_482
        if (ldx == 484) sub_ptr => mydgemv_t_ver1_5_484
        if (ldx == 486) sub_ptr => mydgemv_t_ver1_5_486
        if (ldx == 488) sub_ptr => mydgemv_t_ver1_5_488
        if (ldx == 490) sub_ptr => mydgemv_t_ver1_5_490
        if (ldx == 492) sub_ptr => mydgemv_t_ver1_5_492
        if (ldx == 494) sub_ptr => mydgemv_t_ver1_5_494
        if (ldx == 496) sub_ptr => mydgemv_t_ver1_5_496
        if (ldx == 498) sub_ptr => mydgemv_t_ver1_5_498
        if (ldx == 500) sub_ptr => mydgemv_t_ver1_5_500
        if (ldx == 502) sub_ptr => mydgemv_t_ver1_5_502
        if (ldx == 504) sub_ptr => mydgemv_t_ver1_5_504
        if (ldx == 506) sub_ptr => mydgemv_t_ver1_5_506
        if (ldx == 508) sub_ptr => mydgemv_t_ver1_5_508
        if (ldx == 510) sub_ptr => mydgemv_t_ver1_5_510
        if (ldx == 512) sub_ptr => mydgemv_t_ver1_5_512        
    end subroutine select_sub_ptr_t_v1_5

    subroutine select_sub_ptr_n_v2_4(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 4) sub_ptr => mydgemv_n_ver2_4_4
        if (ldx == 8) sub_ptr => mydgemv_n_ver2_4_8
        if (ldx == 12) sub_ptr => mydgemv_n_ver2_4_12
        if (ldx == 16) sub_ptr => mydgemv_n_ver2_4_16
        if (ldx == 20) sub_ptr => mydgemv_n_ver2_4_20
        if (ldx == 24) sub_ptr => mydgemv_n_ver2_4_24
        if (ldx == 28) sub_ptr => mydgemv_n_ver2_4_28
        if (ldx == 32) sub_ptr => mydgemv_n_ver2_4_32
        if (ldx == 36) sub_ptr => mydgemv_n_ver2_4_36
        if (ldx == 40) sub_ptr => mydgemv_n_ver2_4_40
        if (ldx == 44) sub_ptr => mydgemv_n_ver2_4_44
        if (ldx == 48) sub_ptr => mydgemv_n_ver2_4_48
        if (ldx == 52) sub_ptr => mydgemv_n_ver2_4_52
        if (ldx == 56) sub_ptr => mydgemv_n_ver2_4_56
        if (ldx == 60) sub_ptr => mydgemv_n_ver2_4_60
        if (ldx == 64) sub_ptr => mydgemv_n_ver2_4_64
        if (ldx == 68) sub_ptr => mydgemv_n_ver2_4_68
        if (ldx == 72) sub_ptr => mydgemv_n_ver2_4_72
        if (ldx == 76) sub_ptr => mydgemv_n_ver2_4_76
        if (ldx == 80) sub_ptr => mydgemv_n_ver2_4_80
        if (ldx == 84) sub_ptr => mydgemv_n_ver2_4_84
        if (ldx == 88) sub_ptr => mydgemv_n_ver2_4_88
        if (ldx == 92) sub_ptr => mydgemv_n_ver2_4_92
        if (ldx == 96) sub_ptr => mydgemv_n_ver2_4_96
        if (ldx == 100) sub_ptr => mydgemv_n_ver2_4_100
        if (ldx == 104) sub_ptr => mydgemv_n_ver2_4_104
        if (ldx == 108) sub_ptr => mydgemv_n_ver2_4_108
        if (ldx == 112) sub_ptr => mydgemv_n_ver2_4_112
        if (ldx == 116) sub_ptr => mydgemv_n_ver2_4_116
        if (ldx == 120) sub_ptr => mydgemv_n_ver2_4_120
        if (ldx == 124) sub_ptr => mydgemv_n_ver2_4_124
        if (ldx == 128) sub_ptr => mydgemv_n_ver2_4_128
        if (ldx == 132) sub_ptr => mydgemv_n_ver2_4_132
        if (ldx == 136) sub_ptr => mydgemv_n_ver2_4_136
        if (ldx == 140) sub_ptr => mydgemv_n_ver2_4_140
        if (ldx == 144) sub_ptr => mydgemv_n_ver2_4_144
        if (ldx == 148) sub_ptr => mydgemv_n_ver2_4_148
        if (ldx == 152) sub_ptr => mydgemv_n_ver2_4_152
        if (ldx == 156) sub_ptr => mydgemv_n_ver2_4_156
        if (ldx == 160) sub_ptr => mydgemv_n_ver2_4_160
        if (ldx == 164) sub_ptr => mydgemv_n_ver2_4_164
        if (ldx == 168) sub_ptr => mydgemv_n_ver2_4_168
        if (ldx == 172) sub_ptr => mydgemv_n_ver2_4_172
        if (ldx == 176) sub_ptr => mydgemv_n_ver2_4_176
        if (ldx == 180) sub_ptr => mydgemv_n_ver2_4_180
        if (ldx == 184) sub_ptr => mydgemv_n_ver2_4_184
        if (ldx == 188) sub_ptr => mydgemv_n_ver2_4_188
        if (ldx == 192) sub_ptr => mydgemv_n_ver2_4_192
        if (ldx == 196) sub_ptr => mydgemv_n_ver2_4_196
        if (ldx == 200) sub_ptr => mydgemv_n_ver2_4_200
        if (ldx == 204) sub_ptr => mydgemv_n_ver2_4_204
        if (ldx == 208) sub_ptr => mydgemv_n_ver2_4_208
        if (ldx == 212) sub_ptr => mydgemv_n_ver2_4_212
        if (ldx == 216) sub_ptr => mydgemv_n_ver2_4_216
        if (ldx == 220) sub_ptr => mydgemv_n_ver2_4_220
        if (ldx == 224) sub_ptr => mydgemv_n_ver2_4_224
        if (ldx == 228) sub_ptr => mydgemv_n_ver2_4_228
        if (ldx == 232) sub_ptr => mydgemv_n_ver2_4_232
        if (ldx == 236) sub_ptr => mydgemv_n_ver2_4_236
        if (ldx == 240) sub_ptr => mydgemv_n_ver2_4_240
        if (ldx == 244) sub_ptr => mydgemv_n_ver2_4_244
        if (ldx == 248) sub_ptr => mydgemv_n_ver2_4_248
        if (ldx == 252) sub_ptr => mydgemv_n_ver2_4_252
        if (ldx == 256) sub_ptr => mydgemv_n_ver2_4_256
        if (ldx == 260) sub_ptr => mydgemv_n_ver2_4_260
        if (ldx == 264) sub_ptr => mydgemv_n_ver2_4_264
        if (ldx == 268) sub_ptr => mydgemv_n_ver2_4_268
        if (ldx == 272) sub_ptr => mydgemv_n_ver2_4_272
        if (ldx == 276) sub_ptr => mydgemv_n_ver2_4_276
        if (ldx == 280) sub_ptr => mydgemv_n_ver2_4_280
        if (ldx == 284) sub_ptr => mydgemv_n_ver2_4_284
        if (ldx == 288) sub_ptr => mydgemv_n_ver2_4_288
        if (ldx == 292) sub_ptr => mydgemv_n_ver2_4_292
        if (ldx == 296) sub_ptr => mydgemv_n_ver2_4_296
        if (ldx == 300) sub_ptr => mydgemv_n_ver2_4_300
        if (ldx == 304) sub_ptr => mydgemv_n_ver2_4_304
        if (ldx == 308) sub_ptr => mydgemv_n_ver2_4_308
        if (ldx == 312) sub_ptr => mydgemv_n_ver2_4_312
        if (ldx == 316) sub_ptr => mydgemv_n_ver2_4_316
        if (ldx == 320) sub_ptr => mydgemv_n_ver2_4_320
        if (ldx == 324) sub_ptr => mydgemv_n_ver2_4_324
        if (ldx == 328) sub_ptr => mydgemv_n_ver2_4_328
        if (ldx == 332) sub_ptr => mydgemv_n_ver2_4_332
        if (ldx == 336) sub_ptr => mydgemv_n_ver2_4_336
        if (ldx == 340) sub_ptr => mydgemv_n_ver2_4_340
        if (ldx == 344) sub_ptr => mydgemv_n_ver2_4_344
        if (ldx == 348) sub_ptr => mydgemv_n_ver2_4_348
        if (ldx == 352) sub_ptr => mydgemv_n_ver2_4_352
        if (ldx == 356) sub_ptr => mydgemv_n_ver2_4_356
        if (ldx == 360) sub_ptr => mydgemv_n_ver2_4_360
        if (ldx == 364) sub_ptr => mydgemv_n_ver2_4_364
        if (ldx == 368) sub_ptr => mydgemv_n_ver2_4_368
        if (ldx == 372) sub_ptr => mydgemv_n_ver2_4_372
        if (ldx == 376) sub_ptr => mydgemv_n_ver2_4_376
        if (ldx == 380) sub_ptr => mydgemv_n_ver2_4_380
        if (ldx == 384) sub_ptr => mydgemv_n_ver2_4_384
        if (ldx == 388) sub_ptr => mydgemv_n_ver2_4_388
        if (ldx == 392) sub_ptr => mydgemv_n_ver2_4_392
        if (ldx == 396) sub_ptr => mydgemv_n_ver2_4_396
        if (ldx == 400) sub_ptr => mydgemv_n_ver2_4_400
        if (ldx == 404) sub_ptr => mydgemv_n_ver2_4_404
        if (ldx == 408) sub_ptr => mydgemv_n_ver2_4_408
        if (ldx == 412) sub_ptr => mydgemv_n_ver2_4_412
        if (ldx == 416) sub_ptr => mydgemv_n_ver2_4_416
        if (ldx == 420) sub_ptr => mydgemv_n_ver2_4_420
        if (ldx == 424) sub_ptr => mydgemv_n_ver2_4_424
        if (ldx == 428) sub_ptr => mydgemv_n_ver2_4_428
        if (ldx == 432) sub_ptr => mydgemv_n_ver2_4_432
        if (ldx == 436) sub_ptr => mydgemv_n_ver2_4_436
        if (ldx == 440) sub_ptr => mydgemv_n_ver2_4_440
        if (ldx == 444) sub_ptr => mydgemv_n_ver2_4_444
        if (ldx == 448) sub_ptr => mydgemv_n_ver2_4_448
        if (ldx == 452) sub_ptr => mydgemv_n_ver2_4_452
        if (ldx == 456) sub_ptr => mydgemv_n_ver2_4_456
        if (ldx == 460) sub_ptr => mydgemv_n_ver2_4_460
        if (ldx == 464) sub_ptr => mydgemv_n_ver2_4_464
        if (ldx == 468) sub_ptr => mydgemv_n_ver2_4_468
        if (ldx == 472) sub_ptr => mydgemv_n_ver2_4_472
        if (ldx == 476) sub_ptr => mydgemv_n_ver2_4_476
        if (ldx == 480) sub_ptr => mydgemv_n_ver2_4_480
        if (ldx == 484) sub_ptr => mydgemv_n_ver2_4_484
        if (ldx == 488) sub_ptr => mydgemv_n_ver2_4_488
        if (ldx == 492) sub_ptr => mydgemv_n_ver2_4_492
        if (ldx == 496) sub_ptr => mydgemv_n_ver2_4_496
        if (ldx == 500) sub_ptr => mydgemv_n_ver2_4_500
        if (ldx == 504) sub_ptr => mydgemv_n_ver2_4_504
        if (ldx == 508) sub_ptr => mydgemv_n_ver2_4_508
        if (ldx == 512) sub_ptr => mydgemv_n_ver2_4_512
    end subroutine select_sub_ptr_n_v2_4

    subroutine select_sub_ptr_t_v2_4(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 4) sub_ptr => mydgemv_t_ver2_4_4
        if (ldx == 8) sub_ptr => mydgemv_t_ver2_4_8
        if (ldx == 12) sub_ptr => mydgemv_t_ver2_4_12
        if (ldx == 16) sub_ptr => mydgemv_t_ver2_4_16
        if (ldx == 20) sub_ptr => mydgemv_t_ver2_4_20
        if (ldx == 24) sub_ptr => mydgemv_t_ver2_4_24
        if (ldx == 28) sub_ptr => mydgemv_t_ver2_4_28
        if (ldx == 32) sub_ptr => mydgemv_t_ver2_4_32
        if (ldx == 36) sub_ptr => mydgemv_t_ver2_4_36
        if (ldx == 40) sub_ptr => mydgemv_t_ver2_4_40
        if (ldx == 44) sub_ptr => mydgemv_t_ver2_4_44
        if (ldx == 48) sub_ptr => mydgemv_t_ver2_4_48
        if (ldx == 52) sub_ptr => mydgemv_t_ver2_4_52
        if (ldx == 56) sub_ptr => mydgemv_t_ver2_4_56
        if (ldx == 60) sub_ptr => mydgemv_t_ver2_4_60
        if (ldx == 64) sub_ptr => mydgemv_t_ver2_4_64
        if (ldx == 68) sub_ptr => mydgemv_t_ver2_4_68
        if (ldx == 72) sub_ptr => mydgemv_t_ver2_4_72
        if (ldx == 76) sub_ptr => mydgemv_t_ver2_4_76
        if (ldx == 80) sub_ptr => mydgemv_t_ver2_4_80
        if (ldx == 84) sub_ptr => mydgemv_t_ver2_4_84
        if (ldx == 88) sub_ptr => mydgemv_t_ver2_4_88
        if (ldx == 92) sub_ptr => mydgemv_t_ver2_4_92
        if (ldx == 96) sub_ptr => mydgemv_t_ver2_4_96
        if (ldx == 100) sub_ptr => mydgemv_t_ver2_4_100
        if (ldx == 104) sub_ptr => mydgemv_t_ver2_4_104
        if (ldx == 108) sub_ptr => mydgemv_t_ver2_4_108
        if (ldx == 112) sub_ptr => mydgemv_t_ver2_4_112
        if (ldx == 116) sub_ptr => mydgemv_t_ver2_4_116
        if (ldx == 120) sub_ptr => mydgemv_t_ver2_4_120
        if (ldx == 124) sub_ptr => mydgemv_t_ver2_4_124
        if (ldx == 128) sub_ptr => mydgemv_t_ver2_4_128
        if (ldx == 132) sub_ptr => mydgemv_t_ver2_4_132
        if (ldx == 136) sub_ptr => mydgemv_t_ver2_4_136
        if (ldx == 140) sub_ptr => mydgemv_t_ver2_4_140
        if (ldx == 144) sub_ptr => mydgemv_t_ver2_4_144
        if (ldx == 148) sub_ptr => mydgemv_t_ver2_4_148
        if (ldx == 152) sub_ptr => mydgemv_t_ver2_4_152
        if (ldx == 156) sub_ptr => mydgemv_t_ver2_4_156
        if (ldx == 160) sub_ptr => mydgemv_t_ver2_4_160
        if (ldx == 164) sub_ptr => mydgemv_t_ver2_4_164
        if (ldx == 168) sub_ptr => mydgemv_t_ver2_4_168
        if (ldx == 172) sub_ptr => mydgemv_t_ver2_4_172
        if (ldx == 176) sub_ptr => mydgemv_t_ver2_4_176
        if (ldx == 180) sub_ptr => mydgemv_t_ver2_4_180
        if (ldx == 184) sub_ptr => mydgemv_t_ver2_4_184
        if (ldx == 188) sub_ptr => mydgemv_t_ver2_4_188
        if (ldx == 192) sub_ptr => mydgemv_t_ver2_4_192
        if (ldx == 196) sub_ptr => mydgemv_t_ver2_4_196
        if (ldx == 200) sub_ptr => mydgemv_t_ver2_4_200
        if (ldx == 204) sub_ptr => mydgemv_t_ver2_4_204
        if (ldx == 208) sub_ptr => mydgemv_t_ver2_4_208
        if (ldx == 212) sub_ptr => mydgemv_t_ver2_4_212
        if (ldx == 216) sub_ptr => mydgemv_t_ver2_4_216
        if (ldx == 220) sub_ptr => mydgemv_t_ver2_4_220
        if (ldx == 224) sub_ptr => mydgemv_t_ver2_4_224
        if (ldx == 228) sub_ptr => mydgemv_t_ver2_4_228
        if (ldx == 232) sub_ptr => mydgemv_t_ver2_4_232
        if (ldx == 236) sub_ptr => mydgemv_t_ver2_4_236
        if (ldx == 240) sub_ptr => mydgemv_t_ver2_4_240
        if (ldx == 244) sub_ptr => mydgemv_t_ver2_4_244
        if (ldx == 248) sub_ptr => mydgemv_t_ver2_4_248
        if (ldx == 252) sub_ptr => mydgemv_t_ver2_4_252
        if (ldx == 256) sub_ptr => mydgemv_t_ver2_4_256
        if (ldx == 260) sub_ptr => mydgemv_t_ver2_4_260
        if (ldx == 264) sub_ptr => mydgemv_t_ver2_4_264
        if (ldx == 268) sub_ptr => mydgemv_t_ver2_4_268
        if (ldx == 272) sub_ptr => mydgemv_t_ver2_4_272
        if (ldx == 276) sub_ptr => mydgemv_t_ver2_4_276
        if (ldx == 280) sub_ptr => mydgemv_t_ver2_4_280
        if (ldx == 284) sub_ptr => mydgemv_t_ver2_4_284
        if (ldx == 288) sub_ptr => mydgemv_t_ver2_4_288
        if (ldx == 292) sub_ptr => mydgemv_t_ver2_4_292
        if (ldx == 296) sub_ptr => mydgemv_t_ver2_4_296
        if (ldx == 300) sub_ptr => mydgemv_t_ver2_4_300
        if (ldx == 304) sub_ptr => mydgemv_t_ver2_4_304
        if (ldx == 308) sub_ptr => mydgemv_t_ver2_4_308
        if (ldx == 312) sub_ptr => mydgemv_t_ver2_4_312
        if (ldx == 316) sub_ptr => mydgemv_t_ver2_4_316
        if (ldx == 320) sub_ptr => mydgemv_t_ver2_4_320
        if (ldx == 324) sub_ptr => mydgemv_t_ver2_4_324
        if (ldx == 328) sub_ptr => mydgemv_t_ver2_4_328
        if (ldx == 332) sub_ptr => mydgemv_t_ver2_4_332
        if (ldx == 336) sub_ptr => mydgemv_t_ver2_4_336
        if (ldx == 340) sub_ptr => mydgemv_t_ver2_4_340
        if (ldx == 344) sub_ptr => mydgemv_t_ver2_4_344
        if (ldx == 348) sub_ptr => mydgemv_t_ver2_4_348
        if (ldx == 352) sub_ptr => mydgemv_t_ver2_4_352
        if (ldx == 356) sub_ptr => mydgemv_t_ver2_4_356
        if (ldx == 360) sub_ptr => mydgemv_t_ver2_4_360
        if (ldx == 364) sub_ptr => mydgemv_t_ver2_4_364
        if (ldx == 368) sub_ptr => mydgemv_t_ver2_4_368
        if (ldx == 372) sub_ptr => mydgemv_t_ver2_4_372
        if (ldx == 376) sub_ptr => mydgemv_t_ver2_4_376
        if (ldx == 380) sub_ptr => mydgemv_t_ver2_4_380
        if (ldx == 384) sub_ptr => mydgemv_t_ver2_4_384
        if (ldx == 388) sub_ptr => mydgemv_t_ver2_4_388
        if (ldx == 392) sub_ptr => mydgemv_t_ver2_4_392
        if (ldx == 396) sub_ptr => mydgemv_t_ver2_4_396
        if (ldx == 400) sub_ptr => mydgemv_t_ver2_4_400
        if (ldx == 404) sub_ptr => mydgemv_t_ver2_4_404
        if (ldx == 408) sub_ptr => mydgemv_t_ver2_4_408
        if (ldx == 412) sub_ptr => mydgemv_t_ver2_4_412
        if (ldx == 416) sub_ptr => mydgemv_t_ver2_4_416
        if (ldx == 420) sub_ptr => mydgemv_t_ver2_4_420
        if (ldx == 424) sub_ptr => mydgemv_t_ver2_4_424
        if (ldx == 428) sub_ptr => mydgemv_t_ver2_4_428
        if (ldx == 432) sub_ptr => mydgemv_t_ver2_4_432
        if (ldx == 436) sub_ptr => mydgemv_t_ver2_4_436
        if (ldx == 440) sub_ptr => mydgemv_t_ver2_4_440
        if (ldx == 444) sub_ptr => mydgemv_t_ver2_4_444
        if (ldx == 448) sub_ptr => mydgemv_t_ver2_4_448
        if (ldx == 452) sub_ptr => mydgemv_t_ver2_4_452
        if (ldx == 456) sub_ptr => mydgemv_t_ver2_4_456
        if (ldx == 460) sub_ptr => mydgemv_t_ver2_4_460
        if (ldx == 464) sub_ptr => mydgemv_t_ver2_4_464
        if (ldx == 468) sub_ptr => mydgemv_t_ver2_4_468
        if (ldx == 472) sub_ptr => mydgemv_t_ver2_4_472
        if (ldx == 476) sub_ptr => mydgemv_t_ver2_4_476
        if (ldx == 480) sub_ptr => mydgemv_t_ver2_4_480
        if (ldx == 484) sub_ptr => mydgemv_t_ver2_4_484
        if (ldx == 488) sub_ptr => mydgemv_t_ver2_4_488
        if (ldx == 492) sub_ptr => mydgemv_t_ver2_4_492
        if (ldx == 496) sub_ptr => mydgemv_t_ver2_4_496
        if (ldx == 500) sub_ptr => mydgemv_t_ver2_4_500
        if (ldx == 504) sub_ptr => mydgemv_t_ver2_4_504
        if (ldx == 508) sub_ptr => mydgemv_t_ver2_4_508
        if (ldx == 512) sub_ptr => mydgemv_t_ver2_4_512
    end subroutine select_sub_ptr_t_v2_4

    subroutine select_sub_ptr_t_v2_5(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 4) sub_ptr => mydgemv_t_ver2_5_4
        if (ldx == 8) sub_ptr => mydgemv_t_ver2_5_8
        if (ldx == 12) sub_ptr => mydgemv_t_ver2_5_12
        if (ldx == 16) sub_ptr => mydgemv_t_ver2_5_16
        if (ldx == 20) sub_ptr => mydgemv_t_ver2_5_20
        if (ldx == 24) sub_ptr => mydgemv_t_ver2_5_24
        if (ldx == 28) sub_ptr => mydgemv_t_ver2_5_28
        if (ldx == 32) sub_ptr => mydgemv_t_ver2_5_32
        if (ldx == 36) sub_ptr => mydgemv_t_ver2_5_36
        if (ldx == 40) sub_ptr => mydgemv_t_ver2_5_40
        if (ldx == 44) sub_ptr => mydgemv_t_ver2_5_44
        if (ldx == 48) sub_ptr => mydgemv_t_ver2_5_48
        if (ldx == 52) sub_ptr => mydgemv_t_ver2_5_52
        if (ldx == 56) sub_ptr => mydgemv_t_ver2_5_56
        if (ldx == 60) sub_ptr => mydgemv_t_ver2_5_60
        if (ldx == 64) sub_ptr => mydgemv_t_ver2_5_64
        if (ldx == 68) sub_ptr => mydgemv_t_ver2_5_68
        if (ldx == 72) sub_ptr => mydgemv_t_ver2_5_72
        if (ldx == 76) sub_ptr => mydgemv_t_ver2_5_76
        if (ldx == 80) sub_ptr => mydgemv_t_ver2_5_80
        if (ldx == 84) sub_ptr => mydgemv_t_ver2_5_84
        if (ldx == 88) sub_ptr => mydgemv_t_ver2_5_88
        if (ldx == 92) sub_ptr => mydgemv_t_ver2_5_92
        if (ldx == 96) sub_ptr => mydgemv_t_ver2_5_96
        if (ldx == 100) sub_ptr => mydgemv_t_ver2_5_100
        if (ldx == 104) sub_ptr => mydgemv_t_ver2_5_104
        if (ldx == 108) sub_ptr => mydgemv_t_ver2_5_108
        if (ldx == 112) sub_ptr => mydgemv_t_ver2_5_112
        if (ldx == 116) sub_ptr => mydgemv_t_ver2_5_116
        if (ldx == 120) sub_ptr => mydgemv_t_ver2_5_120
        if (ldx == 124) sub_ptr => mydgemv_t_ver2_5_124
        if (ldx == 128) sub_ptr => mydgemv_t_ver2_5_128
        if (ldx == 132) sub_ptr => mydgemv_t_ver2_5_132
        if (ldx == 136) sub_ptr => mydgemv_t_ver2_5_136
        if (ldx == 140) sub_ptr => mydgemv_t_ver2_5_140
        if (ldx == 144) sub_ptr => mydgemv_t_ver2_5_144
        if (ldx == 148) sub_ptr => mydgemv_t_ver2_5_148
        if (ldx == 152) sub_ptr => mydgemv_t_ver2_5_152
        if (ldx == 156) sub_ptr => mydgemv_t_ver2_5_156
        if (ldx == 160) sub_ptr => mydgemv_t_ver2_5_160
        if (ldx == 164) sub_ptr => mydgemv_t_ver2_5_164
        if (ldx == 168) sub_ptr => mydgemv_t_ver2_5_168
        if (ldx == 172) sub_ptr => mydgemv_t_ver2_5_172
        if (ldx == 176) sub_ptr => mydgemv_t_ver2_5_176
        if (ldx == 180) sub_ptr => mydgemv_t_ver2_5_180
        if (ldx == 184) sub_ptr => mydgemv_t_ver2_5_184
        if (ldx == 188) sub_ptr => mydgemv_t_ver2_5_188
        if (ldx == 192) sub_ptr => mydgemv_t_ver2_5_192
        if (ldx == 196) sub_ptr => mydgemv_t_ver2_5_196
        if (ldx == 200) sub_ptr => mydgemv_t_ver2_5_200
        if (ldx == 204) sub_ptr => mydgemv_t_ver2_5_204
        if (ldx == 208) sub_ptr => mydgemv_t_ver2_5_208
        if (ldx == 212) sub_ptr => mydgemv_t_ver2_5_212
        if (ldx == 216) sub_ptr => mydgemv_t_ver2_5_216
        if (ldx == 220) sub_ptr => mydgemv_t_ver2_5_220
        if (ldx == 224) sub_ptr => mydgemv_t_ver2_5_224
        if (ldx == 228) sub_ptr => mydgemv_t_ver2_5_228
        if (ldx == 232) sub_ptr => mydgemv_t_ver2_5_232
        if (ldx == 236) sub_ptr => mydgemv_t_ver2_5_236
        if (ldx == 240) sub_ptr => mydgemv_t_ver2_5_240
        if (ldx == 244) sub_ptr => mydgemv_t_ver2_5_244
        if (ldx == 248) sub_ptr => mydgemv_t_ver2_5_248
        if (ldx == 252) sub_ptr => mydgemv_t_ver2_5_252
        if (ldx == 256) sub_ptr => mydgemv_t_ver2_5_256
        if (ldx == 260) sub_ptr => mydgemv_t_ver2_5_260
        if (ldx == 264) sub_ptr => mydgemv_t_ver2_5_264
        if (ldx == 268) sub_ptr => mydgemv_t_ver2_5_268
        if (ldx == 272) sub_ptr => mydgemv_t_ver2_5_272
        if (ldx == 276) sub_ptr => mydgemv_t_ver2_5_276
        if (ldx == 280) sub_ptr => mydgemv_t_ver2_5_280
        if (ldx == 284) sub_ptr => mydgemv_t_ver2_5_284
        if (ldx == 288) sub_ptr => mydgemv_t_ver2_5_288
        if (ldx == 292) sub_ptr => mydgemv_t_ver2_5_292
        if (ldx == 296) sub_ptr => mydgemv_t_ver2_5_296
        if (ldx == 300) sub_ptr => mydgemv_t_ver2_5_300
        if (ldx == 304) sub_ptr => mydgemv_t_ver2_5_304
        if (ldx == 308) sub_ptr => mydgemv_t_ver2_5_308
        if (ldx == 312) sub_ptr => mydgemv_t_ver2_5_312
        if (ldx == 316) sub_ptr => mydgemv_t_ver2_5_316
        if (ldx == 320) sub_ptr => mydgemv_t_ver2_5_320
        if (ldx == 324) sub_ptr => mydgemv_t_ver2_5_324
        if (ldx == 328) sub_ptr => mydgemv_t_ver2_5_328
        if (ldx == 332) sub_ptr => mydgemv_t_ver2_5_332
        if (ldx == 336) sub_ptr => mydgemv_t_ver2_5_336
        if (ldx == 340) sub_ptr => mydgemv_t_ver2_5_340
        if (ldx == 344) sub_ptr => mydgemv_t_ver2_5_344
        if (ldx == 348) sub_ptr => mydgemv_t_ver2_5_348
        if (ldx == 352) sub_ptr => mydgemv_t_ver2_5_352
        if (ldx == 356) sub_ptr => mydgemv_t_ver2_5_356
        if (ldx == 360) sub_ptr => mydgemv_t_ver2_5_360
        if (ldx == 364) sub_ptr => mydgemv_t_ver2_5_364
        if (ldx == 368) sub_ptr => mydgemv_t_ver2_5_368
        if (ldx == 372) sub_ptr => mydgemv_t_ver2_5_372
        if (ldx == 376) sub_ptr => mydgemv_t_ver2_5_376
        if (ldx == 380) sub_ptr => mydgemv_t_ver2_5_380
        if (ldx == 384) sub_ptr => mydgemv_t_ver2_5_384
        if (ldx == 388) sub_ptr => mydgemv_t_ver2_5_388
        if (ldx == 392) sub_ptr => mydgemv_t_ver2_5_392
        if (ldx == 396) sub_ptr => mydgemv_t_ver2_5_396
        if (ldx == 400) sub_ptr => mydgemv_t_ver2_5_400
        if (ldx == 404) sub_ptr => mydgemv_t_ver2_5_404
        if (ldx == 408) sub_ptr => mydgemv_t_ver2_5_408
        if (ldx == 412) sub_ptr => mydgemv_t_ver2_5_412
        if (ldx == 416) sub_ptr => mydgemv_t_ver2_5_416
        if (ldx == 420) sub_ptr => mydgemv_t_ver2_5_420
        if (ldx == 424) sub_ptr => mydgemv_t_ver2_5_424
        if (ldx == 428) sub_ptr => mydgemv_t_ver2_5_428
        if (ldx == 432) sub_ptr => mydgemv_t_ver2_5_432
        if (ldx == 436) sub_ptr => mydgemv_t_ver2_5_436
        if (ldx == 440) sub_ptr => mydgemv_t_ver2_5_440
        if (ldx == 444) sub_ptr => mydgemv_t_ver2_5_444
        if (ldx == 448) sub_ptr => mydgemv_t_ver2_5_448
        if (ldx == 452) sub_ptr => mydgemv_t_ver2_5_452
        if (ldx == 456) sub_ptr => mydgemv_t_ver2_5_456
        if (ldx == 460) sub_ptr => mydgemv_t_ver2_5_460
        if (ldx == 464) sub_ptr => mydgemv_t_ver2_5_464
        if (ldx == 468) sub_ptr => mydgemv_t_ver2_5_468
        if (ldx == 472) sub_ptr => mydgemv_t_ver2_5_472
        if (ldx == 476) sub_ptr => mydgemv_t_ver2_5_476
        if (ldx == 480) sub_ptr => mydgemv_t_ver2_5_480
        if (ldx == 484) sub_ptr => mydgemv_t_ver2_5_484
        if (ldx == 488) sub_ptr => mydgemv_t_ver2_5_488
        if (ldx == 492) sub_ptr => mydgemv_t_ver2_5_492
        if (ldx == 496) sub_ptr => mydgemv_t_ver2_5_496
        if (ldx == 500) sub_ptr => mydgemv_t_ver2_5_500
        if (ldx == 504) sub_ptr => mydgemv_t_ver2_5_504
        if (ldx == 508) sub_ptr => mydgemv_t_ver2_5_508
        if (ldx == 512) sub_ptr => mydgemv_t_ver2_5_512
    end subroutine select_sub_ptr_t_v2_5

    subroutine select_sub_ptr_n_v3_4(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 8) sub_ptr => mydgemv_n_ver3_4_8
        if (ldx == 16) sub_ptr => mydgemv_n_ver3_4_16
        if (ldx == 24) sub_ptr => mydgemv_n_ver3_4_24
        if (ldx == 32) sub_ptr => mydgemv_n_ver3_4_32
        if (ldx == 40) sub_ptr => mydgemv_n_ver3_4_40
        if (ldx == 48) sub_ptr => mydgemv_n_ver3_4_48
        if (ldx == 56) sub_ptr => mydgemv_n_ver3_4_56
        if (ldx == 64) sub_ptr => mydgemv_n_ver3_4_64
        if (ldx == 72) sub_ptr => mydgemv_n_ver3_4_72
        if (ldx == 80) sub_ptr => mydgemv_n_ver3_4_80
        if (ldx == 88) sub_ptr => mydgemv_n_ver3_4_88
        if (ldx == 96) sub_ptr => mydgemv_n_ver3_4_96
        if (ldx == 104) sub_ptr => mydgemv_n_ver3_4_104
        if (ldx == 112) sub_ptr => mydgemv_n_ver3_4_112
        if (ldx == 120) sub_ptr => mydgemv_n_ver3_4_120
        if (ldx == 128) sub_ptr => mydgemv_n_ver3_4_128
        if (ldx == 136) sub_ptr => mydgemv_n_ver3_4_136
        if (ldx == 144) sub_ptr => mydgemv_n_ver3_4_144
        if (ldx == 152) sub_ptr => mydgemv_n_ver3_4_152
        if (ldx == 160) sub_ptr => mydgemv_n_ver3_4_160
        if (ldx == 168) sub_ptr => mydgemv_n_ver3_4_168
        if (ldx == 176) sub_ptr => mydgemv_n_ver3_4_176
        if (ldx == 184) sub_ptr => mydgemv_n_ver3_4_184
        if (ldx == 192) sub_ptr => mydgemv_n_ver3_4_192
        if (ldx == 200) sub_ptr => mydgemv_n_ver3_4_200
        if (ldx == 208) sub_ptr => mydgemv_n_ver3_4_208
        if (ldx == 216) sub_ptr => mydgemv_n_ver3_4_216
        if (ldx == 224) sub_ptr => mydgemv_n_ver3_4_224
        if (ldx == 232) sub_ptr => mydgemv_n_ver3_4_232
        if (ldx == 240) sub_ptr => mydgemv_n_ver3_4_240
        if (ldx == 248) sub_ptr => mydgemv_n_ver3_4_248
        if (ldx == 256) sub_ptr => mydgemv_n_ver3_4_256
        if (ldx == 264) sub_ptr => mydgemv_n_ver3_4_264
        if (ldx == 272) sub_ptr => mydgemv_n_ver3_4_272
        if (ldx == 280) sub_ptr => mydgemv_n_ver3_4_280
        if (ldx == 288) sub_ptr => mydgemv_n_ver3_4_288
        if (ldx == 296) sub_ptr => mydgemv_n_ver3_4_296
        if (ldx == 304) sub_ptr => mydgemv_n_ver3_4_304
        if (ldx == 312) sub_ptr => mydgemv_n_ver3_4_312
        if (ldx == 320) sub_ptr => mydgemv_n_ver3_4_320
        if (ldx == 328) sub_ptr => mydgemv_n_ver3_4_328
        if (ldx == 336) sub_ptr => mydgemv_n_ver3_4_336
        if (ldx == 344) sub_ptr => mydgemv_n_ver3_4_344
        if (ldx == 352) sub_ptr => mydgemv_n_ver3_4_352
        if (ldx == 360) sub_ptr => mydgemv_n_ver3_4_360
        if (ldx == 368) sub_ptr => mydgemv_n_ver3_4_368
        if (ldx == 376) sub_ptr => mydgemv_n_ver3_4_376
        if (ldx == 384) sub_ptr => mydgemv_n_ver3_4_384
        if (ldx == 392) sub_ptr => mydgemv_n_ver3_4_392
        if (ldx == 400) sub_ptr => mydgemv_n_ver3_4_400
        if (ldx == 408) sub_ptr => mydgemv_n_ver3_4_408
        if (ldx == 416) sub_ptr => mydgemv_n_ver3_4_416
        if (ldx == 424) sub_ptr => mydgemv_n_ver3_4_424
        if (ldx == 432) sub_ptr => mydgemv_n_ver3_4_432
        if (ldx == 440) sub_ptr => mydgemv_n_ver3_4_440
        if (ldx == 448) sub_ptr => mydgemv_n_ver3_4_448
        if (ldx == 456) sub_ptr => mydgemv_n_ver3_4_456
        if (ldx == 464) sub_ptr => mydgemv_n_ver3_4_464
        if (ldx == 472) sub_ptr => mydgemv_n_ver3_4_472
        if (ldx == 480) sub_ptr => mydgemv_n_ver3_4_480
        if (ldx == 488) sub_ptr => mydgemv_n_ver3_4_488
        if (ldx == 496) sub_ptr => mydgemv_n_ver3_4_496
        if (ldx == 504) sub_ptr => mydgemv_n_ver3_4_504
        if (ldx == 512) sub_ptr => mydgemv_n_ver3_4_512
    end subroutine select_sub_ptr_n_v3_4

    subroutine select_sub_ptr_t_v3_4(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 8) sub_ptr => mydgemv_t_ver3_4_8
        if (ldx == 16) sub_ptr => mydgemv_t_ver3_4_16
        if (ldx == 24) sub_ptr => mydgemv_t_ver3_4_24
        if (ldx == 32) sub_ptr => mydgemv_t_ver3_4_32
        if (ldx == 40) sub_ptr => mydgemv_t_ver3_4_40
        if (ldx == 48) sub_ptr => mydgemv_t_ver3_4_48
        if (ldx == 56) sub_ptr => mydgemv_t_ver3_4_56
        if (ldx == 64) sub_ptr => mydgemv_t_ver3_4_64
        if (ldx == 72) sub_ptr => mydgemv_t_ver3_4_72
        if (ldx == 80) sub_ptr => mydgemv_t_ver3_4_80
        if (ldx == 88) sub_ptr => mydgemv_t_ver3_4_88
        if (ldx == 96) sub_ptr => mydgemv_t_ver3_4_96
        if (ldx == 104) sub_ptr => mydgemv_t_ver3_4_104
        if (ldx == 112) sub_ptr => mydgemv_t_ver3_4_112
        if (ldx == 120) sub_ptr => mydgemv_t_ver3_4_120
        if (ldx == 128) sub_ptr => mydgemv_t_ver3_4_128
        if (ldx == 136) sub_ptr => mydgemv_t_ver3_4_136
        if (ldx == 144) sub_ptr => mydgemv_t_ver3_4_144
        if (ldx == 152) sub_ptr => mydgemv_t_ver3_4_152
        if (ldx == 160) sub_ptr => mydgemv_t_ver3_4_160
        if (ldx == 168) sub_ptr => mydgemv_t_ver3_4_168
        if (ldx == 176) sub_ptr => mydgemv_t_ver3_4_176
        if (ldx == 184) sub_ptr => mydgemv_t_ver3_4_184
        if (ldx == 192) sub_ptr => mydgemv_t_ver3_4_192
        if (ldx == 200) sub_ptr => mydgemv_t_ver3_4_200
        if (ldx == 208) sub_ptr => mydgemv_t_ver3_4_208
        if (ldx == 216) sub_ptr => mydgemv_t_ver3_4_216
        if (ldx == 224) sub_ptr => mydgemv_t_ver3_4_224
        if (ldx == 232) sub_ptr => mydgemv_t_ver3_4_232
        if (ldx == 240) sub_ptr => mydgemv_t_ver3_4_240
        if (ldx == 248) sub_ptr => mydgemv_t_ver3_4_248
        if (ldx == 256) sub_ptr => mydgemv_t_ver3_4_256
        if (ldx == 264) sub_ptr => mydgemv_t_ver3_4_264
        if (ldx == 272) sub_ptr => mydgemv_t_ver3_4_272
        if (ldx == 280) sub_ptr => mydgemv_t_ver3_4_280
        if (ldx == 288) sub_ptr => mydgemv_t_ver3_4_288
        if (ldx == 296) sub_ptr => mydgemv_t_ver3_4_296
        if (ldx == 304) sub_ptr => mydgemv_t_ver3_4_304
        if (ldx == 312) sub_ptr => mydgemv_t_ver3_4_312
        if (ldx == 320) sub_ptr => mydgemv_t_ver3_4_320
        if (ldx == 328) sub_ptr => mydgemv_t_ver3_4_328
        if (ldx == 336) sub_ptr => mydgemv_t_ver3_4_336
        if (ldx == 344) sub_ptr => mydgemv_t_ver3_4_344
        if (ldx == 352) sub_ptr => mydgemv_t_ver3_4_352
        if (ldx == 360) sub_ptr => mydgemv_t_ver3_4_360
        if (ldx == 368) sub_ptr => mydgemv_t_ver3_4_368
        if (ldx == 376) sub_ptr => mydgemv_t_ver3_4_376
        if (ldx == 384) sub_ptr => mydgemv_t_ver3_4_384
        if (ldx == 392) sub_ptr => mydgemv_t_ver3_4_392
        if (ldx == 400) sub_ptr => mydgemv_t_ver3_4_400
        if (ldx == 408) sub_ptr => mydgemv_t_ver3_4_408
        if (ldx == 416) sub_ptr => mydgemv_t_ver3_4_416
        if (ldx == 424) sub_ptr => mydgemv_t_ver3_4_424
        if (ldx == 432) sub_ptr => mydgemv_t_ver3_4_432
        if (ldx == 440) sub_ptr => mydgemv_t_ver3_4_440
        if (ldx == 448) sub_ptr => mydgemv_t_ver3_4_448
        if (ldx == 456) sub_ptr => mydgemv_t_ver3_4_456
        if (ldx == 464) sub_ptr => mydgemv_t_ver3_4_464
        if (ldx == 472) sub_ptr => mydgemv_t_ver3_4_472
        if (ldx == 480) sub_ptr => mydgemv_t_ver3_4_480
        if (ldx == 488) sub_ptr => mydgemv_t_ver3_4_488
        if (ldx == 496) sub_ptr => mydgemv_t_ver3_4_496
        if (ldx == 504) sub_ptr => mydgemv_t_ver3_4_504
        if (ldx == 512) sub_ptr => mydgemv_t_ver3_4_512
    end subroutine select_sub_ptr_t_v3_4

    subroutine select_sub_ptr_t_v3_5(sub_ptr, ldx)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: ldx
        if (ldx == 8) sub_ptr => mydgemv_t_ver3_5_8
        if (ldx == 16) sub_ptr => mydgemv_t_ver3_5_16
        if (ldx == 24) sub_ptr => mydgemv_t_ver3_5_24
        if (ldx == 32) sub_ptr => mydgemv_t_ver3_5_32
        if (ldx == 40) sub_ptr => mydgemv_t_ver3_5_40
        if (ldx == 48) sub_ptr => mydgemv_t_ver3_5_48
        if (ldx == 56) sub_ptr => mydgemv_t_ver3_5_56
        if (ldx == 64) sub_ptr => mydgemv_t_ver3_5_64
        if (ldx == 72) sub_ptr => mydgemv_t_ver3_5_72
        if (ldx == 80) sub_ptr => mydgemv_t_ver3_5_80
        if (ldx == 88) sub_ptr => mydgemv_t_ver3_5_88
        if (ldx == 96) sub_ptr => mydgemv_t_ver3_5_96
        if (ldx == 104) sub_ptr => mydgemv_t_ver3_5_104
        if (ldx == 112) sub_ptr => mydgemv_t_ver3_5_112
        if (ldx == 120) sub_ptr => mydgemv_t_ver3_5_120
        if (ldx == 128) sub_ptr => mydgemv_t_ver3_5_128
        if (ldx == 136) sub_ptr => mydgemv_t_ver3_5_136
        if (ldx == 144) sub_ptr => mydgemv_t_ver3_5_144
        if (ldx == 152) sub_ptr => mydgemv_t_ver3_5_152
        if (ldx == 160) sub_ptr => mydgemv_t_ver3_5_160
        if (ldx == 168) sub_ptr => mydgemv_t_ver3_5_168
        if (ldx == 176) sub_ptr => mydgemv_t_ver3_5_176
        if (ldx == 184) sub_ptr => mydgemv_t_ver3_5_184
        if (ldx == 192) sub_ptr => mydgemv_t_ver3_5_192
        if (ldx == 200) sub_ptr => mydgemv_t_ver3_5_200
        if (ldx == 208) sub_ptr => mydgemv_t_ver3_5_208
        if (ldx == 216) sub_ptr => mydgemv_t_ver3_5_216
        if (ldx == 224) sub_ptr => mydgemv_t_ver3_5_224
        if (ldx == 232) sub_ptr => mydgemv_t_ver3_5_232
        if (ldx == 240) sub_ptr => mydgemv_t_ver3_5_240
        if (ldx == 248) sub_ptr => mydgemv_t_ver3_5_248
        if (ldx == 256) sub_ptr => mydgemv_t_ver3_5_256
        if (ldx == 264) sub_ptr => mydgemv_t_ver3_5_264
        if (ldx == 272) sub_ptr => mydgemv_t_ver3_5_272
        if (ldx == 280) sub_ptr => mydgemv_t_ver3_5_280
        if (ldx == 288) sub_ptr => mydgemv_t_ver3_5_288
        if (ldx == 296) sub_ptr => mydgemv_t_ver3_5_296
        if (ldx == 304) sub_ptr => mydgemv_t_ver3_5_304
        if (ldx == 312) sub_ptr => mydgemv_t_ver3_5_312
        if (ldx == 320) sub_ptr => mydgemv_t_ver3_5_320
        if (ldx == 328) sub_ptr => mydgemv_t_ver3_5_328
        if (ldx == 336) sub_ptr => mydgemv_t_ver3_5_336
        if (ldx == 344) sub_ptr => mydgemv_t_ver3_5_344
        if (ldx == 352) sub_ptr => mydgemv_t_ver3_5_352
        if (ldx == 360) sub_ptr => mydgemv_t_ver3_5_360
        if (ldx == 368) sub_ptr => mydgemv_t_ver3_5_368
        if (ldx == 376) sub_ptr => mydgemv_t_ver3_5_376
        if (ldx == 384) sub_ptr => mydgemv_t_ver3_5_384
        if (ldx == 392) sub_ptr => mydgemv_t_ver3_5_392
        if (ldx == 400) sub_ptr => mydgemv_t_ver3_5_400
        if (ldx == 408) sub_ptr => mydgemv_t_ver3_5_408
        if (ldx == 416) sub_ptr => mydgemv_t_ver3_5_416
        if (ldx == 424) sub_ptr => mydgemv_t_ver3_5_424
        if (ldx == 432) sub_ptr => mydgemv_t_ver3_5_432
        if (ldx == 440) sub_ptr => mydgemv_t_ver3_5_440
        if (ldx == 448) sub_ptr => mydgemv_t_ver3_5_448
        if (ldx == 456) sub_ptr => mydgemv_t_ver3_5_456
        if (ldx == 464) sub_ptr => mydgemv_t_ver3_5_464
        if (ldx == 472) sub_ptr => mydgemv_t_ver3_5_472
        if (ldx == 480) sub_ptr => mydgemv_t_ver3_5_480
        if (ldx == 488) sub_ptr => mydgemv_t_ver3_5_488
        if (ldx == 496) sub_ptr => mydgemv_t_ver3_5_496
        if (ldx == 504) sub_ptr => mydgemv_t_ver3_5_504
        if (ldx == 512) sub_ptr => mydgemv_t_ver3_5_512
    end subroutine select_sub_ptr_t_v3_5

    function run_mydgemv(sub_ptr, max_iter, a_t, x, y, lda, ldx, ldy, print_sum, description, sum_y) result(time)
        implicit none
        procedure(mydgemv), pointer :: sub_ptr
        integer(kind=8) :: max_iter, i, time
        real(kind=8)    :: a_t(ldx, lda), x(ldx), y(ldy)
        integer(kind=8) :: lda, ldx, ldy
        logical(kind=4) :: print_sum
        character(len=*), intent(in) :: description
        character(len=45) :: description_
        real(kind=8) :: sum_y


        integer(kind=8) :: count_rate_s, count_max_s, count_s
        integer(kind=8) :: count_rate_e, count_max_e, count_e

        call left_align_str(description, description_, 45)

        y = 0d0
        call openblas_set_num_threads(1)
        CALL SYSTEM_CLOCK(count_s, count_rate_s, count_max_s)
        do i=1, max_iter, 1
            call sub_ptr(a_t, x, y, lda, ldx, ldy)
        end do
        CALL SYSTEM_CLOCK(count_e, count_rate_e, count_max_e)
        if (print_sum) print*, description_, count_e - count_s, sum(y) 
        if (abs((sum(y) - sum_y) / sum_y) >= 1d-12) then
            print*, sum(y), " vs ", sum_y , "  -->> DIFF = ", abs(sum(y) - sum_y) <= 1d-8, description_, shape(a_t)
            stop "SUM NOT EQUAL."
        end if
        time = count_e - count_s
    end function run_mydgemv

    function run_mydgemv_X(sub_ptr_X, max_iter, a_t, tmp, x, y, lda, ldx, ldy, print_sum, description, sum_y) result(time)
        implicit none
        procedure(mydgemv_X), pointer :: sub_ptr_X
        integer(kind=8) :: max_iter, i, time
        real(kind=8)    :: a_t(ldx, lda), tmp(ldx), x(ldx), y(ldy)
        integer(kind=8) :: lda, ldx, ldy
        logical(kind=4) :: print_sum
        character(len=*), intent(in) :: description
        character(len=45) :: description_
        real(kind=8) :: sum_y

        integer(kind=8) :: count_rate_s, count_max_s, count_s
        integer(kind=8) :: count_rate_e, count_max_e, count_e

        call left_align_str(description, description_, 45)

        y = 0d0
        call openblas_set_num_threads(1)
        CALL SYSTEM_CLOCK(count_s, count_rate_s, count_max_s)
        do i=1, max_iter, 1
            call sub_ptr_X(a_t, tmp, x, y, lda, ldx, ldy)
        end do
        CALL SYSTEM_CLOCK(count_e, count_rate_e, count_max_e)
        if (print_sum) print*, description_, count_e - count_s, sum(y) 
        if (abs((sum(y) - sum_y) / sum_y) >= 1d-12) then
            print*, sum(y), " vs ", sum_y , "  -->> DIFF = ", abs(sum(y) - sum_y) <= 1d-8, description_, shape(a_t)
            stop "SUM NOT EQUAL."
        end if
        time = count_e - count_s
    end function run_mydgemv_X

    function run_dgemv_normal(max_iter, a, x, y, lda, ldx, ldy, print_sum) result(time)
        implicit none
        integer(kind=8) :: max_iter, i, time
        real(kind=8)    :: a(lda, ldx), x(ldx), y(ldy)
        integer(kind=8) :: lda, ldx, ldy
        logical(kind=4) :: print_sum
        character(len=45) :: description_
        
        integer(kind=8) :: count_rate_s, count_max_s, count_s
        integer(kind=8) :: count_rate_e, count_max_e, count_e

        call left_align_str("OpenBLAS Normal:          ", description_, 45)    
        call openblas_set_num_threads(1)
        CALL SYSTEM_CLOCK(count_s, count_rate_s, count_max_s)
        do i=1, max_iter, 1
            call dgemv("N", lda, ldx, 1d0, a, lda, x, 1_8, 0d0, y, 1_8)
        end do
        CALL SYSTEM_CLOCK(count_e, count_rate_e, count_max_e)
        if (print_sum) print*, description_, count_e - count_s, sum(y)        
        time = count_e - count_s
    end function run_dgemv_normal

    function run_dgemv_transposed(max_iter, a_t, x, y, lda, ldx, ldy, print_sum) result(time)
        implicit none
        integer(kind=8) :: max_iter, i, time
        real(kind=8)    :: a_t(ldx, lda), x(ldx), y(ldy)
        integer(kind=8) :: lda, ldx, ldy
        logical(kind=4) :: print_sum
        character(len=45) :: description_
        
        integer(kind=8) :: count_rate_s, count_max_s, count_s
        integer(kind=8) :: count_rate_e, count_max_e, count_e

        call left_align_str("OpenBLAS Transposed:", description_, 45)    
        call openblas_set_num_threads(1)
        CALL SYSTEM_CLOCK(count_s, count_rate_s, count_max_s)
        do i=1, max_iter, 1
            call dgemv("T", ldx, lda, 1d0, a_t, ldx, x, 1_8, 0d0, y, 1_8)
        end do
        CALL SYSTEM_CLOCK(count_e, count_rate_e, count_max_e)
        if (print_sum) print*, description_, count_e - count_s, sum(y)
        time = count_e - count_s
    end function run_dgemv_transposed

end program main