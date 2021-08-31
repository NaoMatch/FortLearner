subroutine count_and_sum_up_gt_vector2vector_01_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n)
    implicit none
    real(kind=8), intent(in)       :: x_vals(n)
    real(kind=8), intent(in)       :: thr_vals(n)
    real(kind=8), intent(inout)    :: sum_vals(n)
    integer(kind=8), intent(inout) :: cnt_vals(n)
    real(kind=8), intent(in)       :: y_val
    integer(kind=8), intent(in)    :: n

    integer(kind=8) :: i
    real(kind=8)    :: r00, r01, r02
    integer(kind=8) :: r03, r04

    do i=1, n, 1
        r00 = x_vals(i)
        r01 = thr_vals(i)
        r02 = sum_vals(i)
        r03 = cnt_vals(i)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i) = r02
        cnt_vals(i) = r03
    end do
end subroutine count_and_sum_up_gt_vector2vector_01_F_r8


subroutine count_and_sum_up_gt_vector2vector_02_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n)
    implicit none
    real(kind=8), intent(in)       :: x_vals(n)
    real(kind=8), intent(in)       :: thr_vals(n)
    real(kind=8), intent(inout)    :: sum_vals(n)
    integer(kind=8), intent(inout) :: cnt_vals(n)
    real(kind=8), intent(in)       :: y_val
    integer(kind=8), intent(in)    :: n

    integer(kind=8) :: unroll, n_unroll
    integer(kind=8) :: i
    real(kind=8)    :: r00, r01, r02
    integer(kind=8) :: r03, r04
    real(kind=8)    :: r05, r06, r07
    integer(kind=8) :: r08, r09

    unroll = 2
    n_unroll = n - mod(n, unroll)

    do i=1, n_unroll, unroll
        r00 = x_vals(i)
        r01 = thr_vals(i)
        r02 = sum_vals(i)
        r03 = cnt_vals(i)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i) = r02
        cnt_vals(i) = r03

        r05 = x_vals(i+1)
        r06 = thr_vals(i+1)
        r07 = sum_vals(i+1)
        r08 = cnt_vals(i+1)

        r09 = r05 .gt. r06

        r07 = r07 + r09 * y_val
        r08 = r08 + r09

        sum_vals(i+1) = r07
        cnt_vals(i+1) = r08
    end do

    do i=n_unroll+1, n, 1
        r00 = x_vals(i)
        r01 = thr_vals(i)
        r02 = sum_vals(i)
        r03 = cnt_vals(i)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i) = r02
        cnt_vals(i) = r03
    end do
end subroutine count_and_sum_up_gt_vector2vector_02_F_r8

subroutine count_and_sum_up_gt_vector2vector_04_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n)
    implicit none
    real(kind=8), intent(in)       :: x_vals(n)
    real(kind=8), intent(in)       :: thr_vals(n)
    real(kind=8), intent(inout)    :: sum_vals(n)
    integer(kind=8), intent(inout) :: cnt_vals(n)
    real(kind=8), intent(in)       :: y_val
    integer(kind=8), intent(in)    :: n

    integer(kind=8) :: unroll, n_unroll
    integer(kind=8) :: i
    real(kind=8)    :: r00, r01, r02
    integer(kind=8) :: r03, r04
    real(kind=8)    :: r05, r06, r07
    integer(kind=8) :: r08, r09
    real(kind=8)    :: r10, r11, r12
    integer(kind=8) :: r13, r14

    unroll = 4
    n_unroll = n - mod(n, unroll)

    do i=1, n_unroll, unroll
        ! -------------------------------------
        r00 = x_vals(i)
        r01 = thr_vals(i)
        r02 = sum_vals(i)
        r03 = cnt_vals(i)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i) = r02
        cnt_vals(i) = r03

        ! -------------------------------------
        r05 = x_vals(i+1)
        r06 = thr_vals(i+1)
        r07 = sum_vals(i+1)
        r08 = cnt_vals(i+1)

        r09 = r05 .gt. r06

        r07 = r07 + r09 * y_val
        r08 = r08 + r09

        sum_vals(i+1) = r07
        cnt_vals(i+1) = r08

        ! -------------------------------------
        r10 = x_vals(i+2)
        r11 = thr_vals(i+2)
        r12 = sum_vals(i+2)
        r13 = cnt_vals(i+2)

        r14 = r10 .gt. r11

        r12 = r12 + r14 * y_val
        r13 = r13 + r14

        sum_vals(i+2) = r12
        cnt_vals(i+2) = r13

        ! -------------------------------------
        r00 = x_vals(i+3)
        r01 = thr_vals(i+3)
        r02 = sum_vals(i+3)
        r03 = cnt_vals(i+3)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i+3) = r02
        cnt_vals(i+3) = r03
    end do

    do i=n_unroll+1, n, 1
        r00 = x_vals(i)
        r01 = thr_vals(i)
        r02 = sum_vals(i)
        r03 = cnt_vals(i)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i) = r02
        cnt_vals(i) = r03
    end do
end subroutine count_and_sum_up_gt_vector2vector_04_F_r8

subroutine count_and_sum_up_gt_vector2vector_08_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n)
    implicit none
    real(kind=8), intent(in)       :: x_vals(n)
    real(kind=8), intent(in)       :: thr_vals(n)
    real(kind=8), intent(inout)    :: sum_vals(n)
    integer(kind=8), intent(inout) :: cnt_vals(n)
    real(kind=8), intent(in)       :: y_val
    integer(kind=8), intent(in)    :: n

    integer(kind=8) :: unroll, n_unroll
    integer(kind=8) :: i
    real(kind=8)    :: r00, r01, r02
    integer(kind=8) :: r03, r04
    real(kind=8)    :: r05, r06, r07
    integer(kind=8) :: r08, r09
    real(kind=8)    :: r10, r11, r12
    integer(kind=8) :: r13, r14

    unroll = 8
    n_unroll = n - mod(n, unroll)

    do i=1, n_unroll, unroll
        ! -------------------------------------
        r00 = x_vals(i)
        r01 = thr_vals(i)
        r02 = sum_vals(i)
        r03 = cnt_vals(i)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i) = r02
        cnt_vals(i) = r03

        ! -------------------------------------
        r05 = x_vals(i+1)
        r06 = thr_vals(i+1)
        r07 = sum_vals(i+1)
        r08 = cnt_vals(i+1)

        r09 = r05 .gt. r06

        r07 = r07 + r09 * y_val
        r08 = r08 + r09

        sum_vals(i+1) = r07
        cnt_vals(i+1) = r08

        ! -------------------------------------
        r10 = x_vals(i+2)
        r11 = thr_vals(i+2)
        r12 = sum_vals(i+2)
        r13 = cnt_vals(i+2)

        r14 = r10 .gt. r11

        r12 = r12 + r14 * y_val
        r13 = r13 + r14

        sum_vals(i+2) = r12
        cnt_vals(i+2) = r13

        ! -------------------------------------
        r00 = x_vals(i+3)
        r01 = thr_vals(i+3)
        r02 = sum_vals(i+3)
        r03 = cnt_vals(i+3)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i+3) = r02
        cnt_vals(i+3) = r03

        ! -------------------------------------
        r05 = x_vals(i+4)
        r06 = thr_vals(i+4)
        r07 = sum_vals(i+4)
        r08 = cnt_vals(i+4)

        r09 = r05 .gt. r06

        r07 = r07 + r09 * y_val
        r08 = r08 + r09

        sum_vals(i+4) = r07
        cnt_vals(i+4) = r08

        ! -------------------------------------
        r10 = x_vals(i+5)
        r11 = thr_vals(i+5)
        r12 = sum_vals(i+5)
        r13 = cnt_vals(i+5)

        r14 = r10 .gt. r11

        r12 = r12 + r14 * y_val
        r13 = r13 + r14

        sum_vals(i+5) = r12
        cnt_vals(i+5) = r13

        ! -------------------------------------
        r00 = x_vals(i+6)
        r01 = thr_vals(i+6)
        r02 = sum_vals(i+6)
        r03 = cnt_vals(i+6)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i+6) = r02
        cnt_vals(i+6) = r03

        ! -------------------------------------
        r05 = x_vals(i+7)
        r06 = thr_vals(i+7)
        r07 = sum_vals(i+7)
        r08 = cnt_vals(i+7)

        r09 = r05 .gt. r06

        r07 = r07 + r09 * y_val
        r08 = r08 + r09

        sum_vals(i+7) = r07
        cnt_vals(i+7) = r08
    end do

    do i=n_unroll+1, n, 1
        r00 = x_vals(i)
        r01 = thr_vals(i)
        r02 = sum_vals(i)
        r03 = cnt_vals(i)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i) = r02
        cnt_vals(i) = r03
    end do
end subroutine count_and_sum_up_gt_vector2vector_08_F_r8

subroutine count_and_sum_up_gt_vector2vector_16_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n)
    implicit none
    real(kind=8), intent(in)       :: x_vals(n)
    real(kind=8), intent(in)       :: thr_vals(n)
    real(kind=8), intent(inout)    :: sum_vals(n)
    integer(kind=8), intent(inout) :: cnt_vals(n)
    real(kind=8), intent(in)       :: y_val
    integer(kind=8), intent(in)    :: n

    integer(kind=8) :: unroll, n_unroll
    integer(kind=8) :: i
    real(kind=8)    :: r00, r01, r02
    integer(kind=8) :: r03, r04
    real(kind=8)    :: r05, r06, r07
    integer(kind=8) :: r08, r09
    real(kind=8)    :: r10, r11, r12
    integer(kind=8) :: r13, r14

    unroll = 16
    n_unroll = n - mod(n, unroll)

    do i=1, n_unroll, unroll
        ! -------------------------------------
        r00 = x_vals(i)
        r01 = thr_vals(i)
        r02 = sum_vals(i)
        r03 = cnt_vals(i)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i) = r02
        cnt_vals(i) = r03

        ! -------------------------------------
        r05 = x_vals(i+1)
        r06 = thr_vals(i+1)
        r07 = sum_vals(i+1)
        r08 = cnt_vals(i+1)

        r09 = r05 .gt. r06

        r07 = r07 + r09 * y_val
        r08 = r08 + r09

        sum_vals(i+1) = r07
        cnt_vals(i+1) = r08

        ! -------------------------------------
        r10 = x_vals(i+2)
        r11 = thr_vals(i+2)
        r12 = sum_vals(i+2)
        r13 = cnt_vals(i+2)

        r14 = r10 .gt. r11

        r12 = r12 + r14 * y_val
        r13 = r13 + r14

        sum_vals(i+2) = r12
        cnt_vals(i+2) = r13

        ! -------------------------------------
        r00 = x_vals(i+3)
        r01 = thr_vals(i+3)
        r02 = sum_vals(i+3)
        r03 = cnt_vals(i+3)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i+3) = r02
        cnt_vals(i+3) = r03

        ! -------------------------------------
        r05 = x_vals(i+4)
        r06 = thr_vals(i+4)
        r07 = sum_vals(i+4)
        r08 = cnt_vals(i+4)

        r09 = r05 .gt. r06

        r07 = r07 + r09 * y_val
        r08 = r08 + r09

        sum_vals(i+4) = r07
        cnt_vals(i+4) = r08

        ! -------------------------------------
        r10 = x_vals(i+5)
        r11 = thr_vals(i+5)
        r12 = sum_vals(i+5)
        r13 = cnt_vals(i+5)

        r14 = r10 .gt. r11

        r12 = r12 + r14 * y_val
        r13 = r13 + r14

        sum_vals(i+5) = r12
        cnt_vals(i+5) = r13

        ! -------------------------------------
        r00 = x_vals(i+6)
        r01 = thr_vals(i+6)
        r02 = sum_vals(i+6)
        r03 = cnt_vals(i+6)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i+6) = r02
        cnt_vals(i+6) = r03

        ! -------------------------------------
        r05 = x_vals(i+7)
        r06 = thr_vals(i+7)
        r07 = sum_vals(i+7)
        r08 = cnt_vals(i+7)

        r09 = r05 .gt. r06

        r07 = r07 + r09 * y_val
        r08 = r08 + r09

        sum_vals(i+7) = r07
        cnt_vals(i+7) = r08

        ! -------------------------------------
        r10 = x_vals(i+8)
        r11 = thr_vals(i+8)
        r12 = sum_vals(i+8)
        r13 = cnt_vals(i+8)

        r14 = r10 .gt. r11

        r12 = r12 + r14 * y_val
        r13 = r13 + r14

        sum_vals(i+8) = r12
        cnt_vals(i+8) = r13

        ! -------------------------------------
        r00 = x_vals(i+9)
        r01 = thr_vals(i+9)
        r02 = sum_vals(i+9)
        r03 = cnt_vals(i+9)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i+9) = r02
        cnt_vals(i+9) = r03

        ! -------------------------------------
        r05 = x_vals(i+10)
        r06 = thr_vals(i+10)
        r07 = sum_vals(i+10)
        r08 = cnt_vals(i+10)

        r09 = r05 .gt. r06

        r07 = r07 + r09 * y_val
        r08 = r08 + r09

        sum_vals(i+10) = r07
        cnt_vals(i+10) = r08

        ! -------------------------------------
        r10 = x_vals(i+11)
        r11 = thr_vals(i+11)
        r12 = sum_vals(i+11)
        r13 = cnt_vals(i+11)

        r14 = r10 .gt. r11

        r12 = r12 + r14 * y_val
        r13 = r13 + r14

        sum_vals(i+11) = r12
        cnt_vals(i+11) = r13

        ! -------------------------------------
        r00 = x_vals(i+12)
        r01 = thr_vals(i+12)
        r02 = sum_vals(i+12)
        r03 = cnt_vals(i+12)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i+12) = r02
        cnt_vals(i+12) = r03

        ! -------------------------------------
        r05 = x_vals(i+13)
        r06 = thr_vals(i+13)
        r07 = sum_vals(i+13)
        r08 = cnt_vals(i+13)

        r09 = r05 .gt. r06

        r07 = r07 + r09 * y_val
        r08 = r08 + r09

        sum_vals(i+13) = r07
        cnt_vals(i+13) = r08

        ! -------------------------------------
        r10 = x_vals(i+14)
        r11 = thr_vals(i+14)
        r12 = sum_vals(i+14)
        r13 = cnt_vals(i+14)

        r14 = r10 .gt. r11

        r12 = r12 + r14 * y_val
        r13 = r13 + r14

        sum_vals(i+14) = r12
        cnt_vals(i+14) = r13

        ! -------------------------------------
        r00 = x_vals(i+15)
        r01 = thr_vals(i+15)
        r02 = sum_vals(i+15)
        r03 = cnt_vals(i+15)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i+15) = r02
        cnt_vals(i+15) = r03
    end do

    do i=n_unroll+1, n, 1
        r00 = x_vals(i)
        r01 = thr_vals(i)
        r02 = sum_vals(i)
        r03 = cnt_vals(i)

        r04 = r00 .gt. r01

        r02 = r02 + r04 * y_val
        r03 = r03 + r04

        sum_vals(i) = r02
        cnt_vals(i) = r03
    end do
end subroutine count_and_sum_up_gt_vector2vector_16_F_r8
