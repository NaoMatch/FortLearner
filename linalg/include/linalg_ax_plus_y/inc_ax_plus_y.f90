subroutine ax_plus_y_01_F_r8(a, x, y, n)
    implicit none
    real(kind=8), intent(in)    :: a
    real(kind=8), intent(in)    :: x(n)
    real(kind=8), intent(inout) :: y(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8) :: i
    real(kind=8)    :: r00, r01

    do i=1, n, 1
        r00 = y(i)
        r01 = x(i)

        r00 = r00 + r01 * a

        y(i) = r00
    end do

end subroutine ax_plus_y_01_F_r8

subroutine ax_plus_y_02_F_r8(a, x, y, n)
    implicit none
    real(kind=8), intent(in)    :: a
    real(kind=8), intent(in)    :: x(n)
    real(kind=8), intent(inout) :: y(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8) :: i, unroll_size, n_remain, n_unroll
    real(kind=8)    :: r00, r01
    real(kind=8)    :: r02, r03

    unroll_size = 2
    n_remain = mod(n, unroll_size)
    n_unroll = n - n_remain

    do i=1, n_unroll, unroll_size
        r00 = y(i)
        r01 = x(i)
        r00 = r00 + r01 * a

        r02 = y(i+1)
        r03 = x(i+1)
        r02 = r02 + r03 * a

        y(i)   = r00
        y(i+1) = r02
    end do

    do i=n_unroll+1, n, 1
        r00 = y(i)
        r01 = x(i)
        r00 = r00 + r01 * a

        y(i)   = r00
    end do
end subroutine ax_plus_y_02_F_r8

subroutine ax_plus_y_04_F_r8(a, x, y, n)
    implicit none
    real(kind=8), intent(in)    :: a
    real(kind=8), intent(in)    :: x(n)
    real(kind=8), intent(inout) :: y(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8) :: i, unroll_size, n_remain, n_unroll
    real(kind=8)    :: r00, r01
    real(kind=8)    :: r02, r03
    real(kind=8)    :: r04, r05
    real(kind=8)    :: r06, r07

    unroll_size = 4
    n_remain = mod(n, unroll_size)
    n_unroll = n - n_remain

    do i=1, n_unroll, unroll_size
        r00 = y(i)
        r01 = x(i)
        r00 = r00 + r01 * a

        r02 = y(i+1)
        r03 = x(i+1)
        r02 = r02 + r03 * a

        r04 = y(i+2)
        r05 = x(i+2)
        r04 = r04 + r05 * a

        r06 = y(i+3)
        r07 = x(i+3)
        r06 = r06 + r07 * a

        y(i)   = r00
        y(i+1) = r02
        y(i+2) = r04
        y(i+3) = r06
    end do

    do i=n_unroll+1, n, 1
        r00 = y(i)
        r01 = x(i)
        r00 = r00 + r01 * a

        y(i)   = r00
    end do
end subroutine ax_plus_y_04_F_r8

subroutine ax_plus_y_08_F_r8(a, x, y, n)
    implicit none
    real(kind=8), intent(in)    :: a
    real(kind=8), intent(in)    :: x(n)
    real(kind=8), intent(inout) :: y(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8) :: i, unroll_size, n_remain, n_unroll
    real(kind=8)    :: r00, r01
    real(kind=8)    :: r02, r03
    real(kind=8)    :: r04, r05
    real(kind=8)    :: r06, r07
    real(kind=8)    :: r08, r09
    real(kind=8)    :: r10, r11
    real(kind=8)    :: r12, r13
    real(kind=8)    :: r14, r15

    unroll_size = 8
    n_remain = mod(n, unroll_size)
    n_unroll = n - n_remain

    do i=1, n_unroll, unroll_size
        r00 = y(i)
        r01 = x(i)
        r00 = r00 + r01 * a

        r02 = y(i+1)
        r03 = x(i+1)
        r02 = r02 + r03 * a

        r04 = y(i+2)
        r05 = x(i+2)
        r04 = r04 + r05 * a

        r06 = y(i+3)
        r07 = x(i+3)
        r06 = r06 + r07 * a

        r08 = y(i+4)
        r09 = x(i+4)
        r08 = r08 + r09 * a

        r10 = y(i+5)
        r11 = x(i+5)
        r10 = r10 + r11 * a

        r12 = y(i+6)
        r13 = x(i+6)
        r12 = r12 + r13 * a

        r14 = y(i+7)
        r15 = x(i+7)
        r14 = r14 + r15 * a

        y(i)   = r00
        y(i+1) = r02
        y(i+2) = r04
        y(i+3) = r06
        y(i+4) = r08
        y(i+5) = r10
        y(i+6) = r12
        y(i+7) = r14
    end do

    do i=n_unroll+1, n, 1
        r00 = y(i)
        r01 = x(i)
        r00 = r00 + r01 * a

        y(i)   = r00
    end do
end subroutine ax_plus_y_08_F_r8

subroutine ax_plus_y_16_F_r8(a, x, y, n)
    implicit none
    real(kind=8), intent(in)    :: a
    real(kind=8), intent(in)    :: x(n)
    real(kind=8), intent(inout) :: y(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8) :: i, unroll_size, n_remain, n_unroll
    real(kind=8)    :: r00, r01
    real(kind=8)    :: r02, r03
    real(kind=8)    :: r04, r05
    real(kind=8)    :: r06, r07
    real(kind=8)    :: r08, r09
    real(kind=8)    :: r10, r11
    real(kind=8)    :: r12, r13
    real(kind=8)    :: r14, r15

    unroll_size = 16
    n_remain = mod(n, unroll_size)
    n_unroll = n - n_remain

    do i=1, n_unroll, unroll_size
        r00 = y(i)
        r01 = x(i)
        r00 = r00 + r01 * a

        r02 = y(i+1)
        r03 = x(i+1)
        r02 = r02 + r03 * a

        r04 = y(i+2)
        r05 = x(i+2)
        r04 = r04 + r05 * a

        r06 = y(i+3)
        r07 = x(i+3)
        r06 = r06 + r07 * a

        r08 = y(i+4)
        r09 = x(i+4)
        r08 = r08 + r09 * a

        r10 = y(i+5)
        r11 = x(i+5)
        r10 = r10 + r11 * a

        r12 = y(i+6)
        r13 = x(i+6)
        r12 = r12 + r13 * a

        r14 = y(i+7)
        r15 = x(i+7)
        r14 = r14 + r15 * a

        y(i)   = r00
        y(i+1) = r02
        y(i+2) = r04
        y(i+3) = r06
        y(i+4) = r08
        y(i+5) = r10
        y(i+6) = r12
        y(i+7) = r14

        r00 = y(i+8)
        r01 = x(i+8)
        r00 = r00 + r01 * a

        r02 = y(i+9)
        r03 = x(i+9)
        r02 = r02 + r03 * a

        r04 = y(i+10)
        r05 = x(i+10)
        r04 = r04 + r05 * a

        r06 = y(i+11)
        r07 = x(i+11)
        r06 = r06 + r07 * a

        r08 = y(i+12)
        r09 = x(i+12)
        r08 = r08 + r09 * a

        r10 = y(i+13)
        r11 = x(i+13)
        r10 = r10 + r11 * a

        r12 = y(i+14)
        r13 = x(i+14)
        r12 = r12 + r13 * a

        r14 = y(i+15)
        r15 = x(i+15)
        r14 = r14 + r15 * a

        y(i+8)  = r00
        y(i+9)  = r02
        y(i+10) = r04
        y(i+11) = r06
        y(i+12) = r08
        y(i+13) = r10
        y(i+14) = r12
        y(i+15) = r14
    end do

    do i=n_unroll+1, n, 1
        r00 = y(i)
        r01 = x(i)
        r00 = r00 + r01 * a

        y(i)   = r00
    end do
end subroutine ax_plus_y_16_F_r8
