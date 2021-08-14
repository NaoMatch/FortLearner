function covariance_loop_F(vec1, vec2, n)
    implicit none
    real(kind=8)                :: covariance_loop_F
    real(kind=8), intent(in)    :: vec1(n), vec2(n)
    integer(kind=8), intent(in) :: n

    real(kind=8)    :: avg1, avg2, tmp
    real(kind=8)    :: r00, r01, r02, r03, r04
    integer(kind=8) :: i

    r02 = 0
    r03 = 0
    r04 = 0
    do i=1, n, 1
        r00 = vec1(i)
        r01 = vec2(i)

        r02 = r02 + r00
        r03 = r03 + r01

        r04 = r04 + r00 * r01
    end do

    covariance_loop_F = r04/dble(n) - r02*r03/dble(n)/dble(n)
end function covariance_loop_F

function covariance_loop_02_F(vec1, vec2, n)
    implicit none
    real(kind=8)                :: covariance_loop_02_F
    real(kind=8), intent(in)    :: vec1(n), vec2(n)
    integer(kind=8), intent(in) :: n

    real(kind=8)    :: avg1, avg2, tmp
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    integer(kind=8) :: i
    integer(kind=8) :: unroll, n_unroll

    unroll = 2
    n_unroll = n - mod(n,unroll)

    r04 = 0
    r05 = 0
    r06 = 0
    do i=1, n_unroll, unroll
        r00 = vec1(i)
        r01 = vec2(i)
        r02 = vec1(i+1)
        r03 = vec2(i+1)

        r04 = r04 + r00
        r05 = r05 + r01
        r06 = r06 + r00 * r01

        r04 = r04 + r02
        r05 = r05 + r03
        r06 = r06 + r02 * r03
    end do

    do i=n_unroll+1, n, 1
        r00 = vec1(i)
        r01 = vec2(i)

        r04 = r04 + r00
        r05 = r05 + r01
        r06 = r06 + r00 * r01
    end do

    covariance_loop_02_F = r06/dble(n) - r04*r05/dble(n)/dble(n)
end function covariance_loop_02_F

function covariance_loop_04_F(vec1, vec2, n)
    implicit none
    real(kind=8)                :: covariance_loop_04_F
    real(kind=8), intent(in)    :: vec1(n), vec2(n)
    integer(kind=8), intent(in) :: n

    real(kind=8)    :: avg1, avg2, tmp
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15
    integer(kind=8) :: i
    integer(kind=8) :: unroll, n_unroll

    unroll = 4
    n_unroll = n - mod(n,unroll)

    r08 = 0
    r09 = 0
    r10 = 0
    do i=1, n_unroll, unroll
        r00 = vec1(i)
        r01 = vec2(i)
        r02 = vec1(i+1)
        r03 = vec2(i+1)
        r04 = vec1(i+2)
        r05 = vec2(i+2)
        r06 = vec1(i+3)
        r07 = vec2(i+3)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r00 * r01

        r08 = r08 + r02
        r09 = r09 + r03
        r10 = r10 + r02 * r03

        r08 = r08 + r04
        r09 = r09 + r05
        r10 = r10 + r04 * r05

        r08 = r08 + r06
        r09 = r09 + r07
        r10 = r10 + r06 * r07

    end do

    do i=n_unroll+1, n, 1
        r00 = vec1(i)
        r01 = vec2(i)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r00 * r01
    end do

    covariance_loop_04_F = r10/dble(n) - r08*r09/dble(n)/dble(n)
end function covariance_loop_04_F

function covariance_loop_08_F(vec1, vec2, n)
    implicit none
    real(kind=8)                :: covariance_loop_08_F
    real(kind=8), intent(in)    :: vec1(n), vec2(n)
    integer(kind=8), intent(in) :: n

    real(kind=8)    :: avg1, avg2, tmp
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15
    integer(kind=8) :: i
    integer(kind=8) :: unroll, n_unroll

    unroll = 8
    n_unroll = n - mod(n,unroll)

    r08 = 0
    r09 = 0
    r10 = 0
    do i=1, n_unroll, unroll
        r00 = vec1(i)
        r01 = vec2(i)
        r02 = vec1(i+1)
        r03 = vec2(i+1)
        r04 = vec1(i+2)
        r05 = vec2(i+2)
        r06 = vec1(i+3)
        r07 = vec2(i+3)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r00 * r01

        r08 = r08 + r02
        r09 = r09 + r03
        r10 = r10 + r02 * r03

        r08 = r08 + r04
        r09 = r09 + r05
        r10 = r10 + r04 * r05

        r08 = r08 + r06
        r09 = r09 + r07
        r10 = r10 + r06 * r07

        r00 = vec1(i+4)
        r01 = vec2(i+4)
        r02 = vec1(i+5)
        r03 = vec2(i+5)
        r04 = vec1(i+6)
        r05 = vec2(i+6)
        r06 = vec1(i+7)
        r07 = vec2(i+7)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r00 * r01

        r08 = r08 + r02
        r09 = r09 + r03
        r10 = r10 + r02 * r03

        r08 = r08 + r04
        r09 = r09 + r05
        r10 = r10 + r04 * r05

        r08 = r08 + r06
        r09 = r09 + r07
        r10 = r10 + r06 * r07

    end do

    do i=n_unroll+1, n, 1
        r00 = vec1(i)
        r01 = vec2(i)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r00 * r01
    end do

    covariance_loop_08_F = r10/dble(n) - r08*r09/dble(n)/dble(n)
end function covariance_loop_08_F

function covariance_loop_16_F(vec1, vec2, n)
    implicit none
    real(kind=8)                :: covariance_loop_16_F
    real(kind=8), intent(in)    :: vec1(n), vec2(n)
    integer(kind=8), intent(in) :: n

    real(kind=8)    :: avg1, avg2, tmp
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15
    integer(kind=8) :: i
    integer(kind=8) :: unroll, n_unroll

    unroll = 16
    n_unroll = n - mod(n,unroll)

    r08 = 0
    r09 = 0
    r10 = 0
    do i=1, n_unroll, unroll
        r00 = vec1(i)
        r01 = vec2(i)
        r02 = vec1(i+1)
        r03 = vec2(i+1)
        r04 = vec1(i+2)
        r05 = vec2(i+2)
        r06 = vec1(i+3)
        r07 = vec2(i+3)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r00 * r01

        r08 = r08 + r02
        r09 = r09 + r03
        r10 = r10 + r02 * r03

        r08 = r08 + r04
        r09 = r09 + r05
        r10 = r10 + r04 * r05

        r08 = r08 + r06
        r09 = r09 + r07
        r10 = r10 + r06 * r07

        r00 = vec1(i+4)
        r01 = vec2(i+4)
        r02 = vec1(i+5)
        r03 = vec2(i+5)
        r04 = vec1(i+6)
        r05 = vec2(i+6)
        r06 = vec1(i+7)
        r07 = vec2(i+7)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r00 * r01

        r08 = r08 + r02
        r09 = r09 + r03
        r10 = r10 + r02 * r03

        r08 = r08 + r04
        r09 = r09 + r05
        r10 = r10 + r04 * r05

        r08 = r08 + r06
        r09 = r09 + r07
        r10 = r10 + r06 * r07

        r00 = vec1(i+8)
        r01 = vec2(i+8)
        r02 = vec1(i+9)
        r03 = vec2(i+9)
        r04 = vec1(i+10)
        r05 = vec2(i+10)
        r06 = vec1(i+11)
        r07 = vec2(i+11)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r00 * r01

        r08 = r08 + r02
        r09 = r09 + r03
        r10 = r10 + r02 * r03

        r08 = r08 + r04
        r09 = r09 + r05
        r10 = r10 + r04 * r05

        r08 = r08 + r06
        r09 = r09 + r07
        r10 = r10 + r06 * r07

        r00 = vec1(i+12)
        r01 = vec2(i+12)
        r02 = vec1(i+13)
        r03 = vec2(i+13)
        r04 = vec1(i+14)
        r05 = vec2(i+14)
        r06 = vec1(i+15)
        r07 = vec2(i+15)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r00 * r01

        r08 = r08 + r02
        r09 = r09 + r03
        r10 = r10 + r02 * r03

        r08 = r08 + r04
        r09 = r09 + r05
        r10 = r10 + r04 * r05

        r08 = r08 + r06
        r09 = r09 + r07
        r10 = r10 + r06 * r07

    end do

    do i=n_unroll+1, n, 1
        r00 = vec1(i)
        r01 = vec2(i)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r00 * r01
    end do

    covariance_loop_16_F = r10/dble(n) - r08*r09/dble(n)/dble(n)
end function covariance_loop_16_F
