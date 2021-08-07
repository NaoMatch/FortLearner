function variance_02_F_r8(vec, n_samples)
    implicit none
    real(kind=8) :: variance_02_F_r8
    real(kind=8), intent(in) :: vec(n_samples)
    integer(kind=8), intent(in) :: n_samples

    real(kind=8) :: avg
    integer(kind=8) :: i, unroll_size, n_samples_unroll, n_samples_remain
    real(kind=8)    :: r00, r01, r14, r15
    avg=0d0

    unroll_size = 2
    n_samples_remain = mod(n_samples, unroll_size)
    n_samples_unroll = n_samples - n_samples_remain

    r15 = 0d0

    do i=1, n_samples_unroll, unroll_size
        r00 = vec(i)
        r01 = vec(i+1)

        avg = avg + r00 + r01

        r00 = r00 ** 2d0
        r01 = r01 ** 2d0

        r00 = r00 + r01

        r15 = r15 + r00
    end do

    do i=n_samples_unroll+1, n_samples, 1
        r00 = vec(i)
        avg = avg + r00

        r00 = r00 ** 2d0

        r15 = r15 + r00
    end do

    avg = avg / dble(n_samples)
    r15 = r15 - avg**2d0 * dble(n_samples)
    variance_02_F_r8 = r15/dble(n_samples)
end function variance_02_F_r8

function variance_04_F_r8(vec, n_samples)
    implicit none
    real(kind=8) :: variance_04_F_r8
    real(kind=8), intent(in) :: vec(n_samples)
    integer(kind=8), intent(in) :: n_samples

    real(kind=8) :: avg
    integer(kind=8) :: i, unroll_size, n_samples_unroll, n_samples_remain
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r14, r15
    avg=0d0

    unroll_size = 4
    n_samples_remain = mod(n_samples, unroll_size)
    n_samples_unroll = n_samples - n_samples_remain

    r15 = 0d0

    do i=1, n_samples_unroll, unroll_size
        r00 = vec(i)
        r01 = vec(i+1)
        r02 = vec(i+2)
        r03 = vec(i+3)

        r04 = r00 + r01
        r05 = r02 + r03
        r04 = r04 + r05
        avg = avg + r04

        r00 = r00 ** 2d0
        r01 = r01 ** 2d0
        r02 = r02 ** 2d0
        r03 = r03 ** 2d0

        r00 = r00 + r01
        r02 = r02 + r03

        r00 = r00 + r02

        r15 = r15 + r00
    end do

    do i=n_samples_unroll+1, n_samples, 1
        r00 = vec(i)
        avg = avg + r00

        r00 = r00 ** 2d0

        r15 = r15 + r00
    end do

    avg = avg / dble(n_samples)
    r15 = r15 - avg**2d0 * dble(n_samples)
    variance_04_F_r8 = r15/dble(n_samples)
end function variance_04_F_r8

function variance_08_F_r8(vec, n_samples)
    implicit none
    real(kind=8) :: variance_08_F_r8
    real(kind=8), intent(in) :: vec(n_samples)
    integer(kind=8), intent(in) :: n_samples

    real(kind=8) :: avg
    integer(kind=8) :: i, unroll_size, n_samples_unroll, n_samples_remain
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r14, r15
    avg=0d0

    unroll_size = 8
    n_samples_remain = mod(n_samples, unroll_size)
    n_samples_unroll = n_samples - n_samples_remain

    r14 = avg
    r15 = 0d0

    do i=1, n_samples_unroll, unroll_size
        r00 = vec(i)
        r01 = vec(i+1)
        r02 = vec(i+2)
        r03 = vec(i+3)
        r04 = vec(i+4)
        r05 = vec(i+5)
        r06 = vec(i+6)
        r07 = vec(i+7)

        r08 = r00 + r01
        r09 = r02 + r03
        r10 = r04 + r05
        r11 = r06 + r07

        r08 = r08 + r09
        r10 = r10 + r11

        r08 = r08 + r10
        avg = avg + r08

        r00 = r00 ** 2d0
        r01 = r01 ** 2d0
        r02 = r02 ** 2d0
        r03 = r03 ** 2d0
        r04 = r04 ** 2d0
        r05 = r05 ** 2d0
        r06 = r06 ** 2d0
        r07 = r07 ** 2d0

        r00 = r00 + r01
        r02 = r02 + r03
        r04 = r04 + r05
        r06 = r06 + r07

        r00 = r00 + r02
        r04 = r04 + r06

        r00 = r00 + r04

        r15 = r15 + r00
    end do

    do i=n_samples_unroll+1, n_samples, 1
        r00 = vec(i)
        avg = avg + r00

        r00 = r00 ** 2d0

        r15 = r15 + r00
    end do

    avg = avg / dble(n_samples)
    r15 = r15 - avg**2d0 * dble(n_samples)
    variance_08_F_r8 = r15/dble(n_samples)
end function variance_08_F_r8

function variance_16_F_r8(vec, n_samples)
    implicit none
    real(kind=8) :: variance_16_F_r8
    real(kind=8), intent(in) :: vec(n_samples)
    integer(kind=8), intent(in) :: n_samples

    real(kind=8) :: avg
    integer(kind=8) :: i, unroll_size, n_samples_unroll, n_samples_remain
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r14, r15
    avg=0d0

    unroll_size = 16
    n_samples_remain = mod(n_samples, unroll_size)
    n_samples_unroll = n_samples - n_samples_remain

    r14 = avg
    r15 = 0d0

    do i=1, n_samples_unroll, unroll_size
        r00 = vec(i)
        r01 = vec(i+1)
        r02 = vec(i+2)
        r03 = vec(i+3)
        r04 = vec(i+4)
        r05 = vec(i+5)
        r06 = vec(i+6)
        r07 = vec(i+7)

        r08 = r00 + r01
        r09 = r02 + r03
        r10 = r04 + r05
        r11 = r06 + r07

        r08 = r08 + r09
        r10 = r10 + r11

        r08 = r08 + r10
        avg = avg + r08

        r00 = r00 ** 2d0
        r01 = r01 ** 2d0
        r02 = r02 ** 2d0
        r03 = r03 ** 2d0
        r04 = r04 ** 2d0
        r05 = r05 ** 2d0
        r06 = r06 ** 2d0
        r07 = r07 ** 2d0

        r00 = r00 + r01
        r02 = r02 + r03
        r04 = r04 + r05
        r06 = r06 + r07

        r00 = r00 + r02
        r04 = r04 + r06

        r00 = r00 + r04

        r15 = r15 + r00

        r00 = vec(i+8)
        r01 = vec(i+9)
        r02 = vec(i+10)
        r03 = vec(i+11)
        r04 = vec(i+12)
        r05 = vec(i+13)
        r06 = vec(i+14)
        r07 = vec(i+15)

        r08 = r00 + r01
        r09 = r02 + r03
        r10 = r04 + r05
        r11 = r06 + r07

        r08 = r08 + r09
        r10 = r10 + r11

        r08 = r08 + r10
        avg = avg + r08

        r00 = r00 ** 2d0
        r01 = r01 ** 2d0
        r02 = r02 ** 2d0
        r03 = r03 ** 2d0
        r04 = r04 ** 2d0
        r05 = r05 ** 2d0
        r06 = r06 ** 2d0
        r07 = r07 ** 2d0

        r00 = r00 + r01
        r02 = r02 + r03
        r04 = r04 + r05
        r06 = r06 + r07

        r00 = r00 + r02
        r04 = r04 + r06

        r00 = r00 + r04

        r15 = r15 + r00
    end do

    do i=n_samples_unroll+1, n_samples, 1
        r00 = vec(i)
        avg = avg + r00

        r00 = r00 ** 2d0

        r15 = r15 + r00
    end do

    avg = avg / dble(n_samples)
    r15 = r15 - avg**2d0 * dble(n_samples)
    variance_16_F_r8 = r15/dble(n_samples)
end function variance_16_F_r8
