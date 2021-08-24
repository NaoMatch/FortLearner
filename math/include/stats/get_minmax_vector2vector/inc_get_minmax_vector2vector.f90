subroutine get_minmax_vector2vector_01_F_r8(vals, min_vals, max_vals, n)
    implicit none
    real(kind=8), intent(in)    :: vals(n)
    real(kind=8), intent(inout) :: min_vals(n), max_vals(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8):: i
    real(kind=8):: r00, r01, r02

    do i=1, n, 1
        r00 = vals(i)

        r01 = min_vals(i)
        r02 = max_vals(i)

        r01 = minval( (/r00, r01/) )
        r02 = maxval( (/r00, r02/) )

        min_vals(i) = r01
        max_vals(i) = r02
    end do
end subroutine get_minmax_vector2vector_01_F_r8

subroutine get_minmax_vector2vector_02_F_r8(vals, min_vals, max_vals, n)
    implicit none
    real(kind=8), intent(in)    :: vals(n)
    real(kind=8), intent(inout) :: min_vals(n), max_vals(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8):: i, unroll, n_unroll
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    unroll = 2
    n_unroll = n - mod(n, unroll)
    do i=1, n_unroll, unroll
        r00 = vals(i)
        r01 = vals(i+1)

        r02 = min_vals(i)
        r03 = max_vals(i)
        r04 = min_vals(i+1)
        r05 = max_vals(i+1)

        r02 = minval( (/r00, r02/) )
        r03 = maxval( (/r00, r03/) )
        r04 = minval( (/r01, r04/) )
        r05 = maxval( (/r01, r05/) )

        min_vals(i) = r02
        max_vals(i) = r03
        min_vals(i+1) = r04
        max_vals(i+1) = r05
    end do

    do i=n_unroll+1, n, 1
        r00 = vals(i)
        r01 = min_vals(i)
        r02 = max_vals(i)

        r01 = minval( (/r00, r01/) )
        r02 = maxval( (/r00, r02/) )

        min_vals(i) = r01
        max_vals(i) = r02
    end do
end subroutine get_minmax_vector2vector_02_F_r8

subroutine get_minmax_vector2vector_04_F_r8(vals, min_vals, max_vals, n)
    implicit none
    real(kind=8), intent(in)    :: vals(n)
    real(kind=8), intent(inout) :: min_vals(n), max_vals(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8):: i, unroll, n_unroll
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    unroll = 4
    n_unroll = n - mod(n, unroll)
    do i=1, n_unroll, unroll
        r00 = vals(i)
        r01 = vals(i+1)
        r02 = vals(i+2)
        r03 = vals(i+3)

        r04 = min_vals(i)
        r05 = max_vals(i)
        r06 = min_vals(i+1)
        r07 = max_vals(i+1)
        r08 = min_vals(i+2)
        r09 = max_vals(i+2)
        r10 = min_vals(i+3)
        r11 = max_vals(i+3)

        r04 = minval( (/r00, r04/) )
        r05 = maxval( (/r00, r05/) )
        r06 = minval( (/r01, r06/) )
        r07 = maxval( (/r01, r07/) )
        r08 = minval( (/r02, r08/) )
        r09 = maxval( (/r02, r09/) )
        r10 = minval( (/r03, r10/) )
        r11 = maxval( (/r03, r11/) )

        min_vals(i) = r04
        max_vals(i) = r05
        min_vals(i+1) = r06
        max_vals(i+1) = r07
        min_vals(i+2) = r08
        max_vals(i+2) = r09
        min_vals(i+3) = r10
        max_vals(i+3) = r11
    end do

    do i=n_unroll+1, n, 1
        r00 = vals(i)
        r01 = min_vals(i)
        r02 = max_vals(i)

        r01 = minval( (/r00, r01/) )
        r02 = maxval( (/r00, r02/) )

        min_vals(i) = r01
        max_vals(i) = r02
    end do
end subroutine get_minmax_vector2vector_04_F_r8

subroutine get_minmax_vector2vector_08_F_r8(vals, min_vals, max_vals, n)
    implicit none
    real(kind=8), intent(in)    :: vals(n)
    real(kind=8), intent(inout) :: min_vals(n), max_vals(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8):: i, unroll, n_unroll
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    unroll = 8
    n_unroll = n - mod(n, unroll)
    do i=1, n_unroll, unroll
        r00 = vals(i)
        r01 = vals(i+1)
        r02 = vals(i+2)
        r03 = vals(i+3)

        r04 = min_vals(i)
        r05 = max_vals(i)
        r06 = min_vals(i+1)
        r07 = max_vals(i+1)
        r08 = min_vals(i+2)
        r09 = max_vals(i+2)
        r10 = min_vals(i+3)
        r11 = max_vals(i+3)

        r04 = minval( (/r00, r04/) )
        r05 = maxval( (/r00, r05/) )
        r06 = minval( (/r01, r06/) )
        r07 = maxval( (/r01, r07/) )
        r08 = minval( (/r02, r08/) )
        r09 = maxval( (/r02, r09/) )
        r10 = minval( (/r03, r10/) )
        r11 = maxval( (/r03, r11/) )

        min_vals(i) = r04
        max_vals(i) = r05
        min_vals(i+1) = r06
        max_vals(i+1) = r07
        min_vals(i+2) = r08
        max_vals(i+2) = r09
        min_vals(i+3) = r10
        max_vals(i+3) = r11

        r00 = vals(i+4)
        r01 = vals(i+5)
        r02 = vals(i+6)
        r03 = vals(i+7)

        r04 = min_vals(i+4)
        r05 = max_vals(i+4)
        r06 = min_vals(i+5)
        r07 = max_vals(i+5)
        r08 = min_vals(i+6)
        r09 = max_vals(i+6)
        r10 = min_vals(i+7)
        r11 = max_vals(i+7)

        r04 = minval( (/r00, r04/) )
        r05 = maxval( (/r00, r05/) )
        r06 = minval( (/r01, r06/) )
        r07 = maxval( (/r01, r07/) )
        r08 = minval( (/r02, r08/) )
        r09 = maxval( (/r02, r09/) )
        r10 = minval( (/r03, r10/) )
        r11 = maxval( (/r03, r11/) )

        min_vals(i+4) = r04
        max_vals(i+4) = r05
        min_vals(i+5) = r06
        max_vals(i+5) = r07
        min_vals(i+6) = r08
        max_vals(i+6) = r09
        min_vals(i+7) = r10
        max_vals(i+7) = r11
    end do

    do i=n_unroll+1, n, 1
        r00 = vals(i)
        r01 = min_vals(i)
        r02 = max_vals(i)

        r01 = minval( (/r00, r01/) )
        r02 = maxval( (/r00, r02/) )

        min_vals(i) = r01
        max_vals(i) = r02
    end do
end subroutine get_minmax_vector2vector_08_F_r8

subroutine get_minmax_vector2vector_16_F_r8(vals, min_vals, max_vals, n)
    implicit none
    real(kind=8), intent(in)    :: vals(n)
    real(kind=8), intent(inout) :: min_vals(n), max_vals(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8):: i, unroll, n_unroll
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    unroll = 16
    n_unroll = n - mod(n, unroll)
    do i=1, n_unroll, unroll
        r00 = vals(i)
        r01 = vals(i+1)
        r02 = vals(i+2)
        r03 = vals(i+3)

        r04 = min_vals(i)
        r05 = max_vals(i)
        r06 = min_vals(i+1)
        r07 = max_vals(i+1)
        r08 = min_vals(i+2)
        r09 = max_vals(i+2)
        r10 = min_vals(i+3)
        r11 = max_vals(i+3)

        r04 = minval( (/r00, r04/) )
        r05 = maxval( (/r00, r05/) )
        r06 = minval( (/r01, r06/) )
        r07 = maxval( (/r01, r07/) )
        r08 = minval( (/r02, r08/) )
        r09 = maxval( (/r02, r09/) )
        r10 = minval( (/r03, r10/) )
        r11 = maxval( (/r03, r11/) )

        min_vals(i) = r04
        max_vals(i) = r05
        min_vals(i+1) = r06
        max_vals(i+1) = r07
        min_vals(i+2) = r08
        max_vals(i+2) = r09
        min_vals(i+3) = r10
        max_vals(i+3) = r11

        r00 = vals(i+4)
        r01 = vals(i+5)
        r02 = vals(i+6)
        r03 = vals(i+7)

        r04 = min_vals(i+4)
        r05 = max_vals(i+4)
        r06 = min_vals(i+5)
        r07 = max_vals(i+5)
        r08 = min_vals(i+6)
        r09 = max_vals(i+6)
        r10 = min_vals(i+7)
        r11 = max_vals(i+7)

        r04 = minval( (/r00, r04/) )
        r05 = maxval( (/r00, r05/) )
        r06 = minval( (/r01, r06/) )
        r07 = maxval( (/r01, r07/) )
        r08 = minval( (/r02, r08/) )
        r09 = maxval( (/r02, r09/) )
        r10 = minval( (/r03, r10/) )
        r11 = maxval( (/r03, r11/) )

        min_vals(i+4) = r04
        max_vals(i+4) = r05
        min_vals(i+5) = r06
        max_vals(i+5) = r07
        min_vals(i+6) = r08
        max_vals(i+6) = r09
        min_vals(i+7) = r10
        max_vals(i+7) = r11

        r00 = vals(i+8)
        r01 = vals(i+9)
        r02 = vals(i+10)
        r03 = vals(i+11)

        r04 = min_vals(i+8)
        r05 = max_vals(i+8)
        r06 = min_vals(i+9)
        r07 = max_vals(i+9)
        r08 = min_vals(i+10)
        r09 = max_vals(i+10)
        r10 = min_vals(i+11)
        r11 = max_vals(i+11)

        r04 = minval( (/r00, r04/) )
        r05 = maxval( (/r00, r05/) )
        r06 = minval( (/r01, r06/) )
        r07 = maxval( (/r01, r07/) )
        r08 = minval( (/r02, r08/) )
        r09 = maxval( (/r02, r09/) )
        r10 = minval( (/r03, r10/) )
        r11 = maxval( (/r03, r11/) )

        min_vals(i+8) = r04
        max_vals(i+8) = r05
        min_vals(i+9) = r06
        max_vals(i+9) = r07
        min_vals(i+10) = r08
        max_vals(i+10) = r09
        min_vals(i+11) = r10
        max_vals(i+11) = r11

        r00 = vals(i+12)
        r01 = vals(i+13)
        r02 = vals(i+14)
        r03 = vals(i+15)

        r04 = min_vals(i+12)
        r05 = max_vals(i+12)
        r06 = min_vals(i+13)
        r07 = max_vals(i+13)
        r08 = min_vals(i+14)
        r09 = max_vals(i+14)
        r10 = min_vals(i+15)
        r11 = max_vals(i+15)

        r04 = minval( (/r00, r04/) )
        r05 = maxval( (/r00, r05/) )
        r06 = minval( (/r01, r06/) )
        r07 = maxval( (/r01, r07/) )
        r08 = minval( (/r02, r08/) )
        r09 = maxval( (/r02, r09/) )
        r10 = minval( (/r03, r10/) )
        r11 = maxval( (/r03, r11/) )

        min_vals(i+12) = r04
        max_vals(i+12) = r05
        min_vals(i+13) = r06
        max_vals(i+13) = r07
        min_vals(i+14) = r08
        max_vals(i+14) = r09
        min_vals(i+15) = r10
        max_vals(i+15) = r11
    end do

    do i=n_unroll+1, n, 1
        r00 = vals(i)
        r01 = min_vals(i)
        r02 = max_vals(i)

        r01 = minval( (/r00, r01/) )
        r02 = maxval( (/r00, r02/) )

        min_vals(i) = r01
        max_vals(i) = r02
    end do
end subroutine get_minmax_vector2vector_16_F_r8
