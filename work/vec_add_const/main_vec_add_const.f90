program main_vec_add_const
    implicit none
    

contains

    subroutine vec_add_const_01_r8(vec, val, n)
        implicit none
        real(kind=8),    intent(inout) :: vec(n)
        real(kind=8),    intent(in)    :: val
        integer(kind=8), intent(in)    :: n

        integer(kind=8) :: unroll_size, n_unroll, i

        real(kind=8)    :: r00, r01, r02, r03
        real(kind=8)    :: r04, r05, r06, r07
        real(kind=8)    :: r08, r09, r10, r11
        real(kind=8)    :: r12, r13, r14, r15

        unroll_size = 1
        n_unroll = n - mod(n, unroll_size)

        do i=1, n_unroll, unroll_size
            r00 = vec(i)

            r00 = r00 + val

            vec(i) = r00
        end do
    end subroutine vec_add_const_01_r8

    subroutine vec_add_const_02_r8(vec, val, n)
        implicit none
        real(kind=8),    intent(inout) :: vec(n)
        real(kind=8),    intent(in)    :: val
        integer(kind=8), intent(in)    :: n

        integer(kind=8) :: unroll_size, n_unroll, i

        real(kind=8)    :: r00, r01, r02, r03
        real(kind=8)    :: r04, r05, r06, r07
        real(kind=8)    :: r08, r09, r10, r11
        real(kind=8)    :: r12, r13, r14, r15

        unroll_size = 2
        n_unroll = n - mod(n, unroll_size)

        do i=1, n_unroll, unroll_size
            r00 = vec(i)
            r01 = vec(i+1)

            r00 = r00 + val
            r01 = r01 + val

            vec(i)   = r00
            vec(i+1) = r01
        end do

        do i=n_unroll+1, n, 1
            r00 = vec(i)

            r00 = r00 + val

            vec(i) = r00
        end do
    end subroutine vec_add_const_02_r8

    subroutine vec_add_const_04_r8(vec, val, n)
        implicit none
        real(kind=8),    intent(inout) :: vec(n)
        real(kind=8),    intent(in)    :: val
        integer(kind=8), intent(in)    :: n

        integer(kind=8) :: unroll_size, n_unroll, i

        real(kind=8)    :: r00, r01, r02, r03
        real(kind=8)    :: r04, r05, r06, r07
        real(kind=8)    :: r08, r09, r10, r11
        real(kind=8)    :: r12, r13, r14, r15

        unroll_size = 4
        n_unroll = n - mod(n, unroll_size)

        do i=1, n_unroll, unroll_size
            r00 = vec(i)
            r01 = vec(i+1)
            r02 = vec(i+2)
            r03 = vec(i+3)

            r00 = r00 + val
            r01 = r01 + val
            r02 = r02 + val
            r03 = r03 + val

            vec(i)   = r00
            vec(i+1) = r01
            vec(i+2) = r02
            vec(i+3) = r03
        end do

        do i=n_unroll+1, n, 1
            r00 = vec(i)

            r00 = r00 + val

            vec(i) = r00
        end do
    end subroutine vec_add_const_04_r8

    subroutine vec_add_const_08_r8(vec, val, n)
        implicit none
        real(kind=8),    intent(inout) :: vec(n)
        real(kind=8),    intent(in)    :: val
        integer(kind=8), intent(in)    :: n

        integer(kind=8) :: unroll_size, n_unroll, i

        real(kind=8)    :: r00, r01, r02, r03
        real(kind=8)    :: r04, r05, r06, r07
        real(kind=8)    :: r08, r09, r10, r11
        real(kind=8)    :: r12, r13, r14, r15

        unroll_size = 8
        n_unroll = n - mod(n, unroll_size)

        do i=1, n_unroll, unroll_size
            r00 = vec(i)
            r01 = vec(i+1)
            r02 = vec(i+2)
            r03 = vec(i+3)
            r04 = vec(i+4)
            r05 = vec(i+5)
            r06 = vec(i+6)
            r07 = vec(i+7)

            r00 = r00 + val
            r01 = r01 + val
            r02 = r02 + val
            r03 = r03 + val
            r04 = r04 + val
            r05 = r05 + val
            r06 = r06 + val
            r07 = r07 + val

            vec(i)   = r00
            vec(i+1) = r01
            vec(i+2) = r02
            vec(i+3) = r03
            vec(i+4) = r04
            vec(i+5) = r05
            vec(i+6) = r06
            vec(i+7) = r07
        end do

        do i=n_unroll+1, n, 1
            r00 = vec(i)

            r00 = r00 + val

            vec(i) = r00
        end do
    end subroutine vec_add_const_08_r8

    subroutine vec_add_const_16_r8(vec, val, n)
        implicit none
        real(kind=8),    intent(inout) :: vec(n)
        real(kind=8),    intent(in)    :: val
        integer(kind=8), intent(in)    :: n

        integer(kind=8) :: unroll_size, n_unroll, i

        real(kind=8)    :: r00, r01, r02, r03
        real(kind=8)    :: r04, r05, r06, r07
        real(kind=8)    :: r08, r09, r10, r11
        real(kind=8)    :: r12, r13, r14, r15

        unroll_size = 16
        n_unroll = n - mod(n, unroll_size)

        do i=1, n_unroll, unroll_size
            r00 = vec(i)
            r01 = vec(i+1)
            r02 = vec(i+2)
            r03 = vec(i+3)
            r04 = vec(i+4)
            r05 = vec(i+5)
            r06 = vec(i+6)
            r07 = vec(i+7)
            r08 = vec(i+8)
            r09 = vec(i+9)
            r10 = vec(i+10)
            r11 = vec(i+11)
            r12 = vec(i+12)
            r13 = vec(i+13)
            r14 = vec(i+14)
            r15 = vec(i+15)

            r00 = r00 + val
            r01 = r01 + val
            r02 = r02 + val
            r03 = r03 + val
            r04 = r04 + val
            r05 = r05 + val
            r06 = r06 + val
            r07 = r07 + val
            r08 = r08 + val
            r09 = r09 + val
            r10 = r10 + val
            r11 = r11 + val
            r12 = r12 + val
            r13 = r13 + val
            r14 = r14 + val
            r15 = r15 + val

            vec(i)   = r00
            vec(i+1) = r01
            vec(i+2) = r02
            vec(i+3) = r03
            vec(i+4) = r04
            vec(i+5) = r05
            vec(i+6) = r06
            vec(i+7) = r07
            vec(i+8) = r08
            vec(i+9) = r09
            vec(i+10)= r10
            vec(i+11)= r11
            vec(i+12)= r12
            vec(i+13)= r13
            vec(i+14)= r14
            vec(i+15)= r15
        end do

        do i=n_unroll+1, n, 1
            r00 = vec(i)

            r00 = r00 + val

            vec(i) = r00
        end do
    end subroutine vec_add_const_16_r8

end program main_vec_add_const
