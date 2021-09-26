subroutine matrix_sqsum_row_01x01_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=1; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=1; n_columns_unroll=n_columns/c_unroll

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r15 = matrix(i,j)

            r15 = r15 * r15
            r00 = r00 + r15

            matrix_sqsum_vals(i) = r00
        end do
    end do
end subroutine matrix_sqsum_row_01x01_F_r8




subroutine matrix_sqsum_row_02x01_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=2; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=1; n_columns_unroll=n_columns/c_unroll

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            r14 = matrix(i,j)
            r15 = matrix(i+1,j)

            r14 = r14 * r14
            r15 = r15 * r15

            r00 = r00 + r14
            r01 = r01 + r15

            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            r15 = matrix(i,j)

            r15 = r15 * r15
            r00 = r00 + r15

            matrix_sqsum_vals(i) = r00
        end do
    end do
end subroutine matrix_sqsum_row_02x01_F_r8

subroutine matrix_sqsum_row_04x01_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=4; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=1; n_columns_unroll=n_columns/c_unroll

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            r02 = matrix_sqsum_vals(i+2)
            r03 = matrix_sqsum_vals(i+3)
            r12 = matrix(i,j)
            r13 = matrix(i+1,j)
            r14 = matrix(i+2,j)
            r15 = matrix(i+3,j)

            r12 = r12 * r12
            r13 = r13 * r13
            r14 = r14 * r14
            r15 = r15 * r15

            r00 = r00 + r12
            r01 = r01 + r13
            r02 = r02 + r14
            r03 = r03 + r15

            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
            matrix_sqsum_vals(i+2) = r02
            matrix_sqsum_vals(i+3) = r03
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            r15 = matrix(i,j)

            r15 = r15 * r15
            r00 = r00 + r15

            matrix_sqsum_vals(i) = r00
        end do
    end do
end subroutine matrix_sqsum_row_04x01_F_r8

subroutine matrix_sqsum_row_08x01_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=8; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=1; n_columns_unroll=n_columns/c_unroll

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            r02 = matrix_sqsum_vals(i+2)
            r03 = matrix_sqsum_vals(i+3)
            r04 = matrix_sqsum_vals(i+4)
            r05 = matrix_sqsum_vals(i+5)
            r06 = matrix_sqsum_vals(i+6)
            r07 = matrix_sqsum_vals(i+7)
            r08 = matrix(i,j)
            r09 = matrix(i+1,j)
            r10 = matrix(i+2,j)
            r11 = matrix(i+3,j)
            r12 = matrix(i+4,j)
            r13 = matrix(i+5,j)
            r14 = matrix(i+6,j)
            r15 = matrix(i+7,j)

            r08 = r08 * r08
            r09 = r09 * r09
            r10 = r10 * r10
            r11 = r11 * r11
            r12 = r12 * r12
            r13 = r13 * r13
            r14 = r14 * r14
            r15 = r15 * r15

            r00 = r00 + r08
            r01 = r01 + r09
            r02 = r02 + r10
            r03 = r03 + r11
            r04 = r04 + r12
            r05 = r05 + r13
            r06 = r06 + r14
            r07 = r07 + r15

            matrix_sqsum_vals(i) = r00
            matrix_sqsum_vals(i+1) = r01
            matrix_sqsum_vals(i+2) = r02
            matrix_sqsum_vals(i+3) = r03
            matrix_sqsum_vals(i+4) = r04
            matrix_sqsum_vals(i+5) = r05
            matrix_sqsum_vals(i+6) = r06
            matrix_sqsum_vals(i+7) = r07
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            r15 = matrix(i,j)

            r15 = r15 * r15
            r00 = r00 + r15

            matrix_sqsum_vals(i) = r00
        end do
    end do
end subroutine matrix_sqsum_row_08x01_F_r8

subroutine matrix_sqsum_row_16x01_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=16; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=1;  n_columns_unroll=n_columns/c_unroll

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            do k=0, s_unroll-1, s_unroll/2
                r00 = matrix_sqsum_vals(i+k)
                r01 = matrix_sqsum_vals(i+k+1)
                r02 = matrix_sqsum_vals(i+k+2)
                r03 = matrix_sqsum_vals(i+k+3)
                r04 = matrix_sqsum_vals(i+k+4)
                r05 = matrix_sqsum_vals(i+k+5)
                r06 = matrix_sqsum_vals(i+k+6)
                r07 = matrix_sqsum_vals(i+k+7)
                r08 = matrix(i+k,j)
                r09 = matrix(i+k+1,j)
                r10 = matrix(i+k+2,j)
                r11 = matrix(i+k+3,j)
                r12 = matrix(i+k+4,j)
                r13 = matrix(i+k+5,j)
                r14 = matrix(i+k+6,j)
                r15 = matrix(i+k+7,j)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11
                r04 = r04 + r12
                r05 = r05 + r13
                r06 = r06 + r14
                r07 = r07 + r15

                matrix_sqsum_vals(i+k)   = r00
                matrix_sqsum_vals(i+k+1) = r01
                matrix_sqsum_vals(i+k+2) = r02
                matrix_sqsum_vals(i+k+3) = r03
                matrix_sqsum_vals(i+k+4) = r04
                matrix_sqsum_vals(i+k+5) = r05
                matrix_sqsum_vals(i+k+6) = r06
                matrix_sqsum_vals(i+k+7) = r07
            end do
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            r15 = matrix(i,j)

            r15 = r15 * r15
            r00 = r00 + r15

            matrix_sqsum_vals(i) = r00
        end do
    end do
end subroutine matrix_sqsum_row_16x01_F_r8

subroutine matrix_sqsum_row_32x01_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=32; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=1;  n_columns_unroll=n_columns/c_unroll

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            do k=0, s_unroll-1, s_unroll/4
                r00 = matrix_sqsum_vals(i+k)
                r01 = matrix_sqsum_vals(i+k+1)
                r02 = matrix_sqsum_vals(i+k+2)
                r03 = matrix_sqsum_vals(i+k+3)
                r04 = matrix_sqsum_vals(i+k+4)
                r05 = matrix_sqsum_vals(i+k+5)
                r06 = matrix_sqsum_vals(i+k+6)
                r07 = matrix_sqsum_vals(i+k+7)
                r08 = matrix(i+k,j)
                r09 = matrix(i+k+1,j)
                r10 = matrix(i+k+2,j)
                r11 = matrix(i+k+3,j)
                r12 = matrix(i+k+4,j)
                r13 = matrix(i+k+5,j)
                r14 = matrix(i+k+6,j)
                r15 = matrix(i+k+7,j)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11
                r04 = r04 + r12
                r05 = r05 + r13
                r06 = r06 + r14
                r07 = r07 + r15

                matrix_sqsum_vals(i+k)   = r00
                matrix_sqsum_vals(i+k+1) = r01
                matrix_sqsum_vals(i+k+2) = r02
                matrix_sqsum_vals(i+k+3) = r03
                matrix_sqsum_vals(i+k+4) = r04
                matrix_sqsum_vals(i+k+5) = r05
                matrix_sqsum_vals(i+k+6) = r06
                matrix_sqsum_vals(i+k+7) = r07
            end do
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            r15 = matrix(i,j)

            r15 = r15 * r15
            r00 = r00 + r15

            matrix_sqsum_vals(i) = r00
        end do
    end do
end subroutine matrix_sqsum_row_32x01_F_r8




subroutine matrix_sqsum_row_01x02_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=1; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=2; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r14 = matrix(i,j)
            r15 = matrix(i,j+1)

            r14 = r14 * r14
            r15 = r15 * r15

            r14 = r14 + r15

            r00 = r00 + r14

            matrix_sqsum_vals(i) = r00
        end do
    end do

    do j=n_columns_unroll+1, n_columns, 1
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r15 = matrix(i,j)

            r15 = r15 * r15
            r00 = r00 + r15

            matrix_sqsum_vals(i) = r00
        end do
    end do
end subroutine matrix_sqsum_row_01x02_F_r8

subroutine matrix_sqsum_row_01x04_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=1; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=4; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r12 = matrix(i,j)
            r13 = matrix(i,j+1)
            r14 = matrix(i,j+2)
            r15 = matrix(i,j+3)

            r12 = r12 * r12
            r13 = r13 * r13
            r14 = r14 * r14
            r15 = r15 * r15

            r12 = r12 + r13
            r14 = r14 + r15

            r12 = r12 + r14

            r00 = r00 + r12

            matrix_sqsum_vals(i) = r00
        end do
    end do

    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        shift = shift + 2
        do j=n_columns_unroll+1, n_columns_unroll+2, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r14 = matrix(i,j)
                r15 = matrix(i,j+1)

                r14 = r14 * r14
                r15 = r15 * r15

                r14 = r14 + r15

                r00 = r00 + r14

                matrix_sqsum_vals(i) = r00
            end do
        end do
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r15 = matrix(i,j)

                r15 = r15 * r15
                r00 = r00 + r15

                matrix_sqsum_vals(i) = r00
            end do
        end do
    end if
end subroutine matrix_sqsum_row_01x04_F_r8

subroutine matrix_sqsum_row_01x08_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=1; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=8; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r08 = matrix(i,j)
            r09 = matrix(i,j+1)
            r10 = matrix(i,j+2)
            r11 = matrix(i,j+3)
            r12 = matrix(i,j+4)
            r13 = matrix(i,j+5)
            r14 = matrix(i,j+6)
            r15 = matrix(i,j+7)

            r08 = r08 * r08
            r09 = r09 * r09
            r10 = r10 * r10
            r11 = r11 * r11
            r12 = r12 * r12
            r13 = r13 * r13
            r14 = r14 * r14
            r15 = r15 * r15

            r08 = r08 + r09
            r10 = r10 + r11
            r12 = r12 + r13
            r14 = r14 + r15

            r08 = r08 + r10
            r12 = r12 + r14

            r08 = r08 + r12

            r00 = r00 + r08

            matrix_sqsum_vals(i) = r00
        end do
    end do

    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 4
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i,j)
                r13 = matrix(i,j+1)
                r14 = matrix(i,j+2)
                r15 = matrix(i,j+3)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r13
                r14 = r14 + r15

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i) = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r14 = matrix(i,j)
                r15 = matrix(i,j+1)

                r14 = r14 * r14
                r15 = r15 * r15

                r14 = r14 + r15

                r00 = r00 + r14

                matrix_sqsum_vals(i) = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r15 = matrix(i,j)

                r15 = r15 * r15
                r00 = r00 + r15

                matrix_sqsum_vals(i) = r00
            end do
        end do
    end if
end subroutine matrix_sqsum_row_01x08_F_r8

subroutine matrix_sqsum_row_01x16_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=1; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=16; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/2
                r08 = matrix(i,j+k)
                r09 = matrix(i,j+k+1)
                r10 = matrix(i,j+k+2)
                r11 = matrix(i,j+k+3)
                r12 = matrix(i,j+k+4)
                r13 = matrix(i,j+k+5)
                r14 = matrix(i,j+k+6)
                r15 = matrix(i,j+k+7)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r09
                r10 = r10 + r11
                r12 = r12 + r13
                r14 = r14 + r15

                r08 = r08 + r10
                r12 = r12 + r14

                r08 = r08 + r12

                r00 = r00 + r08
            end do
            matrix_sqsum_vals(i) = r00
        end do
    end do

    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 8_8 ) then
        n_columns_remain = n_columns_remain - 8
        do j=n_columns_unroll+shift, n_columns_unroll+shift+7, 8
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r08 = matrix(i,j)
                r09 = matrix(i,j+1)
                r10 = matrix(i,j+2)
                r11 = matrix(i,j+3)
                r12 = matrix(i,j+4)
                r13 = matrix(i,j+5)
                r14 = matrix(i,j+6)
                r15 = matrix(i,j+7)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r09
                r10 = r10 + r11
                r12 = r12 + r13
                r14 = r14 + r15

                r08 = r08 + r10
                r12 = r12 + r14

                r08 = r08 + r12

                r00 = r00 + r08

                matrix_sqsum_vals(i) = r00
        end do
        end do
        shift = shift + 8
    end if

    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 4
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i,j)
                r13 = matrix(i,j+1)
                r14 = matrix(i,j+2)
                r15 = matrix(i,j+3)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r13
                r14 = r14 + r15

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i) = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r14 = matrix(i,j)
                r15 = matrix(i,j+1)

                r14 = r14 * r14
                r15 = r15 * r15

                r14 = r14 + r15

                r00 = r00 + r14

                matrix_sqsum_vals(i) = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r15 = matrix(i,j)

                r15 = r15 * r15
                r00 = r00 + r15

                matrix_sqsum_vals(i) = r00
            end do
        end do
    end if
end subroutine matrix_sqsum_row_01x16_F_r8

subroutine matrix_sqsum_row_01x32_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=1; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=32; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0
    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/4
                r08 = matrix(i,j+k)
                r09 = matrix(i,j+k+1)
                r10 = matrix(i,j+k+2)
                r11 = matrix(i,j+k+3)
                r12 = matrix(i,j+k+4)
                r13 = matrix(i,j+k+5)
                r14 = matrix(i,j+k+6)
                r15 = matrix(i,j+k+7)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r09
                r10 = r10 + r11
                r12 = r12 + r13
                r14 = r14 + r15

                r08 = r08 + r10
                r12 = r12 + r14

                r08 = r08 + r12

                r00 = r00 + r08
            end do
            matrix_sqsum_vals(i) = r00
        end do
    end do

    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 16_8 ) then
        n_columns_remain = n_columns_remain - 16
        do j=n_columns_unroll+shift, n_columns_unroll+shift+15, 16
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                do k=0, 15, 8
                    r08 = matrix(i,j+k)
                    r09 = matrix(i,j+k+1)
                    r10 = matrix(i,j+k+2)
                    r11 = matrix(i,j+k+3)
                    r12 = matrix(i,j+k+4)
                    r13 = matrix(i,j+k+5)
                    r14 = matrix(i,j+k+6)
                    r15 = matrix(i,j+k+7)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r09
                    r10 = r10 + r11
                    r12 = r12 + r13
                    r14 = r14 + r15

                    r08 = r08 + r10
                    r12 = r12 + r14

                    r08 = r08 + r12

                    r00 = r00 + r08
                end do
                matrix_sqsum_vals(i) = r00
            end do
        end do
        shift = shift + 16
    end if

    if ( n_columns_remain .ge. 8_8 ) then
        n_columns_remain = n_columns_remain - 8
        do j=n_columns_unroll+shift, n_columns_unroll+shift+7, 8
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r08 = matrix(i,j)
                r09 = matrix(i,j+1)
                r10 = matrix(i,j+2)
                r11 = matrix(i,j+3)
                r12 = matrix(i,j+4)
                r13 = matrix(i,j+5)
                r14 = matrix(i,j+6)
                r15 = matrix(i,j+7)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r09
                r10 = r10 + r11
                r12 = r12 + r13
                r14 = r14 + r15

                r08 = r08 + r10
                r12 = r12 + r14

                r08 = r08 + r12

                r00 = r00 + r08

                matrix_sqsum_vals(i) = r00
            end do
        end do
        shift = shift + 8
    end if

    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 4
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i,j)
                r13 = matrix(i,j+1)
                r14 = matrix(i,j+2)
                r15 = matrix(i,j+3)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r13
                r14 = r14 + r15

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i) = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r14 = matrix(i,j)
                r15 = matrix(i,j+1)

                r14 = r14 * r14
                r15 = r15 * r15

                r14 = r14 + r15

                r00 = r00 + r14

                matrix_sqsum_vals(i) = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r15 = matrix(i,j)

                r15 = r15 * r15
                r00 = r00 + r15

                matrix_sqsum_vals(i) = r00
            end do
        end do
    end if
end subroutine matrix_sqsum_row_01x32_F_r8




subroutine matrix_sqsum_row_02x02_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=2; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=2; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            r12 = matrix(i+0, j)
            r13 = matrix(i+1, j)
            r14 = matrix(i+0, j+1)
            r15 = matrix(i+1, j+1)

            r12 = r12 * r12
            r13 = r13 * r13
            r14 = r14 * r14
            r15 = r15 * r15

            r12 = r12 + r14
            r13 = r13 + r15

            r00 = r00 + r12
            r01 = r01 + r13

            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            r12 = matrix(i+0, j)
            r14 = matrix(i+0, j+1)

            r12 = r12 * r12
            r14 = r14 * r14

            r12 = r12 + r14

            r00 = r00 + r12

            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r12 = matrix(i+0, j)
                r13 = matrix(i+1, j)

                r12 = r12 * r12
                r13 = r13 * r13

                r00 = r00 + r12
                r01 = r01 + r13

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_02x02_F_r8

subroutine matrix_sqsum_row_02x04_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=2; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=4; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            r08 = matrix(i+0, j)
            r09 = matrix(i+1, j)
            r10 = matrix(i+0, j+1)
            r11 = matrix(i+1, j+1)
            r12 = matrix(i+0, j+2)
            r13 = matrix(i+1, j+2)
            r14 = matrix(i+0, j+3)
            r15 = matrix(i+1, j+3)

            r08 = r08 * r08
            r09 = r09 * r09
            r10 = r10 * r10
            r11 = r11 * r11
            r12 = r12 * r12
            r13 = r13 * r13
            r14 = r14 * r14
            r15 = r15 * r15

            r08 = r08 + r10
            r09 = r09 + r11
            r12 = r12 + r14
            r13 = r13 + r15

            r08 = r08 + r12
            r09 = r09 + r13

            r00 = r00 + r08
            r01 = r01 + r09

            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            r12 = matrix(i+0, j)
            r13 = matrix(i+0, j+1)
            r14 = matrix(i+0, j+2)
            r15 = matrix(i+0, j+3)

            r12 = r12 * r12
            r13 = r13 * r13
            r14 = r14 * r14
            r15 = r15 * r15

            r12 = r12 + r13
            r14 = r14 + r15

            r12 = r12 + r14

            r00 = r00 + r12

            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r12 = matrix(i+0, j)
                r13 = matrix(i+1, j)
                r14 = matrix(i+0, j+1)
                r15 = matrix(i+1, j+1)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r14
                r13 = r13 + r15

                r00 = r00 + r12
                r01 = r01 + r13

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r12 = matrix(i+0, j)
                r13 = matrix(i+1, j)

                r12 = r12 * r12
                r13 = r13 * r13

                r00 = r00 + r12
                r01 = r01 + r13

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_02x04_F_r8

subroutine matrix_sqsum_row_02x08_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=2; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=8; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            do k=0, c_unroll-1, c_unroll/2
                r08 = matrix(i+0, j+k)
                r09 = matrix(i+1, j+k)
                r10 = matrix(i+0, j+k+1)
                r11 = matrix(i+1, j+k+1)
                r12 = matrix(i+0, j+k+2)
                r13 = matrix(i+1, j+k+2)
                r14 = matrix(i+0, j+k+3)
                r15 = matrix(i+1, j+k+3)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r10
                r09 = r09 + r11
                r12 = r12 + r14
                r13 = r13 + r15

                r08 = r08 + r12
                r09 = r09 + r13

                r00 = r00 + r08
                r01 = r01 + r09
            end do
            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/2
                r12 = matrix(i+0, j+k)
                r13 = matrix(i+0, j+k+1)
                r14 = matrix(i+0, j+k+2)
                r15 = matrix(i+0, j+k+3)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r13
                r14 = r14 + r15

                r12 = r12 + r14

                r00 = r00 + r12
            end do
            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 4
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+0, j+1)
                r11 = matrix(i+1, j+1)
                r12 = matrix(i+0, j+2)
                r13 = matrix(i+1, j+2)
                r14 = matrix(i+0, j+3)
                r15 = matrix(i+1, j+3)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r10
                r09 = r09 + r11
                r12 = r12 + r14
                r13 = r13 + r15

                r08 = r08 + r12
                r09 = r09 + r13

                r00 = r00 + r08
                r01 = r01 + r09

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r13 = matrix(i+0, j+1)
                r14 = matrix(i+0, j+2)
                r15 = matrix(i+0, j+3)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r13
                r14 = r14 + r15

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r12 = matrix(i+0, j)
                r13 = matrix(i+1, j)
                r14 = matrix(i+0, j+1)
                r15 = matrix(i+1, j+1)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r14
                r13 = r13 + r15

                r00 = r00 + r12
                r01 = r01 + r13

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r12 = matrix(i+0, j)
                r13 = matrix(i+1, j)

                r12 = r12 * r12
                r13 = r13 * r13

                r00 = r00 + r12
                r01 = r01 + r13

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_02x08_F_r8

subroutine matrix_sqsum_row_02x16_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=2; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=16; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            do k=0, c_unroll-1, c_unroll/4
                r08 = matrix(i+0, j+k)
                r09 = matrix(i+1, j+k)
                r10 = matrix(i+0, j+k+1)
                r11 = matrix(i+1, j+k+1)
                r12 = matrix(i+0, j+k+2)
                r13 = matrix(i+1, j+k+2)
                r14 = matrix(i+0, j+k+3)
                r15 = matrix(i+1, j+k+3)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r10
                r09 = r09 + r11
                r12 = r12 + r14
                r13 = r13 + r15

                r08 = r08 + r12
                r09 = r09 + r13

                r00 = r00 + r08
                r01 = r01 + r09
            end do
            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/4
                r12 = matrix(i+0, j+k)
                r13 = matrix(i+0, j+k+1)
                r14 = matrix(i+0, j+k+2)
                r15 = matrix(i+0, j+k+3)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r13
                r14 = r14 + r15

                r12 = r12 + r14

                r00 = r00 + r12
            end do
            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 8_8 ) then
        n_columns_remain = n_columns_remain - 8
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 8
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                do k=0, 7, 4
                    r08 = matrix(i+0, j+k)
                    r09 = matrix(i+1, j+k)
                    r10 = matrix(i+0, j+k+1)
                    r11 = matrix(i+1, j+k+1)
                    r12 = matrix(i+0, j+k+2)
                    r13 = matrix(i+1, j+k+2)
                    r14 = matrix(i+0, j+k+3)
                    r15 = matrix(i+1, j+k+3)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r10
                    r09 = r09 + r11
                    r12 = r12 + r14
                    r13 = r13 + r15

                    r08 = r08 + r12
                    r09 = r09 + r13

                    r00 = r00 + r08
                    r01 = r01 + r09
                end do
                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 7, 4
                    r12 = matrix(i+0, j+k)
                    r13 = matrix(i+0, j+k+1)
                    r14 = matrix(i+0, j+k+2)
                    r15 = matrix(i+0, j+k+3)

                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r12 = r12 + r13
                    r14 = r14 + r15

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 8
    end if

    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 4
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+0, j+1)
                r11 = matrix(i+1, j+1)
                r12 = matrix(i+0, j+2)
                r13 = matrix(i+1, j+2)
                r14 = matrix(i+0, j+3)
                r15 = matrix(i+1, j+3)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r10
                r09 = r09 + r11
                r12 = r12 + r14
                r13 = r13 + r15

                r08 = r08 + r12
                r09 = r09 + r13

                r00 = r00 + r08
                r01 = r01 + r09

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r13 = matrix(i+0, j+1)
                r14 = matrix(i+0, j+2)
                r15 = matrix(i+0, j+3)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r13
                r14 = r14 + r15

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r12 = matrix(i+0, j)
                r13 = matrix(i+1, j)
                r14 = matrix(i+0, j+1)
                r15 = matrix(i+1, j+1)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r14
                r13 = r13 + r15

                r00 = r00 + r12
                r01 = r01 + r13

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r12 = matrix(i+0, j)
                r13 = matrix(i+1, j)

                r12 = r12 * r12
                r13 = r13 * r13

                r00 = r00 + r12
                r01 = r01 + r13

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_02x16_F_r8

subroutine matrix_sqsum_row_02x32_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=2; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=32; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            do k=0, c_unroll-1, c_unroll/8
                r08 = matrix(i+0, j+k)
                r09 = matrix(i+1, j+k)
                r10 = matrix(i+0, j+k+1)
                r11 = matrix(i+1, j+k+1)
                r12 = matrix(i+0, j+k+2)
                r13 = matrix(i+1, j+k+2)
                r14 = matrix(i+0, j+k+3)
                r15 = matrix(i+1, j+k+3)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r10
                r09 = r09 + r11
                r12 = r12 + r14
                r13 = r13 + r15

                r08 = r08 + r12
                r09 = r09 + r13

                r00 = r00 + r08
                r01 = r01 + r09
            end do
            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/8
                r12 = matrix(i+0, j+k)
                r13 = matrix(i+0, j+k+1)
                r14 = matrix(i+0, j+k+2)
                r15 = matrix(i+0, j+k+3)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r13
                r14 = r14 + r15

                r12 = r12 + r14

                r00 = r00 + r12
            end do
            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 16_8 ) then
        n_columns_remain = n_columns_remain - 16
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 16
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                do k=0, 15, 4
                    r08 = matrix(i+0, j+k)
                    r09 = matrix(i+1, j+k)
                    r10 = matrix(i+0, j+k+1)
                    r11 = matrix(i+1, j+k+1)
                    r12 = matrix(i+0, j+k+2)
                    r13 = matrix(i+1, j+k+2)
                    r14 = matrix(i+0, j+k+3)
                    r15 = matrix(i+1, j+k+3)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r10
                    r09 = r09 + r11
                    r12 = r12 + r14
                    r13 = r13 + r15

                    r08 = r08 + r12
                    r09 = r09 + r13

                    r00 = r00 + r08
                    r01 = r01 + r09
                end do
                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 15, 4
                    r12 = matrix(i+0, j+k)
                    r13 = matrix(i+0, j+k+1)
                    r14 = matrix(i+0, j+k+2)
                    r15 = matrix(i+0, j+k+3)

                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r12 = r12 + r13
                    r14 = r14 + r15

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 16
    end if

    if ( n_columns_remain .ge. 8_8 ) then
        n_columns_remain = n_columns_remain - 8
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 8
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                do k=0, 7, 4
                    r08 = matrix(i+0, j+k)
                    r09 = matrix(i+1, j+k)
                    r10 = matrix(i+0, j+k+1)
                    r11 = matrix(i+1, j+k+1)
                    r12 = matrix(i+0, j+k+2)
                    r13 = matrix(i+1, j+k+2)
                    r14 = matrix(i+0, j+k+3)
                    r15 = matrix(i+1, j+k+3)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r10
                    r09 = r09 + r11
                    r12 = r12 + r14
                    r13 = r13 + r15

                    r08 = r08 + r12
                    r09 = r09 + r13

                    r00 = r00 + r08
                    r01 = r01 + r09
                end do
                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 7, 4
                    r12 = matrix(i+0, j+k)
                    r13 = matrix(i+0, j+k+1)
                    r14 = matrix(i+0, j+k+2)
                    r15 = matrix(i+0, j+k+3)

                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r12 = r12 + r13
                    r14 = r14 + r15

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 8
    end if

    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 4
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+0, j+1)
                r11 = matrix(i+1, j+1)
                r12 = matrix(i+0, j+2)
                r13 = matrix(i+1, j+2)
                r14 = matrix(i+0, j+3)
                r15 = matrix(i+1, j+3)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r10
                r09 = r09 + r11
                r12 = r12 + r14
                r13 = r13 + r15

                r08 = r08 + r12
                r09 = r09 + r13

                r00 = r00 + r08
                r01 = r01 + r09

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r13 = matrix(i+0, j+1)
                r14 = matrix(i+0, j+2)
                r15 = matrix(i+0, j+3)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r13
                r14 = r14 + r15

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r12 = matrix(i+0, j)
                r13 = matrix(i+1, j)
                r14 = matrix(i+0, j+1)
                r15 = matrix(i+1, j+1)

                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r12 = r12 + r14
                r13 = r13 + r15

                r00 = r00 + r12
                r01 = r01 + r13

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r12 = matrix(i+0, j)
                r13 = matrix(i+1, j)

                r12 = r12 * r12
                r13 = r13 * r13

                r00 = r00 + r12
                r01 = r01 + r13

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_02x32_F_r8




subroutine matrix_sqsum_row_04x02_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=4; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=2; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            r02 = matrix_sqsum_vals(i+2)
            r03 = matrix_sqsum_vals(i+3)

            r08 = matrix(i+0, j)
            r09 = matrix(i+1, j)
            r10 = matrix(i+2, j)
            r11 = matrix(i+3, j)
            r12 = matrix(i+0, j+1)
            r13 = matrix(i+1, j+1)
            r14 = matrix(i+2, j+1)
            r15 = matrix(i+3, j+1)

            r08 = r08 * r08
            r09 = r09 * r09
            r10 = r10 * r10
            r11 = r11 * r11
            r12 = r12 * r12
            r13 = r13 * r13
            r14 = r14 * r14
            r15 = r15 * r15

            r08 = r08 + r12
            r09 = r09 + r13
            r10 = r10 + r14
            r11 = r11 + r15

            r00 = r00 + r08
            r01 = r01 + r09
            r02 = r02 + r10
            r03 = r03 + r11

            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
            matrix_sqsum_vals(i+2) = r02
            matrix_sqsum_vals(i+3) = r03
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            r12 = matrix(i+0, j)
            r14 = matrix(i+0, j+1)

            r12 = r12 * r12
            r14 = r14 * r14

            r12 = r12 + r14

            r00 = r00 + r12

            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)

                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+2, j)
                r11 = matrix(i+3, j)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_04x02_F_r8

subroutine matrix_sqsum_row_04x04_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=4; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=4; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            r02 = matrix_sqsum_vals(i+2)
            r03 = matrix_sqsum_vals(i+3)
            do k=0, c_unroll-1, c_unroll/2
                r08 = matrix(i+0, j+k)
                r09 = matrix(i+1, j+k)
                r10 = matrix(i+2, j+k)
                r11 = matrix(i+3, j+k)
                r12 = matrix(i+0, j+k+1)
                r13 = matrix(i+1, j+k+1)
                r14 = matrix(i+2, j+k+1)
                r15 = matrix(i+3, j+k+1)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r12
                r09 = r09 + r13
                r10 = r10 + r14
                r11 = r11 + r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11
            end do
            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
            matrix_sqsum_vals(i+2) = r02
            matrix_sqsum_vals(i+3) = r03
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/2
                r12 = matrix(i+0, j+k)
                r14 = matrix(i+0, j+k+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12
            end do
            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)

                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+2, j)
                r11 = matrix(i+3, j)
                r12 = matrix(i+0, j+1)
                r13 = matrix(i+1, j+1)
                r14 = matrix(i+2, j+1)
                r15 = matrix(i+3, j+1)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r12
                r09 = r09 + r13
                r10 = r10 + r14
                r11 = r11 + r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)

                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+2, j)
                r11 = matrix(i+3, j)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_04x04_F_r8

subroutine matrix_sqsum_row_04x08_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=4; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=8; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            r02 = matrix_sqsum_vals(i+2)
            r03 = matrix_sqsum_vals(i+3)
            do k=0, c_unroll-1, c_unroll/4
                r08 = matrix(i+0, j+k)
                r09 = matrix(i+1, j+k)
                r10 = matrix(i+2, j+k)
                r11 = matrix(i+3, j+k)
                r12 = matrix(i+0, j+k+1)
                r13 = matrix(i+1, j+k+1)
                r14 = matrix(i+2, j+k+1)
                r15 = matrix(i+3, j+k+1)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r12
                r09 = r09 + r13
                r10 = r10 + r14
                r11 = r11 + r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11
            end do
            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
            matrix_sqsum_vals(i+2) = r02
            matrix_sqsum_vals(i+3) = r03
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/4
                r12 = matrix(i+0, j+k)
                r14 = matrix(i+0, j+k+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12
            end do
            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 4
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)
                do k=0, 3, 2
                    r08 = matrix(i+0, j+k)
                    r09 = matrix(i+1, j+k)
                    r10 = matrix(i+2, j+k)
                    r11 = matrix(i+3, j+k)
                    r12 = matrix(i+0, j+k+1)
                    r13 = matrix(i+1, j+k+1)
                    r14 = matrix(i+2, j+k+1)
                    r15 = matrix(i+3, j+k+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11
                end do
                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 3, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)

                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+2, j)
                r11 = matrix(i+3, j)
                r12 = matrix(i+0, j+1)
                r13 = matrix(i+1, j+1)
                r14 = matrix(i+2, j+1)
                r15 = matrix(i+3, j+1)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r12
                r09 = r09 + r13
                r10 = r10 + r14
                r11 = r11 + r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)

                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+2, j)
                r11 = matrix(i+3, j)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_04x08_F_r8

subroutine matrix_sqsum_row_04x16_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=4; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=16; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            r02 = matrix_sqsum_vals(i+2)
            r03 = matrix_sqsum_vals(i+3)
            do k=0, c_unroll-1, c_unroll/8
                r08 = matrix(i+0, j+k)
                r09 = matrix(i+1, j+k)
                r10 = matrix(i+2, j+k)
                r11 = matrix(i+3, j+k)
                r12 = matrix(i+0, j+k+1)
                r13 = matrix(i+1, j+k+1)
                r14 = matrix(i+2, j+k+1)
                r15 = matrix(i+3, j+k+1)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r12
                r09 = r09 + r13
                r10 = r10 + r14
                r11 = r11 + r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11
            end do
            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
            matrix_sqsum_vals(i+2) = r02
            matrix_sqsum_vals(i+3) = r03
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/8
                r12 = matrix(i+0, j+k)
                r14 = matrix(i+0, j+k+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12
            end do
            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 8_8 ) then
        n_columns_remain = n_columns_remain - 8
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 8
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)
                do k=0, 7, 2
                    r08 = matrix(i+0, j+k)
                    r09 = matrix(i+1, j+k)
                    r10 = matrix(i+2, j+k)
                    r11 = matrix(i+3, j+k)
                    r12 = matrix(i+0, j+k+1)
                    r13 = matrix(i+1, j+k+1)
                    r14 = matrix(i+2, j+k+1)
                    r15 = matrix(i+3, j+k+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11
                end do
                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 7, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 8
    end if

    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 4
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)
                do k=0, 3, 2
                    r08 = matrix(i+0, j+k)
                    r09 = matrix(i+1, j+k)
                    r10 = matrix(i+2, j+k)
                    r11 = matrix(i+3, j+k)
                    r12 = matrix(i+0, j+k+1)
                    r13 = matrix(i+1, j+k+1)
                    r14 = matrix(i+2, j+k+1)
                    r15 = matrix(i+3, j+k+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11
                end do
                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 3, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)

                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+2, j)
                r11 = matrix(i+3, j)
                r12 = matrix(i+0, j+1)
                r13 = matrix(i+1, j+1)
                r14 = matrix(i+2, j+1)
                r15 = matrix(i+3, j+1)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r12
                r09 = r09 + r13
                r10 = r10 + r14
                r11 = r11 + r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)

                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+2, j)
                r11 = matrix(i+3, j)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_04x16_F_r8

subroutine matrix_sqsum_row_04x32_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=4; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=32; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            r00 = matrix_sqsum_vals(i)
            r01 = matrix_sqsum_vals(i+1)
            r02 = matrix_sqsum_vals(i+2)
            r03 = matrix_sqsum_vals(i+3)
            do k=0, c_unroll-1, c_unroll/16
                r08 = matrix(i+0, j+k)
                r09 = matrix(i+1, j+k)
                r10 = matrix(i+2, j+k)
                r11 = matrix(i+3, j+k)
                r12 = matrix(i+0, j+k+1)
                r13 = matrix(i+1, j+k+1)
                r14 = matrix(i+2, j+k+1)
                r15 = matrix(i+3, j+k+1)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r12
                r09 = r09 + r13
                r10 = r10 + r14
                r11 = r11 + r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11
            end do
            matrix_sqsum_vals(i)   = r00
            matrix_sqsum_vals(i+1) = r01
            matrix_sqsum_vals(i+2) = r02
            matrix_sqsum_vals(i+3) = r03
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/16
                r12 = matrix(i+0, j+k)
                r14 = matrix(i+0, j+k+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12
            end do
            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 16_8 ) then
        n_columns_remain = n_columns_remain - 16
        do j=n_columns_unroll+shift, n_columns_unroll+shift+15, 16
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)
                do k=0, 15, 2
                    r08 = matrix(i+0, j+k)
                    r09 = matrix(i+1, j+k)
                    r10 = matrix(i+2, j+k)
                    r11 = matrix(i+3, j+k)
                    r12 = matrix(i+0, j+k+1)
                    r13 = matrix(i+1, j+k+1)
                    r14 = matrix(i+2, j+k+1)
                    r15 = matrix(i+3, j+k+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11
                end do
                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 15, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 16
    end if

    if ( n_columns_remain .ge. 8_8 ) then
        n_columns_remain = n_columns_remain - 8
        do j=n_columns_unroll+shift, n_columns_unroll+shift+7, 8
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)
                do k=0, 7, 2
                    r08 = matrix(i+0, j+k)
                    r09 = matrix(i+1, j+k)
                    r10 = matrix(i+2, j+k)
                    r11 = matrix(i+3, j+k)
                    r12 = matrix(i+0, j+k+1)
                    r13 = matrix(i+1, j+k+1)
                    r14 = matrix(i+2, j+k+1)
                    r15 = matrix(i+3, j+k+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11
                end do
                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 7, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 8
    end if

    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+3, 4
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)
                do k=0, 3, 2
                    r08 = matrix(i+0, j+k)
                    r09 = matrix(i+1, j+k)
                    r10 = matrix(i+2, j+k)
                    r11 = matrix(i+3, j+k)
                    r12 = matrix(i+0, j+k+1)
                    r13 = matrix(i+1, j+k+1)
                    r14 = matrix(i+2, j+k+1)
                    r15 = matrix(i+3, j+k+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11
                end do
                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 3, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)

                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+2, j)
                r11 = matrix(i+3, j)
                r12 = matrix(i+0, j+1)
                r13 = matrix(i+1, j+1)
                r14 = matrix(i+2, j+1)
                r15 = matrix(i+3, j+1)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r12
                r09 = r09 + r13
                r10 = r10 + r14
                r11 = r11 + r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                r00 = matrix_sqsum_vals(i)
                r01 = matrix_sqsum_vals(i+1)
                r02 = matrix_sqsum_vals(i+2)
                r03 = matrix_sqsum_vals(i+3)

                r08 = matrix(i+0, j)
                r09 = matrix(i+1, j)
                r10 = matrix(i+2, j)
                r11 = matrix(i+3, j)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11

                matrix_sqsum_vals(i)   = r00
                matrix_sqsum_vals(i+1) = r01
                matrix_sqsum_vals(i+2) = r02
                matrix_sqsum_vals(i+3) = r03
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_04x32_F_r8




subroutine matrix_sqsum_row_08x02_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=8; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=2; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            do l=0, s_unroll-1, s_unroll/2
                r00 = matrix_sqsum_vals(i+l)
                r01 = matrix_sqsum_vals(i+l+1)
                r02 = matrix_sqsum_vals(i+l+2)
                r03 = matrix_sqsum_vals(i+l+3)

                r08 = matrix(i+l+0, j)
                r09 = matrix(i+l+1, j)
                r10 = matrix(i+l+2, j)
                r11 = matrix(i+l+3, j)
                r12 = matrix(i+l+0, j+1)
                r13 = matrix(i+l+1, j+1)
                r14 = matrix(i+l+2, j+1)
                r15 = matrix(i+l+3, j+1)

                r08 = r08 * r08
                r09 = r09 * r09
                r10 = r10 * r10
                r11 = r11 * r11
                r12 = r12 * r12
                r13 = r13 * r13
                r14 = r14 * r14
                r15 = r15 * r15

                r08 = r08 + r12
                r09 = r09 + r13
                r10 = r10 + r14
                r11 = r11 + r15

                r00 = r00 + r08
                r01 = r01 + r09
                r02 = r02 + r10
                r03 = r03 + r11

                matrix_sqsum_vals(i+l)   = r00
                matrix_sqsum_vals(i+l+1) = r01
                matrix_sqsum_vals(i+l+2) = r02
                matrix_sqsum_vals(i+l+3) = r03
            end do
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            r12 = matrix(i+0, j)
            r14 = matrix(i+0, j+1)

            r12 = r12 * r12
            r14 = r14 * r14

            r12 = r12 + r14

            r00 = r00 + r12

            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    r08 = matrix(i+l+0, j)
                    r09 = matrix(i+l+1, j)
                    r10 = matrix(i+l+2, j)
                    r11 = matrix(i+l+3, j)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11

                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_08x02_F_r8

subroutine matrix_sqsum_row_08x04_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=8; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=4; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            do l=0, s_unroll-1, s_unroll/2
                r00 = matrix_sqsum_vals(i+l)
                r01 = matrix_sqsum_vals(i+l+1)
                r02 = matrix_sqsum_vals(i+l+2)
                r03 = matrix_sqsum_vals(i+l+3)

                do k=0, c_unroll-1, c_unroll/2
                    r08 = matrix(i+l+0, j+k)
                    r09 = matrix(i+l+1, j+k)
                    r10 = matrix(i+l+2, j+k)
                    r11 = matrix(i+l+3, j+k)
                    r12 = matrix(i+l+0, j+k+1)
                    r13 = matrix(i+l+1, j+k+1)
                    r14 = matrix(i+l+2, j+k+1)
                    r15 = matrix(i+l+3, j+k+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11
                end do
                matrix_sqsum_vals(i+l)   = r00
                matrix_sqsum_vals(i+l+1) = r01
                matrix_sqsum_vals(i+l+2) = r02
                matrix_sqsum_vals(i+l+3) = r03
            end do
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/2
                r12 = matrix(i+0, j+k)
                r14 = matrix(i+0, j+k+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12
            end do
            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    r08 = matrix(i+l+0, j)
                    r09 = matrix(i+l+1, j)
                    r10 = matrix(i+l+2, j)
                    r11 = matrix(i+l+3, j)
                    r12 = matrix(i+l+0, j+1)
                    r13 = matrix(i+l+1, j+1)
                    r14 = matrix(i+l+2, j+1)
                    r15 = matrix(i+l+3, j+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11

                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    r08 = matrix(i+l+0, j)
                    r09 = matrix(i+l+1, j)
                    r10 = matrix(i+l+2, j)
                    r11 = matrix(i+l+3, j)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11

                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_08x04_F_r8

subroutine matrix_sqsum_row_08x08_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=8; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=8; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            do l=0, s_unroll-1, s_unroll/2
                r00 = matrix_sqsum_vals(i+l)
                r01 = matrix_sqsum_vals(i+l+1)
                r02 = matrix_sqsum_vals(i+l+2)
                r03 = matrix_sqsum_vals(i+l+3)

                do k=0, c_unroll-1, c_unroll/4
                    r08 = matrix(i+l+0, j+k)
                    r09 = matrix(i+l+1, j+k)
                    r10 = matrix(i+l+2, j+k)
                    r11 = matrix(i+l+3, j+k)
                    r12 = matrix(i+l+0, j+k+1)
                    r13 = matrix(i+l+1, j+k+1)
                    r14 = matrix(i+l+2, j+k+1)
                    r15 = matrix(i+l+3, j+k+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11
                end do
                matrix_sqsum_vals(i+l)   = r00
                matrix_sqsum_vals(i+l+1) = r01
                matrix_sqsum_vals(i+l+2) = r02
                matrix_sqsum_vals(i+l+3) = r03
            end do
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/4
                r12 = matrix(i+0, j+k)
                r14 = matrix(i+0, j+k+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12
            end do
            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 4
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    do k=0, 3, 2
                        r08 = matrix(i+l+0, j+k)
                        r09 = matrix(i+l+1, j+k)
                        r10 = matrix(i+l+2, j+k)
                        r11 = matrix(i+l+3, j+k)
                        r12 = matrix(i+l+0, j+k+1)
                        r13 = matrix(i+l+1, j+k+1)
                        r14 = matrix(i+l+2, j+k+1)
                        r15 = matrix(i+l+3, j+k+1)

                        r08 = r08 * r08
                        r09 = r09 * r09
                        r10 = r10 * r10
                        r11 = r11 * r11
                        r12 = r12 * r12
                        r13 = r13 * r13
                        r14 = r14 * r14
                        r15 = r15 * r15

                        r08 = r08 + r12
                        r09 = r09 + r13
                        r10 = r10 + r14
                        r11 = r11 + r15

                        r00 = r00 + r08
                        r01 = r01 + r09
                        r02 = r02 + r10
                        r03 = r03 + r11
                    end do
                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 3, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    r08 = matrix(i+l+0, j)
                    r09 = matrix(i+l+1, j)
                    r10 = matrix(i+l+2, j)
                    r11 = matrix(i+l+3, j)
                    r12 = matrix(i+l+0, j+1)
                    r13 = matrix(i+l+1, j+1)
                    r14 = matrix(i+l+2, j+1)
                    r15 = matrix(i+l+3, j+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11

                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    r08 = matrix(i+l+0, j)
                    r09 = matrix(i+l+1, j)
                    r10 = matrix(i+l+2, j)
                    r11 = matrix(i+l+3, j)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11

                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_08x08_F_r8

subroutine matrix_sqsum_row_08x16_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=8; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=16; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            do l=0, s_unroll-1, s_unroll/2
                r00 = matrix_sqsum_vals(i+l)
                r01 = matrix_sqsum_vals(i+l+1)
                r02 = matrix_sqsum_vals(i+l+2)
                r03 = matrix_sqsum_vals(i+l+3)

                do k=0, c_unroll-1, c_unroll/8
                    r08 = matrix(i+l+0, j+k)
                    r09 = matrix(i+l+1, j+k)
                    r10 = matrix(i+l+2, j+k)
                    r11 = matrix(i+l+3, j+k)
                    r12 = matrix(i+l+0, j+k+1)
                    r13 = matrix(i+l+1, j+k+1)
                    r14 = matrix(i+l+2, j+k+1)
                    r15 = matrix(i+l+3, j+k+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11
                end do
                matrix_sqsum_vals(i+l)   = r00
                matrix_sqsum_vals(i+l+1) = r01
                matrix_sqsum_vals(i+l+2) = r02
                matrix_sqsum_vals(i+l+3) = r03
            end do
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)
            do k=0, c_unroll-1, c_unroll/8
                r12 = matrix(i+0, j+k)
                r14 = matrix(i+0, j+k+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12
            end do
            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 8_8 ) then
        n_columns_remain = n_columns_remain - 8
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 8
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    do k=0, 7, 2
                        r08 = matrix(i+l+0, j+k)
                        r09 = matrix(i+l+1, j+k)
                        r10 = matrix(i+l+2, j+k)
                        r11 = matrix(i+l+3, j+k)
                        r12 = matrix(i+l+0, j+k+1)
                        r13 = matrix(i+l+1, j+k+1)
                        r14 = matrix(i+l+2, j+k+1)
                        r15 = matrix(i+l+3, j+k+1)

                        r08 = r08 * r08
                        r09 = r09 * r09
                        r10 = r10 * r10
                        r11 = r11 * r11
                        r12 = r12 * r12
                        r13 = r13 * r13
                        r14 = r14 * r14
                        r15 = r15 * r15

                        r08 = r08 + r12
                        r09 = r09 + r13
                        r10 = r10 + r14
                        r11 = r11 + r15

                        r00 = r00 + r08
                        r01 = r01 + r09
                        r02 = r02 + r10
                        r03 = r03 + r11
                    end do
                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 7, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 8
    end if

    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 4
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    do k=0, 3, 2
                        r08 = matrix(i+l+0, j+k)
                        r09 = matrix(i+l+1, j+k)
                        r10 = matrix(i+l+2, j+k)
                        r11 = matrix(i+l+3, j+k)
                        r12 = matrix(i+l+0, j+k+1)
                        r13 = matrix(i+l+1, j+k+1)
                        r14 = matrix(i+l+2, j+k+1)
                        r15 = matrix(i+l+3, j+k+1)

                        r08 = r08 * r08
                        r09 = r09 * r09
                        r10 = r10 * r10
                        r11 = r11 * r11
                        r12 = r12 * r12
                        r13 = r13 * r13
                        r14 = r14 * r14
                        r15 = r15 * r15

                        r08 = r08 + r12
                        r09 = r09 + r13
                        r10 = r10 + r14
                        r11 = r11 + r15

                        r00 = r00 + r08
                        r01 = r01 + r09
                        r02 = r02 + r10
                        r03 = r03 + r11
                    end do
                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 3, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    r08 = matrix(i+l+0, j)
                    r09 = matrix(i+l+1, j)
                    r10 = matrix(i+l+2, j)
                    r11 = matrix(i+l+3, j)
                    r12 = matrix(i+l+0, j+1)
                    r13 = matrix(i+l+1, j+1)
                    r14 = matrix(i+l+2, j+1)
                    r15 = matrix(i+l+3, j+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11

                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    r08 = matrix(i+l+0, j)
                    r09 = matrix(i+l+1, j)
                    r10 = matrix(i+l+2, j)
                    r11 = matrix(i+l+3, j)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11

                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_08x16_F_r8

subroutine matrix_sqsum_row_08x32_F_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
    integer(kind=8), intent(in) :: n_samples, n_columns

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: s_unroll, n_samples_unroll
    integer(kind=8) :: c_unroll, n_columns_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    s_unroll=8; n_samples_unroll=n_samples - mod(n_samples, s_unroll)
    c_unroll=32; n_columns_unroll=n_columns - mod(n_columns, c_unroll)

    matrix_sqsum_vals(:) = 0d0

    do j=1, n_columns_unroll, c_unroll
        do i=1, n_samples_unroll, s_unroll
            do l=0, s_unroll-1, s_unroll/2
                r00 = matrix_sqsum_vals(i+l)
                r01 = matrix_sqsum_vals(i+l+1)
                r02 = matrix_sqsum_vals(i+l+2)
                r03 = matrix_sqsum_vals(i+l+3)

                do k=0, c_unroll-1, c_unroll/16
                    r08 = matrix(i+l+0, j+k)
                    r09 = matrix(i+l+1, j+k)
                    r10 = matrix(i+l+2, j+k)
                    r11 = matrix(i+l+3, j+k)
                    r12 = matrix(i+l+0, j+k+1)
                    r13 = matrix(i+l+1, j+k+1)
                    r14 = matrix(i+l+2, j+k+1)
                    r15 = matrix(i+l+3, j+k+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11
                end do

                matrix_sqsum_vals(i+l)   = r00
                matrix_sqsum_vals(i+l+1) = r01
                matrix_sqsum_vals(i+l+2) = r02
                matrix_sqsum_vals(i+l+3) = r03
            end do
        end do

        do i=n_samples_unroll+1, n_samples, 1
            r00 = matrix_sqsum_vals(i)

            do k=0, c_unroll-1, c_unroll/16
                r12 = matrix(i+0, j+k)
                r14 = matrix(i+0, j+k+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12
            end do

            matrix_sqsum_vals(i)   = r00
        end do
    end do


    shift = 1
    n_columns_remain = mod(n_columns, c_unroll)
    if ( n_columns_remain .ge. 16_8 ) then
        n_columns_remain = n_columns_remain - 16
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 16
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    do k=0, 15, 2
                        r08 = matrix(i+l+0, j+k)
                        r09 = matrix(i+l+1, j+k)
                        r10 = matrix(i+l+2, j+k)
                        r11 = matrix(i+l+3, j+k)
                        r12 = matrix(i+l+0, j+k+1)
                        r13 = matrix(i+l+1, j+k+1)
                        r14 = matrix(i+l+2, j+k+1)
                        r15 = matrix(i+l+3, j+k+1)

                        r08 = r08 * r08
                        r09 = r09 * r09
                        r10 = r10 * r10
                        r11 = r11 * r11
                        r12 = r12 * r12
                        r13 = r13 * r13
                        r14 = r14 * r14
                        r15 = r15 * r15

                        r08 = r08 + r12
                        r09 = r09 + r13
                        r10 = r10 + r14
                        r11 = r11 + r15

                        r00 = r00 + r08
                        r01 = r01 + r09
                        r02 = r02 + r10
                        r03 = r03 + r11
                    end do
                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 15, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 16
    end if

    if ( n_columns_remain .ge. 8_8 ) then
        n_columns_remain = n_columns_remain - 8
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 8
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    do k=0, 7, 2
                        r08 = matrix(i+l+0, j+k)
                        r09 = matrix(i+l+1, j+k)
                        r10 = matrix(i+l+2, j+k)
                        r11 = matrix(i+l+3, j+k)
                        r12 = matrix(i+l+0, j+k+1)
                        r13 = matrix(i+l+1, j+k+1)
                        r14 = matrix(i+l+2, j+k+1)
                        r15 = matrix(i+l+3, j+k+1)

                        r08 = r08 * r08
                        r09 = r09 * r09
                        r10 = r10 * r10
                        r11 = r11 * r11
                        r12 = r12 * r12
                        r13 = r13 * r13
                        r14 = r14 * r14
                        r15 = r15 * r15

                        r08 = r08 + r12
                        r09 = r09 + r13
                        r10 = r10 + r14
                        r11 = r11 + r15

                        r00 = r00 + r08
                        r01 = r01 + r09
                        r02 = r02 + r10
                        r03 = r03 + r11
                    end do
                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 7, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 8
    end if

    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 4
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    do k=0, 3, 2
                        r08 = matrix(i+l+0, j+k)
                        r09 = matrix(i+l+1, j+k)
                        r10 = matrix(i+l+2, j+k)
                        r11 = matrix(i+l+3, j+k)
                        r12 = matrix(i+l+0, j+k+1)
                        r13 = matrix(i+l+1, j+k+1)
                        r14 = matrix(i+l+2, j+k+1)
                        r15 = matrix(i+l+3, j+k+1)

                        r08 = r08 * r08
                        r09 = r09 * r09
                        r10 = r10 * r10
                        r11 = r11 * r11
                        r12 = r12 * r12
                        r13 = r13 * r13
                        r14 = r14 * r14
                        r15 = r15 * r15

                        r08 = r08 + r12
                        r09 = r09 + r13
                        r10 = r10 + r14
                        r11 = r11 + r15

                        r00 = r00 + r08
                        r01 = r01 + r09
                        r02 = r02 + r10
                        r03 = r03 + r11
                    end do
                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                do k=0, 3, 2
                    r12 = matrix(i+0, j+k)
                    r14 = matrix(i+0, j+k+1)

                    r12 = r12 * r12
                    r14 = r14 * r14

                    r12 = r12 + r14

                    r00 = r00 + r12
                end do
                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_columns_unroll+shift, n_columns_unroll+shift+1, 2
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    r08 = matrix(i+l+0, j)
                    r09 = matrix(i+l+1, j)
                    r10 = matrix(i+l+2, j)
                    r11 = matrix(i+l+3, j)
                    r12 = matrix(i+l+0, j+1)
                    r13 = matrix(i+l+1, j+1)
                    r14 = matrix(i+l+2, j+1)
                    r15 = matrix(i+l+3, j+1)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11
                    r12 = r12 * r12
                    r13 = r13 * r13
                    r14 = r14 * r14
                    r15 = r15 * r15

                    r08 = r08 + r12
                    r09 = r09 + r13
                    r10 = r10 + r14
                    r11 = r11 + r15

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11

                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)
                r14 = matrix(i+0, j+1)

                r12 = r12 * r12
                r14 = r14 * r14

                r12 = r12 + r14

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_columns_unroll+shift, n_columns, 1
            do i=1, n_samples_unroll, s_unroll
                do l=0, s_unroll-1, s_unroll/2
                    r00 = matrix_sqsum_vals(i+l)
                    r01 = matrix_sqsum_vals(i+l+1)
                    r02 = matrix_sqsum_vals(i+l+2)
                    r03 = matrix_sqsum_vals(i+l+3)

                    r08 = matrix(i+l+0, j)
                    r09 = matrix(i+l+1, j)
                    r10 = matrix(i+l+2, j)
                    r11 = matrix(i+l+3, j)

                    r08 = r08 * r08
                    r09 = r09 * r09
                    r10 = r10 * r10
                    r11 = r11 * r11

                    r00 = r00 + r08
                    r01 = r01 + r09
                    r02 = r02 + r10
                    r03 = r03 + r11

                    matrix_sqsum_vals(i+l)   = r00
                    matrix_sqsum_vals(i+l+1) = r01
                    matrix_sqsum_vals(i+l+2) = r02
                    matrix_sqsum_vals(i+l+3) = r03
                end do
            end do

            do i=n_samples_unroll+1, n_samples, 1
                r00 = matrix_sqsum_vals(i)
                r12 = matrix(i+0, j)

                r12 = r12 * r12

                r00 = r00 + r12

                matrix_sqsum_vals(i)   = r00
            end do
        end do
        shift = shift + 1
    end if

end subroutine matrix_sqsum_row_08x32_F_r8
