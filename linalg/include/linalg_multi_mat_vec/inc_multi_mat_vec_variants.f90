! subroutine multi_mat_vec_32x04_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
!     implicit none
!     real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
!     real(kind=8), intent(in)    :: input_vector(n_cols)
!     real(kind=8), intent(inout) :: output_vector(n_rows)
!     integer(kind=8), intent(in) :: n_rows, n_cols

!     integer(kind=8) :: i, j, k
!     integer(kind=8) :: r_unroll, n_rows_unroll
!     integer(kind=8) :: c_unroll, n_cols_unroll

!     real(kind=8), allocatable :: buffer_r(:), buffer_c(:), buffer_o(:)

!     real(kind=8)    :: r00, r01, r02, r03
!     real(kind=8)    :: r04, r05, r06, r07
!     real(kind=8)    :: r08, r09, r10, r11
!     real(kind=8)    :: r12, r13, r14, r15

!     output_vector = 0

!     r_unroll = 2
!     n_rows_unroll = n_rows - mod(n_rows, r_unroll)
!     c_unroll = 8
!     n_cols_unroll = n_cols - mod(n_cols, c_unroll)

!     allocate(buffer_r(r_unroll))
!     allocate(buffer_c(c_unroll))
!     allocate(buffer_o(r_unroll))

!     do j=1, n_cols_unroll, c_unroll
!         buffer_c(1:c_unroll)=input_vector(j:j+c_unroll-1)
!         do i=1, n_rows_unroll, r_unroll
!             buffer_o(1:r_unroll) = output_vector(i:i+r_unroll-1)
            
!             do k=0, c_unroll-1, 1
!                 buffer_r(1:r_unroll) = matrix(i:i+r_unroll-1,j+k) * buffer_c(k+1)
!                 buffer_o(1:r_unroll) = buffer_o(1:r_unroll) + buffer_r(1:r_unroll)
!             end do
!             output_vector(i:i+r_unroll-1) = buffer_o(1:r_unroll)
!         end do
!     end do
! end subroutine multi_mat_vec_32x04_N_F_r8

subroutine multi_mat_vec_01x01_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    do j=1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows, 1
            r15 = output_vector(i)
            
            r01 = matrix(i,j)
            
            r01 = r01 * r00

            r15 = r15 + r01

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_01x01_N_F_r8

subroutine multi_mat_vec_01x02_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    c_unroll = 2
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r00 = input_vector(j)
        r01 = input_vector(j+1)
        do i=1, n_rows, 1
            r15 = output_vector(i)
            
            r02 = matrix(i,j)
            r03 = matrix(i,j+1)
            
            r02 = r02 * r00
            r03 = r03 * r01

            r15 = r15 + r02
            r15 = r15 + r03

            output_vector(i) = r15
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows, 1
            r15 = output_vector(i)
            
            r02 = matrix(i,j)
            
            r02 = r02 * r00

            r15 = r15 + r02

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_01x02_N_F_r8

subroutine multi_mat_vec_01x04_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    c_unroll = 4
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r00 = input_vector(j)
        r01 = input_vector(j+1)
        r02 = input_vector(j+2)
        r03 = input_vector(j+3)
        do i=1, n_rows, 1
            r15 = output_vector(i)
            
            r04 = matrix(i,j)
            r05 = matrix(i,j+1)
            r06 = matrix(i,j+2)
            r07 = matrix(i,j+3)
            
            r04 = r04 * r00
            r05 = r05 * r01
            r06 = r06 * r02
            r07 = r07 * r03

            r04 = r04 + r05
            r06 = r06 + r07

            r04 = r04 + r06
            r15 = r15 + r04

            output_vector(i) = r15
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows, 1
            r15 = output_vector(i)
            
            r02 = matrix(i,j)
            
            r02 = r02 * r00

            r15 = r15 + r02

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_01x04_N_F_r8

subroutine multi_mat_vec_02x01_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j
    integer(kind=8) :: r_unroll, n_rows_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0
    r_unroll = 2
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)

    do j=1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            r14 = output_vector(i)
            r15 = output_vector(i+1)
            
            r01 = matrix(i,  j)
            r02 = matrix(i+1,j)
            
            r01 = r01 * r00
            r02 = r02 * r00

            r14 = r14 + r01
            r15 = r15 + r02

            output_vector(i  ) = r14
            output_vector(i+1) = r15
        end do

        do i=n_rows_unroll+1, n_rows
            r15 = output_vector(i)
            
            r01 = matrix(i,j)
            
            r01 = r01 * r00

            r15 = r15 + r01

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_02x01_N_F_r8

subroutine multi_mat_vec_02x02_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    r_unroll = 2
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 2
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r00 = input_vector(j)
        r01 = input_vector(j+1)
        do i=1, n_rows_unroll, r_unroll
            r14 = output_vector(i)
            r15 = output_vector(i+1)
            
            r02 = matrix(i,  j)
            r03 = matrix(i,  j+1)
            r04 = matrix(i+1,j)
            r05 = matrix(i+1,j+1)
            
            r02 = r02 * r00
            r03 = r03 * r01
            r04 = r04 * r00
            r05 = r05 * r01

            r02 = r02 + r03
            r04 = r04 + r05

            r14 = r14 + r02
            r15 = r15 + r04

            output_vector(i  ) = r14
            output_vector(i+1) = r15
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r15 = output_vector(i)
            
            r02 = matrix(i,j)
            r03 = matrix(i,j+1)
            
            r02 = r02 * r00
            r03 = r03 * r01

            r02 = r02 + r03

            r15 = r15 + r02

            output_vector(i) = r15
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            r14 = output_vector(i)
            r15 = output_vector(i+1)
            
            r02 = matrix(i,  j)
            r04 = matrix(i+1,j)
            
            r02 = r02 * r00
            r04 = r04 * r00

            r14 = r14 + r02
            r15 = r15 + r04

            output_vector(i  ) = r14
            output_vector(i+1) = r15
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r15 = output_vector(i)
            
            r01 = matrix(i,j)
            
            r01 = r01 * r00

            r15 = r15 + r01

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_02x02_N_F_r8

subroutine multi_mat_vec_02x04_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    r_unroll = 2
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 4
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r00 = input_vector(j)
        r01 = input_vector(j+1)
        r02 = input_vector(j+2)
        r03 = input_vector(j+3)
        do i=1, n_rows_unroll, r_unroll
            r14 = output_vector(i)
            r15 = output_vector(i+1)
            
            r04 = matrix(i,  j)
            r05 = matrix(i,  j+1)
            r06 = matrix(i,  j+2)
            r07 = matrix(i,  j+3)
            r08 = matrix(i+1,j)
            r09 = matrix(i+1,j+1)
            r10 = matrix(i+1,j+2)
            r11 = matrix(i+1,j+3)
            
            r04 = r04 * r00
            r05 = r05 * r01
            r06 = r06 * r02
            r07 = r07 * r03
            r08 = r08 * r00
            r09 = r09 * r01
            r10 = r10 * r02
            r11 = r11 * r03

            r04 = r04 + r05
            r06 = r06 + r07
            r08 = r08 + r09
            r10 = r10 + r11

            r04 = r04 + r06
            r08 = r08 + r10

            r14 = r14 + r04
            r15 = r15 + r08

            output_vector(i  ) = r14
            output_vector(i+1) = r15
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r15 = output_vector(i)
            
            r04 = matrix(i,  j)
            r05 = matrix(i,  j+1)
            r06 = matrix(i,  j+2)
            r07 = matrix(i,  j+3)
            
            r04 = r04 * r00
            r05 = r05 * r01
            r06 = r06 * r02
            r07 = r07 * r03

            r04 = r04 + r05
            r06 = r06 + r07

            r04 = r04 + r06

            r15 = r15 + r04

            output_vector(i) = r15
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            r14 = output_vector(i)
            r15 = output_vector(i+1)
            
            r01 = matrix(i,  j)
            r02 = matrix(i+1,j)

            r01 = r01 * r00
            r02 = r02 * r00

            r14 = r14 + r01
            r15 = r15 + r02

            output_vector(i  ) = r14
            output_vector(i+1) = r15
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r15 = output_vector(i)
            
            r01 = matrix(i,j)
            
            r01 = r01 * r00

            r15 = r15 + r01

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_02x04_N_F_r8

subroutine multi_mat_vec_04x01_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j
    integer(kind=8) :: r_unroll, n_rows_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0
    r_unroll = 4
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)

    do j=1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            r01 = matrix(i,  j)
            r02 = matrix(i+1,j)
            r03 = matrix(i+2,j)
            r04 = matrix(i+3,j)
            
            r01 = r01 * r00
            r02 = r02 * r00
            r03 = r03 * r00
            r04 = r04 * r00

            r12 = r12 + r01
            r13 = r13 + r02
            r14 = r14 + r03
            r15 = r15 + r04

            output_vector(i  ) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15
        end do

        do i=n_rows_unroll+1, n_rows
            r15 = output_vector(i)
            
            r01 = matrix(i,j)
            
            r01 = r01 * r00

            r15 = r15 + r01

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_04x01_N_F_r8

subroutine multi_mat_vec_04x02_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    r_unroll = 4
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 2
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r00 = input_vector(j)
        r01 = input_vector(j+1)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i+0)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            r02 = matrix(i+0,j+0)
            r03 = matrix(i+1,j+0)
            r04 = matrix(i+2,j+0)
            r05 = matrix(i+3,j+0)
            r06 = matrix(i+0,j+1)
            r07 = matrix(i+1,j+1)
            r08 = matrix(i+2,j+1)
            r09 = matrix(i+3,j+1)
            
            r02 = r02 * r00
            r03 = r03 * r00
            r04 = r04 * r00
            r05 = r05 * r00
            r06 = r06 * r01
            r07 = r07 * r01
            r08 = r08 * r01
            r09 = r09 * r01

            r12 = r12 + r02
            r13 = r13 + r03
            r14 = r14 + r04
            r15 = r15 + r05
            r12 = r12 + r06
            r13 = r13 + r07
            r14 = r14 + r08
            r15 = r15 + r09

            output_vector(i+0) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r12 = output_vector(i+0)
            
            r02 = matrix(i+0,j+0)
            r06 = matrix(i+0,j+1)
            
            r02 = r02 * r00
            r06 = r06 * r01

            r12 = r12 + r02
            r12 = r12 + r06

            output_vector(i+0) = r12
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i+0)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            r02 = matrix(i+0,j+0)
            r03 = matrix(i+1,j+0)
            r04 = matrix(i+2,j+0)
            r05 = matrix(i+3,j+0)
            
            r02 = r02 * r00
            r03 = r03 * r00
            r04 = r04 * r00
            r05 = r05 * r00

            r12 = r12 + r02
            r13 = r13 + r03
            r14 = r14 + r04
            r15 = r15 + r05

            output_vector(i+0) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r12 = output_vector(i+0)
            
            r02 = matrix(i+0,j+0)
            
            r02 = r02 * r00

            r12 = r12 + r02

            output_vector(i+0) = r12
        end do
    end do
end subroutine multi_mat_vec_04x02_N_F_r8

subroutine multi_mat_vec_04x04_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    r_unroll = 4
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 4
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r00 = input_vector(j)
        r01 = input_vector(j+1)
        r02 = input_vector(j+2)
        r03 = input_vector(j+3)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i+0)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            r04 = matrix(i+0,j+0)
            r05 = matrix(i+1,j+0)
            r06 = matrix(i+2,j+0)
            r07 = matrix(i+3,j+0)
            r08 = matrix(i+0,j+1)
            r09 = matrix(i+1,j+1)
            r10 = matrix(i+2,j+1)
            r11 = matrix(i+3,j+1)
            
            r04 = r04 * r00
            r05 = r05 * r00
            r06 = r06 * r00
            r07 = r07 * r00
            r08 = r08 * r01
            r09 = r09 * r01
            r10 = r10 * r01
            r11 = r11 * r01

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07
            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            r04 = matrix(i+0,j+2)
            r05 = matrix(i+1,j+2)
            r06 = matrix(i+2,j+2)
            r07 = matrix(i+3,j+2)
            r08 = matrix(i+0,j+3)
            r09 = matrix(i+1,j+3)
            r10 = matrix(i+2,j+3)
            r11 = matrix(i+3,j+3)
            
            r04 = r04 * r02
            r05 = r05 * r02
            r06 = r06 * r02
            r07 = r07 * r02
            r08 = r08 * r03
            r09 = r09 * r03
            r10 = r10 * r03
            r11 = r11 * r03

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07
            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            output_vector(i+0) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r12 = output_vector(i)
            
            r04 = matrix(i,j)
            r05 = matrix(i,j+1)
            r06 = matrix(i,j+2)
            r07 = matrix(i,j+3)
            
            r04 = r04 * r00
            r05 = r05 * r01
            r06 = r06 * r02
            r07 = r07 * r03

            r04 = r04 + r05
            r06 = r06 + r07

            r04 = r04 + r06

            r12 = r12 + r04

            output_vector(i) = r12
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i+0)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            r04 = matrix(i+0,j+0)
            r05 = matrix(i+1,j+0)
            r06 = matrix(i+2,j+0)
            r07 = matrix(i+3,j+0)
            
            r04 = r04 * r00
            r05 = r05 * r00
            r06 = r06 * r00
            r07 = r07 * r00

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07

            output_vector(i+0) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r12 = output_vector(i+0)
            
            r04 = matrix(i+0,j+0)
            
            r04 = r04 * r00

            r12 = r12 + r04

            output_vector(i+0) = r12
        end do
    end do
end subroutine multi_mat_vec_04x04_N_F_r8

subroutine multi_mat_vec_04x08_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    r_unroll = 4
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 8
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r00 = input_vector(j)
        r01 = input_vector(j+1)
        r02 = input_vector(j+2)
        r03 = input_vector(j+3)
        r04 = input_vector(j+4)
        r05 = input_vector(j+5)
        r06 = input_vector(j+6)
        r07 = input_vector(j+7)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i+0)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            ! ---------------------------------------
            r08 = matrix(i+0,j+0)
            r09 = matrix(i+1,j+0)
            r10 = matrix(i+2,j+0)
            r11 = matrix(i+3,j+0)
            
            r08 = r08 * r00
            r09 = r09 * r00
            r10 = r10 * r00
            r11 = r11 * r00

            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            ! ---------------------------------------
            r08 = matrix(i+0,j+1)
            r09 = matrix(i+1,j+1)
            r10 = matrix(i+2,j+1)
            r11 = matrix(i+3,j+1)
            
            r08 = r08 * r01
            r09 = r09 * r01
            r10 = r10 * r01
            r11 = r11 * r01

            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            ! ---------------------------------------
            r08 = matrix(i+0,j+2)
            r09 = matrix(i+1,j+2)
            r10 = matrix(i+2,j+2)
            r11 = matrix(i+3,j+2)
            
            r08 = r08 * r02
            r09 = r09 * r02
            r10 = r10 * r02
            r11 = r11 * r02

            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            ! ---------------------------------------
            r08 = matrix(i+0,j+3)
            r09 = matrix(i+1,j+3)
            r10 = matrix(i+2,j+3)
            r11 = matrix(i+3,j+3)
            
            r08 = r08 * r03
            r09 = r09 * r03
            r10 = r10 * r03
            r11 = r11 * r03

            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11


            ! ---------------------------------------
            r08 = matrix(i+0,j+4)
            r09 = matrix(i+1,j+4)
            r10 = matrix(i+2,j+4)
            r11 = matrix(i+3,j+4)
            
            r08 = r08 * r04
            r09 = r09 * r04
            r10 = r10 * r04
            r11 = r11 * r04

            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            ! ---------------------------------------
            r08 = matrix(i+0,j+5)
            r09 = matrix(i+1,j+5)
            r10 = matrix(i+2,j+5)
            r11 = matrix(i+3,j+5)
            
            r08 = r08 * r05
            r09 = r09 * r05
            r10 = r10 * r05
            r11 = r11 * r05

            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            ! ---------------------------------------
            r08 = matrix(i+0,j+6)
            r09 = matrix(i+1,j+6)
            r10 = matrix(i+2,j+6)
            r11 = matrix(i+3,j+6)
            
            r08 = r08 * r06
            r09 = r09 * r06
            r10 = r10 * r06
            r11 = r11 * r06

            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            ! ---------------------------------------
            r08 = matrix(i+0,j+7)
            r09 = matrix(i+1,j+7)
            r10 = matrix(i+2,j+7)
            r11 = matrix(i+3,j+7)
            
            r08 = r08 * r07
            r09 = r09 * r07
            r10 = r10 * r07
            r11 = r11 * r07

            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            output_vector(i+0) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15
        end do
    end do
end subroutine multi_mat_vec_04x08_N_F_r8

subroutine multi_mat_vec_08x01_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j
    integer(kind=8) :: r_unroll, n_rows_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0
    r_unroll = 8
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)

    do j=1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            r01 = matrix(i,  j)
            r02 = matrix(i+1,j)
            r03 = matrix(i+2,j)
            r04 = matrix(i+3,j)
            
            r01 = r01 * r00
            r02 = r02 * r00
            r03 = r03 * r00
            r04 = r04 * r00

            r12 = r12 + r01
            r13 = r13 + r02
            r14 = r14 + r03
            r15 = r15 + r04

            output_vector(i  ) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15

            r12 = output_vector(i+4)
            r13 = output_vector(i+5)
            r14 = output_vector(i+6)
            r15 = output_vector(i+7)
            
            r01 = matrix(i+4,j)
            r02 = matrix(i+5,j)
            r03 = matrix(i+6,j)
            r04 = matrix(i+7,j)
            
            r01 = r01 * r00
            r02 = r02 * r00
            r03 = r03 * r00
            r04 = r04 * r00

            r12 = r12 + r01
            r13 = r13 + r02
            r14 = r14 + r03
            r15 = r15 + r04

            output_vector(i+4) = r12
            output_vector(i+5) = r13
            output_vector(i+6) = r14
            output_vector(i+7) = r15
        end do

        do i=n_rows_unroll+1, n_rows
            r15 = output_vector(i)
            
            r01 = matrix(i,j)
            
            r01 = r01 * r00

            r15 = r15 + r01

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_08x01_N_F_r8

subroutine multi_mat_vec_08x02_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    r_unroll = 8
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 2
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r00 = input_vector(j)
        r01 = input_vector(j+1)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            r02 = matrix(i,  j)
            r03 = matrix(i+1,j)
            r04 = matrix(i+2,j)
            r05 = matrix(i+3,j)
            r06 = matrix(i,  j+1)
            r07 = matrix(i+1,j+1)
            r08 = matrix(i+2,j+1)
            r09 = matrix(i+3,j+1)

            r02 = r02 * r00
            r03 = r03 * r00
            r04 = r04 * r00
            r05 = r05 * r00
            r06 = r06 * r01
            r07 = r07 * r01
            r08 = r08 * r01
            r09 = r09 * r01

            r12 = r12 + r02
            r13 = r13 + r03
            r14 = r14 + r04
            r15 = r15 + r05
            r12 = r12 + r06
            r13 = r13 + r07
            r14 = r14 + r08
            r15 = r15 + r09

            output_vector(i  ) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15

            r12 = output_vector(i+4)
            r13 = output_vector(i+5)
            r14 = output_vector(i+6)
            r15 = output_vector(i+7)
            
            r02 = matrix(i+4,j)
            r03 = matrix(i+5,j)
            r04 = matrix(i+6,j)
            r05 = matrix(i+7,j)
            r06 = matrix(i+4,j+1)
            r07 = matrix(i+5,j+1)
            r08 = matrix(i+6,j+1)
            r09 = matrix(i+7,j+1)

            r02 = r02 * r00
            r03 = r03 * r00
            r04 = r04 * r00
            r05 = r05 * r00
            r06 = r06 * r01
            r07 = r07 * r01
            r08 = r08 * r01
            r09 = r09 * r01

            r12 = r12 + r02
            r13 = r13 + r03
            r14 = r14 + r04
            r15 = r15 + r05
            r12 = r12 + r06
            r13 = r13 + r07
            r14 = r14 + r08
            r15 = r15 + r09

            output_vector(i+4) = r12
            output_vector(i+5) = r13
            output_vector(i+6) = r14
            output_vector(i+7) = r15
        end do

        do i=n_rows_unroll+1, n_rows
            r15 = output_vector(i)
            
            r02 = matrix(i,  j)
            r06 = matrix(i,  j+1)
            
            r02 = r02 * r00
            r06 = r06 * r01

            r02 = r02 + r06

            r15 = r15 + r02

            output_vector(i) = r15
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            r01 = matrix(i,  j)
            r02 = matrix(i+1,j)
            r03 = matrix(i+2,j)
            r04 = matrix(i+3,j)
            
            r01 = r01 * r00
            r02 = r02 * r00
            r03 = r03 * r00
            r04 = r04 * r00

            r12 = r12 + r01
            r13 = r13 + r02
            r14 = r14 + r03
            r15 = r15 + r04

            output_vector(i  ) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15

            r12 = output_vector(i+4)
            r13 = output_vector(i+5)
            r14 = output_vector(i+6)
            r15 = output_vector(i+7)
            
            r01 = matrix(i+4,j)
            r02 = matrix(i+5,j)
            r03 = matrix(i+6,j)
            r04 = matrix(i+7,j)
            
            r01 = r01 * r00
            r02 = r02 * r00
            r03 = r03 * r00
            r04 = r04 * r00

            r12 = r12 + r01
            r13 = r13 + r02
            r14 = r14 + r03
            r15 = r15 + r04

            output_vector(i+4) = r12
            output_vector(i+5) = r13
            output_vector(i+6) = r14
            output_vector(i+7) = r15
        end do

        do i=n_rows_unroll+1, n_rows
            r15 = output_vector(i)
            
            r01 = matrix(i,j)
            
            r01 = r01 * r00

            r15 = r15 + r01

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_08x02_N_F_r8

subroutine multi_mat_vec_08x04_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    r_unroll = 8
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 4
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r00 = input_vector(j)
        r01 = input_vector(j+1)
        r02 = input_vector(j+2)
        r03 = input_vector(j+3)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i+0)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            r04 = matrix(i+0,j+0)
            r05 = matrix(i+1,j+0)
            r06 = matrix(i+2,j+0)
            r07 = matrix(i+3,j+0)
            r08 = matrix(i+0,j+1)
            r09 = matrix(i+1,j+1)
            r10 = matrix(i+2,j+1)
            r11 = matrix(i+3,j+1)
            
            r04 = r04 * r00
            r05 = r05 * r00
            r06 = r06 * r00
            r07 = r07 * r00
            r08 = r08 * r01
            r09 = r09 * r01
            r10 = r10 * r01
            r11 = r11 * r01

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07
            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            r04 = matrix(i+0,j+2)
            r05 = matrix(i+1,j+2)
            r06 = matrix(i+2,j+2)
            r07 = matrix(i+3,j+2)
            r08 = matrix(i+0,j+3)
            r09 = matrix(i+1,j+3)
            r10 = matrix(i+2,j+3)
            r11 = matrix(i+3,j+3)
            
            r04 = r04 * r02
            r05 = r05 * r02
            r06 = r06 * r02
            r07 = r07 * r02
            r08 = r08 * r03
            r09 = r09 * r03
            r10 = r10 * r03
            r11 = r11 * r03

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07
            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            output_vector(i+0) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15

            r12 = output_vector(i+4)
            r13 = output_vector(i+5)
            r14 = output_vector(i+6)
            r15 = output_vector(i+7)
            
            r04 = matrix(i+4,j+0)
            r05 = matrix(i+5,j+0)
            r06 = matrix(i+6,j+0)
            r07 = matrix(i+7,j+0)
            r08 = matrix(i+4,j+1)
            r09 = matrix(i+5,j+1)
            r10 = matrix(i+6,j+1)
            r11 = matrix(i+7,j+1)
            
            r04 = r04 * r00
            r05 = r05 * r00
            r06 = r06 * r00
            r07 = r07 * r00
            r08 = r08 * r01
            r09 = r09 * r01
            r10 = r10 * r01
            r11 = r11 * r01

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07
            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            r04 = matrix(i+4,j+2)
            r05 = matrix(i+5,j+2)
            r06 = matrix(i+6,j+2)
            r07 = matrix(i+7,j+2)
            r08 = matrix(i+4,j+3)
            r09 = matrix(i+5,j+3)
            r10 = matrix(i+6,j+3)
            r11 = matrix(i+7,j+3)
            
            r04 = r04 * r02
            r05 = r05 * r02
            r06 = r06 * r02
            r07 = r07 * r02
            r08 = r08 * r03
            r09 = r09 * r03
            r10 = r10 * r03
            r11 = r11 * r03

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07
            r12 = r12 + r08
            r13 = r13 + r09
            r14 = r14 + r10
            r15 = r15 + r11

            output_vector(i+4) = r12
            output_vector(i+5) = r13
            output_vector(i+6) = r14
            output_vector(i+7) = r15
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r12 = output_vector(i)
            
            r04 = matrix(i,j)
            r05 = matrix(i,j+1)
            r06 = matrix(i,j+2)
            r07 = matrix(i,j+3)
            
            r04 = r04 * r00
            r05 = r05 * r01
            r06 = r06 * r02
            r07 = r07 * r03

            r04 = r04 + r05
            r06 = r06 + r07

            r04 = r04 + r06

            r12 = r12 + r04

            output_vector(i) = r12
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            r12 = output_vector(i+0)
            r13 = output_vector(i+1)
            r14 = output_vector(i+2)
            r15 = output_vector(i+3)
            
            r04 = matrix(i+0,j+0)
            r05 = matrix(i+1,j+0)
            r06 = matrix(i+2,j+0)
            r07 = matrix(i+3,j+0)
            
            r04 = r04 * r00
            r05 = r05 * r00
            r06 = r06 * r00
            r07 = r07 * r00

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07

            output_vector(i+0) = r12
            output_vector(i+1) = r13
            output_vector(i+2) = r14
            output_vector(i+3) = r15

            r12 = output_vector(i+4)
            r13 = output_vector(i+5)
            r14 = output_vector(i+6)
            r15 = output_vector(i+7)
            
            r04 = matrix(i+4,j+0)
            r05 = matrix(i+5,j+0)
            r06 = matrix(i+6,j+0)
            r07 = matrix(i+7,j+0)
            
            r04 = r04 * r00
            r05 = r05 * r00
            r06 = r06 * r00
            r07 = r07 * r00

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07

            output_vector(i+4) = r12
            output_vector(i+5) = r13
            output_vector(i+6) = r14
            output_vector(i+7) = r15
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r12 = output_vector(i+0)
            
            r04 = matrix(i+0,j+0)
            
            r04 = r04 * r00

            r12 = r12 + r04

            output_vector(i+0) = r12
        end do
    end do
end subroutine multi_mat_vec_08x04_N_F_r8

subroutine multi_mat_vec_01x01_T_F_r8(matrix_t, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix_t(n_cols, n_rows)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    do i=1, n_rows, 1
        r15 = output_vector(i)
        do j=1, n_cols, 1
            r00 = input_vector(j)
            
            r01 = matrix_t(j,i)
            
            r01 = r01 * r00

            r15 = r15 + r01
        end do
        output_vector(i) = r15
    end do
end subroutine multi_mat_vec_01x01_T_F_r8

subroutine multi_mat_vec_04x04_T_F_r8(matrix_t, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix_t(n_cols, n_rows)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    r_unroll = 4
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 4
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do i=1, n_rows_unroll, r_unroll
        r12 = output_vector(i)
        r13 = output_vector(i+1)
        r14 = output_vector(i+2)
        r15 = output_vector(i+3)
        do j=1, n_cols_unroll, c_unroll
            r00 = input_vector(j)
            r01 = input_vector(j+1)
            r02 = input_vector(j+2)
            r03 = input_vector(j+3)
            
            r04 = matrix_t(j+0,i+0)
            r05 = matrix_t(j+1,i+0)
            r06 = matrix_t(j+2,i+0)
            r07 = matrix_t(j+3,i+0)
            r08 = matrix_t(j+0,i+1)
            r09 = matrix_t(j+1,i+1)
            r10 = matrix_t(j+2,i+1)
            r11 = matrix_t(j+3,i+1)

            r04 = r04 * r00
            r05 = r05 * r01
            r06 = r06 * r02
            r07 = r07 * r03
            r08 = r08 * r00
            r09 = r09 * r01
            r10 = r10 * r02
            r11 = r11 * r03

            r04 = r04 + r08
            r05 = r05 + r09
            r06 = r06 + r10
            r07 = r07 + r11

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07

            r04 = matrix_t(j+0,i+2)
            r05 = matrix_t(j+1,i+2)
            r06 = matrix_t(j+2,i+2)
            r07 = matrix_t(j+3,i+2)
            r08 = matrix_t(j+0,i+3)
            r09 = matrix_t(j+1,i+3)
            r10 = matrix_t(j+2,i+3)
            r11 = matrix_t(j+3,i+3)

            r04 = r04 * r00
            r05 = r05 * r01
            r06 = r06 * r02
            r07 = r07 * r03
            r08 = r08 * r00
            r09 = r09 * r01
            r10 = r10 * r02
            r11 = r11 * r03

            r04 = r04 + r08
            r05 = r05 + r09
            r06 = r06 + r10
            r07 = r07 + r11

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07
        end do

        do j=n_cols_unroll+1, n_cols, 1
            r00 = input_vector(j)
            
            r04 = matrix_t(j+0,i+0)
            r05 = matrix_t(j+0,i+1)
            r06 = matrix_t(j+0,i+2)
            r07 = matrix_t(j+0,i+3)

            r04 = r04 * r00
            r05 = r05 * r00
            r06 = r06 * r00
            r07 = r07 * r00

            r12 = r12 + r04
            r13 = r13 + r05
            r14 = r14 + r06
            r15 = r15 + r07
        end do
        output_vector(i)   = r12
        output_vector(i+1) = r13
        output_vector(i+2) = r14
        output_vector(i+3) = r15
    end do

    do i=n_rows_unroll+1, n_rows, 1
        r12 = output_vector(i)
        do j=1, n_cols_unroll, c_unroll
            r00 = input_vector(j)
            r01 = input_vector(j+1)
            r02 = input_vector(j+2)
            r03 = input_vector(j+3)
            
            r04 = matrix_t(j+0,i+0)
            r05 = matrix_t(j+1,i+0)
            r06 = matrix_t(j+2,i+0)
            r07 = matrix_t(j+3,i+0)

            r04 = r04 * r00
            r05 = r05 * r01
            r06 = r06 * r02
            r07 = r07 * r03

            r04 = r04 + r05
            r06 = r06 + r07

            r04 = r04 + r06

            r12 = r12 + r04
        end do

        do j=n_cols_unroll+1, n_cols, 1
            r00 = input_vector(j)
            
            r04 = matrix_t(j+0,i+0)

            r04 = r04 * r00

            r12 = r12 + r04
        end do
        output_vector(i)   = r12
    end do
end subroutine multi_mat_vec_04x04_T_F_r8

