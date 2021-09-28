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

subroutine multi_mat_vec_08x01_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, l
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
            do l=0, 7, 4
                r12 = output_vector(i+l)
                r13 = output_vector(i+l+1)
                r14 = output_vector(i+l+2)
                r15 = output_vector(i+l+3)
                
                r01 = matrix(i+l,  j)
                r02 = matrix(i+l+1,j)
                r03 = matrix(i+l+2,j)
                r04 = matrix(i+l+3,j)
                
                r01 = r01 * r00
                r02 = r02 * r00
                r03 = r03 * r00
                r04 = r04 * r00

                r12 = r12 + r01
                r13 = r13 + r02
                r14 = r14 + r03
                r15 = r15 + r04

                output_vector(i+l  ) = r12
                output_vector(i+l+1) = r13
                output_vector(i+l+2) = r14
                output_vector(i+l+3) = r15
            end do
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

subroutine multi_mat_vec_16x01_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, l
    integer(kind=8) :: r_unroll, n_rows_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0
    r_unroll = 16
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)

    do j=1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            do l=0, 15, 4
                r12 = output_vector(i+l)
                r13 = output_vector(i+l+1)
                r14 = output_vector(i+l+2)
                r15 = output_vector(i+l+3)
                
                r01 = matrix(i+l,  j)
                r02 = matrix(i+l+1,j)
                r03 = matrix(i+l+2,j)
                r04 = matrix(i+l+3,j)
                
                r01 = r01 * r00
                r02 = r02 * r00
                r03 = r03 * r00
                r04 = r04 * r00

                r12 = r12 + r01
                r13 = r13 + r02
                r14 = r14 + r03
                r15 = r15 + r04

                output_vector(i+l  ) = r12
                output_vector(i+l+1) = r13
                output_vector(i+l+2) = r14
                output_vector(i+l+3) = r15
            end do
        end do

        do i=n_rows_unroll+1, n_rows
            r15 = output_vector(i)
            
            r01 = matrix(i,j)
            
            r01 = r01 * r00

            r15 = r15 + r01

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_16x01_N_F_r8

subroutine multi_mat_vec_32x01_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, l
    integer(kind=8) :: r_unroll, n_rows_unroll

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0
    r_unroll = 32
    n_rows_unroll = n_rows - mod(n_rows, r_unroll)

    do j=1, n_cols, 1
        r00 = input_vector(j)
        do i=1, n_rows_unroll, r_unroll
            do l=0, 31, 4
                r12 = output_vector(i+l)
                r13 = output_vector(i+l+1)
                r14 = output_vector(i+l+2)
                r15 = output_vector(i+l+3)
                
                r01 = matrix(i+l,  j)
                r02 = matrix(i+l+1,j)
                r03 = matrix(i+l+2,j)
                r04 = matrix(i+l+3,j)
                
                r01 = r01 * r00
                r02 = r02 * r00
                r03 = r03 * r00
                r04 = r04 * r00

                r12 = r12 + r01
                r13 = r13 + r02
                r14 = r14 + r03
                r15 = r15 + r04

                output_vector(i+l  ) = r12
                output_vector(i+l+1) = r13
                output_vector(i+l+2) = r14
                output_vector(i+l+3) = r15
            end do
        end do

        do i=n_rows_unroll+1, n_rows
            r15 = output_vector(i)
            
            r01 = matrix(i,j)
            
            r01 = r01 * r00

            r15 = r15 + r01

            output_vector(i) = r15
        end do
    end do
end subroutine multi_mat_vec_32x01_N_F_r8







subroutine multi_mat_vec_01x02_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

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

    shift = 1
    n_columns_remain = mod(n_cols, c_unroll)
    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_cols_unroll+shift, n_cols, 1
            r00 = input_vector(j)
            do i=1, n_rows, 1
                r15 = output_vector(i)
                
                r02 = matrix(i,j)
                
                r02 = r02 * r00

                r15 = r15 + r02

                output_vector(i) = r15
            end do
        end do
        shift = shift + 1
    end if

end subroutine multi_mat_vec_01x02_N_F_r8

subroutine multi_mat_vec_01x04_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

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

    shift = 1
    n_columns_remain = mod(n_cols, c_unroll)
    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_cols_unroll+shift, n_cols_unroll+shift+1, 2
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
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_cols_unroll+shift, n_cols, 1
            r00 = input_vector(j)
            do i=1, n_rows, 1
                r15 = output_vector(i)
                
                r02 = matrix(i,j)
                
                r02 = r02 * r00

                r15 = r15 + r02

                output_vector(i) = r15
            end do
        end do
        shift = shift + 1
    end if
end subroutine multi_mat_vec_01x04_N_F_r8

subroutine multi_mat_vec_01x08_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    c_unroll = 8
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        do k=0, 7, 4
            r00 = input_vector(j+k)
            r01 = input_vector(j+k+1)
            r02 = input_vector(j+k+2)
            r03 = input_vector(j+k+3)
            do i=1, n_rows, 1
                r15 = output_vector(i)
                
                r04 = matrix(i,j+k)
                r05 = matrix(i,j+k+1)
                r06 = matrix(i,j+k+2)
                r07 = matrix(i,j+k+3)
                
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
    end do

    shift = 1
    n_columns_remain = mod(n_cols, c_unroll)
    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_cols_unroll+shift, n_cols_unroll+shift+3, 4
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
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_cols_unroll+shift, n_cols_unroll+shift+1, 2
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
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_cols_unroll+shift, n_cols, 1
            r00 = input_vector(j)
            do i=1, n_rows, 1
                r15 = output_vector(i)
                
                r02 = matrix(i,j)
                
                r02 = r02 * r00

                r15 = r15 + r02

                output_vector(i) = r15
            end do
        end do
        shift = shift + 1
    end if
end subroutine multi_mat_vec_01x08_N_F_r8

subroutine multi_mat_vec_01x16_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    c_unroll = 16
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        do k=0, 15, 4
            r00 = input_vector(j+k)
            r01 = input_vector(j+k+1)
            r02 = input_vector(j+k+2)
            r03 = input_vector(j+k+3)
            do i=1, n_rows, 1
                r15 = output_vector(i)
                
                r04 = matrix(i,j+k)
                r05 = matrix(i,j+k+1)
                r06 = matrix(i,j+k+2)
                r07 = matrix(i,j+k+3)
                
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
    end do

    shift = 1
    n_columns_remain = mod(n_cols, c_unroll)
    if ( n_columns_remain .ge. 8_8 ) then
        n_columns_remain = n_columns_remain - 8
        do j=n_cols_unroll+shift, n_cols_unroll+shift+3, 8
            do k=0, 7, 4
                r00 = input_vector(j+k)
                r01 = input_vector(j+k+1)
                r02 = input_vector(j+k+2)
                r03 = input_vector(j+k+3)
                do i=1, n_rows, 1
                    r15 = output_vector(i)
                    
                    r04 = matrix(i,j+k)
                    r05 = matrix(i,j+k+1)
                    r06 = matrix(i,j+k+2)
                    r07 = matrix(i,j+k+3)
                    
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
        end do
        shift = shift + 8
    end if

    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_cols_unroll+shift, n_cols_unroll+shift+3, 4
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
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_cols_unroll+shift, n_cols_unroll+shift+1, 2
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
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_cols_unroll+shift, n_cols, 1
            r00 = input_vector(j)
            do i=1, n_rows, 1
                r15 = output_vector(i)
                
                r02 = matrix(i,j)
                
                r02 = r02 * r00

                r15 = r15 + r02

                output_vector(i) = r15
            end do
        end do
        shift = shift + 1
    end if
end subroutine multi_mat_vec_01x16_N_F_r8

subroutine multi_mat_vec_01x32_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    output_vector = 0

    c_unroll = 32
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        do k=0, 31, 4
            r00 = input_vector(j+k)
            r01 = input_vector(j+k+1)
            r02 = input_vector(j+k+2)
            r03 = input_vector(j+k+3)
            do i=1, n_rows, 1
                r15 = output_vector(i)
                
                r04 = matrix(i,j+k)
                r05 = matrix(i,j+k+1)
                r06 = matrix(i,j+k+2)
                r07 = matrix(i,j+k+3)
                
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
    end do

    shift = 1
    n_columns_remain = mod(n_cols, c_unroll)
    if ( n_columns_remain .ge. 16_8 ) then
        n_columns_remain = n_columns_remain - 16
        do j=n_cols_unroll+shift, n_cols_unroll+shift+15, 16
            do k=0, 15, 4
                r00 = input_vector(j+k)
                r01 = input_vector(j+k+1)
                r02 = input_vector(j+k+2)
                r03 = input_vector(j+k+3)
                do i=1, n_rows, 1
                    r15 = output_vector(i)
                    
                    r04 = matrix(i,j+k)
                    r05 = matrix(i,j+k+1)
                    r06 = matrix(i,j+k+2)
                    r07 = matrix(i,j+k+3)
                    
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
        end do
        shift = shift + 16
    end if

    if ( n_columns_remain .ge. 8_8 ) then
        n_columns_remain = n_columns_remain - 8
        do j=n_cols_unroll+shift, n_cols_unroll+shift+7, 8
            do k=0, 7, 4
                r00 = input_vector(j+k)
                r01 = input_vector(j+k+1)
                r02 = input_vector(j+k+2)
                r03 = input_vector(j+k+3)
                do i=1, n_rows, 1
                    r15 = output_vector(i)
                    
                    r04 = matrix(i,j+k)
                    r05 = matrix(i,j+k+1)
                    r06 = matrix(i,j+k+2)
                    r07 = matrix(i,j+k+3)
                    
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
        end do
        shift = shift + 8
    end if

    if ( n_columns_remain .ge. 4_8 ) then
        n_columns_remain = n_columns_remain - 4
        do j=n_cols_unroll+shift, n_cols_unroll+shift+3, 4
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
        shift = shift + 4
    end if

    if ( n_columns_remain .ge. 2_8 ) then
        n_columns_remain = n_columns_remain - 2
        do j=n_cols_unroll+shift, n_cols_unroll+shift+1, 2
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
        shift = shift + 2
    end if

    if ( n_columns_remain .ge. 1_8 ) then
        n_columns_remain = n_columns_remain - 1
        do j=n_cols_unroll+shift, n_cols, 1
            r00 = input_vector(j)
            do i=1, n_rows, 1
                r15 = output_vector(i)
                
                r02 = matrix(i,j)
                
                r02 = r02 * r00

                r15 = r15 + r02

                output_vector(i) = r15
            end do
        end do
        shift = shift + 1
    end if
end subroutine multi_mat_vec_01x32_N_F_r8


subroutine multi_mat_vec_01x07_N_F_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer(7)

    output_vector = 0

    c_unroll = 7
    n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        do k=0, 7-1, 1
            buffer(k+1) = input_vector(j+k)
        end do

        do i=1, n_rows, 1
            r15 = 0d0
            do k=0, 7-1, 1
                r15 = r15 + matrix(i,j+k)*buffer(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
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
end subroutine multi_mat_vec_01x07_N_F_r8








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




! subroutine multi_mat_vec_02x01_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
!     implicit none
!     real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
!     real(kind=8), intent(in)    :: input_vector(n_cols)
!     real(kind=8), intent(inout) :: output_vector(n_rows)
!     integer(kind=8), intent(in) :: n_rows, n_cols

!     integer(kind=8) :: i, j, k, l, shift
!     integer(kind=8) :: r_unroll, n_rows_unroll
!     integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

!     real(kind=8)    :: r00, r01, r02, r03
!     real(kind=8)    :: r04, r05, r06, r07
!     real(kind=8)    :: r08, r09, r10, r11
!     real(kind=8)    :: r12, r13, r14, r15

!     real(kind=8)    :: buffer_O(cache_len_R)
!     real(kind=8)    :: buffer_R(cache_len_R, cache_len_C)
!     real(kind=8)    :: buffer_C(cache_len_C)

!     output_vector = 0

!     r_unroll = cache_len_R; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
!     c_unroll = cache_len_C; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

!     do j=1, n_cols_unroll, c_unroll
!         do k=0, cache_len_C-1, 1
!             buffer_C(k+1) = input_vector(j+k)
!         end do

!         do i=1, n_rows_unroll, r_unroll
!             buffer_O(:) = 0d0
!             do l=0, cache_len_R-1, 1
!                 do k=0, cache_len_C-1, 1
!                     buffer_O(k+1) = buffer_O(k+1) + matrix(i+l,j+k)*buffer_C(k+1)
!                 end do
!             end do

!             do l=0, cache_len_R-1, 1
!                 output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
!             end do
!         end do

!         do i=n_rows_unroll+1, n_rows, 1
!             r15 = 0d0
!             do k=0, cache_len_C-1, 1
!                 r15 = r15 + matrix(i+l,j+k)*buffer_C(k+1)
!             end do
!             output_vector(i) = output_vector(i) + r15
!         end do
!     end do

! end subroutine multi_mat_vec_02x01_N_F_B_r8

subroutine multi_mat_vec_02x01_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 1)
    real(kind=8)    :: buffer_C(1)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 1; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r15 = input_vector(j)

        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j) * r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do

end subroutine multi_mat_vec_02x01_N_F_B_r8

subroutine multi_mat_vec_02x02_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 2)
    real(kind=8)    :: buffer_C(2)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 2; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 2-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                do k=0, 2-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 2-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_02x02_N_F_B_r8

subroutine multi_mat_vec_02x04_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 4)
    real(kind=8)    :: buffer_C(4)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 4; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 4-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                do k=0, 4-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 4-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_02x04_N_F_B_r8

subroutine multi_mat_vec_02x08_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 8)
    real(kind=8)    :: buffer_C(8)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 8; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 8-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                do k=0, 8-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 8-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_02x08_N_F_B_r8

subroutine multi_mat_vec_02x16_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 16)
    real(kind=8)    :: buffer_C(16)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 16; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 16-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                do k=0, 16-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 16-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_02x16_N_F_B_r8

subroutine multi_mat_vec_02x32_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 32)
    real(kind=8)    :: buffer_C(32)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 32; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 32-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                do k=0, 32-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 32-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_02x32_N_F_B_r8






subroutine multi_mat_vec_04x01_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 1)
    real(kind=8)    :: buffer_C(1)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 1; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r15 = input_vector(j)

        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j) * r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do

end subroutine multi_mat_vec_04x01_N_F_B_r8

subroutine multi_mat_vec_04x02_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 2)
    real(kind=8)    :: buffer_C(2)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 2; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 2-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                do k=0, 2-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 2-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_04x02_N_F_B_r8

subroutine multi_mat_vec_04x04_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 4)
    real(kind=8)    :: buffer_C(4)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 4; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 4-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                do k=0, 4-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 4-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_04x04_N_F_B_r8

subroutine multi_mat_vec_04x08_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 8)
    real(kind=8)    :: buffer_C(8)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 8; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 8-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                do k=0, 8-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 8-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_04x08_N_F_B_r8

subroutine multi_mat_vec_04x16_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 16)
    real(kind=8)    :: buffer_C(16)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 16; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 16-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                do k=0, 16-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 16-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_04x16_N_F_B_r8

subroutine multi_mat_vec_04x32_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 32)
    real(kind=8)    :: buffer_C(32)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 32; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 32-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                do k=0, 32-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 32-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_04x32_N_F_B_r8





subroutine multi_mat_vec_08x01_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 1)
    real(kind=8)    :: buffer_C(1)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 1; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    do j=1, n_cols_unroll, c_unroll
        r15 = input_vector(j)

        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j) * r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do

end subroutine multi_mat_vec_08x01_N_F_B_r8

subroutine multi_mat_vec_08x02_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 2)
    real(kind=8)    :: buffer_C(2)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 2; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 2-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                do k=0, 2-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 2-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_08x02_N_F_B_r8

subroutine multi_mat_vec_08x04_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 4)
    real(kind=8)    :: buffer_C(4)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 4; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 4-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                do k=0, 4-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 4-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_08x04_N_F_B_r8

subroutine multi_mat_vec_08x08_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 8)
    real(kind=8)    :: buffer_C(8)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 8; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 8-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                do k=0, 8-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 8-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_08x08_N_F_B_r8

subroutine multi_mat_vec_08x16_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 16)
    real(kind=8)    :: buffer_C(16)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 16; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 16-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                do k=0, 16-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 16-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_08x16_N_F_B_r8

subroutine multi_mat_vec_08x32_N_F_B_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 32)
    real(kind=8)    :: buffer_C(32)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 32; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    do j=1, n_cols_unroll, c_unroll
        do k=0, 32-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                do k=0, 32-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 32-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_08x32_N_F_B_r8









subroutine multi_mat_vec_02x01_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 1)
    real(kind=8)    :: buffer_C(1)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 1; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        r15 = input_vector(j)

        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j) * r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
    !$omp end do
    !$omp end parallel

end subroutine multi_mat_vec_02x01_N_F_B_P_r8

subroutine multi_mat_vec_02x02_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 2)
    real(kind=8)    :: buffer_C(2)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 2; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 2-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                do k=0, 2-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 2-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_02x02_N_F_B_P_r8

subroutine multi_mat_vec_02x04_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 4)
    real(kind=8)    :: buffer_C(4)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 4; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 4-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                do k=0, 4-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 4-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_02x04_N_F_B_P_r8

subroutine multi_mat_vec_02x08_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 8)
    real(kind=8)    :: buffer_C(8)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 8; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 8-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                do k=0, 8-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 8-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_02x08_N_F_B_P_r8

subroutine multi_mat_vec_02x16_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 16)
    real(kind=8)    :: buffer_C(16)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 16; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 16-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                do k=0, 16-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 16-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_02x16_N_F_B_P_r8

subroutine multi_mat_vec_02x32_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(2)
    real(kind=8)    :: buffer_R(2, 32)
    real(kind=8)    :: buffer_C(32)

    output_vector = 0

    r_unroll = 2; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 32; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 32-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                do k=0, 32-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 32-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 2-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 2-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_02x32_N_F_B_P_r8






subroutine multi_mat_vec_04x01_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 1)
    real(kind=8)    :: buffer_C(1)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 1; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        r15 = input_vector(j)

        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j) * r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
    !$omp end do
    !$omp end parallel

end subroutine multi_mat_vec_04x01_N_F_B_P_r8

subroutine multi_mat_vec_04x02_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 2)
    real(kind=8)    :: buffer_C(2)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 2; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 2-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                do k=0, 2-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 2-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_04x02_N_F_B_P_r8

subroutine multi_mat_vec_04x04_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 4)
    real(kind=8)    :: buffer_C(4)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 4; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 4-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                do k=0, 4-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 4-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_04x04_N_F_B_P_r8

subroutine multi_mat_vec_04x08_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 8)
    real(kind=8)    :: buffer_C(8)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 8; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 8-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                do k=0, 8-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 8-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_04x08_N_F_B_P_r8

subroutine multi_mat_vec_04x16_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 16)
    real(kind=8)    :: buffer_C(16)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 16; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 16-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                do k=0, 16-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 16-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_04x16_N_F_B_P_r8

subroutine multi_mat_vec_04x32_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(4)
    real(kind=8)    :: buffer_R(4, 32)
    real(kind=8)    :: buffer_C(32)

    output_vector = 0

    r_unroll = 4; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 32; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 32-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                do k=0, 32-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 32-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 4-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 4-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_04x32_N_F_B_P_r8





subroutine multi_mat_vec_08x01_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 1)
    real(kind=8)    :: buffer_C(1)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 1; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        r15 = input_vector(j)

        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j) * r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
    !$omp end do
    !$omp end parallel

end subroutine multi_mat_vec_08x01_N_F_B_P_r8

subroutine multi_mat_vec_08x02_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 2)
    real(kind=8)    :: buffer_C(2)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 2; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 2-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                do k=0, 2-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 2-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_08x02_N_F_B_P_r8

subroutine multi_mat_vec_08x04_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 4)
    real(kind=8)    :: buffer_C(4)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 4; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 4-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                do k=0, 4-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 4-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_08x04_N_F_B_P_r8

subroutine multi_mat_vec_08x08_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 8)
    real(kind=8)    :: buffer_C(8)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 8; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 8-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                do k=0, 8-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 8-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_08x08_N_F_B_P_r8

subroutine multi_mat_vec_08x16_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 16)
    real(kind=8)    :: buffer_C(16)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 16; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 16-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                do k=0, 16-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 16-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_08x16_N_F_B_P_r8

subroutine multi_mat_vec_08x32_N_F_B_P_r8(matrix, input_vector, output_vector, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)    :: input_vector(n_cols)
    real(kind=8), intent(inout) :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols

    integer(kind=8) :: i, j, k, l, shift
    integer(kind=8) :: r_unroll, n_rows_unroll
    integer(kind=8) :: c_unroll, n_cols_unroll, n_columns_remain

    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15

    real(kind=8)    :: buffer_O(8)
    real(kind=8)    :: buffer_R(8, 32)
    real(kind=8)    :: buffer_C(32)

    output_vector = 0

    r_unroll = 8; n_rows_unroll = n_rows - mod(n_rows, r_unroll)
    c_unroll = 32; n_cols_unroll = n_cols - mod(n_cols, c_unroll)

    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Main Part -----------------------------------------------------
    !$omp parallel num_threads(4)
    !$omp do reduction(+:output_vector) private(buffer_O, buffer_C, i, j, k, l, r14, r15)
    do j=1, n_cols_unroll, c_unroll
        do k=0, 32-1, 1
            buffer_C(k+1) = input_vector(j+k)
        end do

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                do k=0, 32-1, 1
                    buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j+k)*buffer_C(k+1)
                end do
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r15 = 0d0
            do k=0, 32-1, 1
                r15 = r15 + matrix(i,j+k)*buffer_C(k+1)
            end do
            output_vector(i) = output_vector(i) + r15
        end do
    end do
    !$omp end do
    !$omp end parallel


    ! ----------------------------------------------------------------------
    ! ----------------------------------------------------------------------
    ! Column Fraction Part -----------------------------------------------------
    do j=n_cols_unroll+1, n_cols, 1
        r15 = input_vector(j)

        ! Sample Main Part -----------------------------------------------------
        do i=1, n_rows_unroll, r_unroll
            buffer_O(:) = 0d0
            do l=0, 8-1, 1
                buffer_O(l+1) = buffer_O(l+1) + matrix(i+l,j)*r15
            end do

            do l=0, 8-1, 1
                output_vector(i+l) = output_vector(i+l) + buffer_O(l+1)
            end do
        end do

        ! Sample Fraction Part -----------------------------------------------------
        do i=n_rows_unroll+1, n_rows, 1
            r14 = matrix(i,j)*r15
            output_vector(i) = output_vector(i) + r14
        end do
    end do
end subroutine multi_mat_vec_08x32_N_F_B_P_r8
