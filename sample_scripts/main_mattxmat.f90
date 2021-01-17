program main_mattxmat
    use mod_const
    use mod_timer
    use mod_stats
    use mod_linalg, only: mattxmat
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    integer(kind=8)           :: n_samples, n_columns
    real(kind=8), allocatable :: matrix(:,:), matrix_t(:,:)
    real(kind=8), allocatable :: mat_sq(:,:), mat_sq_ref(:,:)
    integer(kind=8)           :: i
    logical(kind=4)           :: print_matrix

    n_samples = 1000000
    n_columns = 10
    print_matrix = .false.

    print*, '============================================================='
    print*, "Matrix size: ", n_samples, n_columns
    allocate(matrix(n_samples, n_columns))
    allocate(matrix_t(n_columns, n_samples))
    allocate(mat_sq(n_columns, n_columns))
    allocate(mat_sq_ref(n_columns, n_columns))

    print*, '============================================================='
    print*, "Randomization"
    call random_number(matrix)
    matrix_t = transpose(matrix)

    print*, '============================================================='
    print*, "matmul(transpose(matrix), matrix)"
    call date_and_time(values=date_value1)
    mat_sq_ref = matmul(transpose(matrix), matrix)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    if (print_matrix) then
        do i=1, size(mat_sq_ref, dim=2), 1
            print*, real(mat_sq_ref(i,1:n_columns), kind=4)
        end do
    end if

    print*, '============================================================='
    print*, "matmul(matrix_t, matrix)"
    call date_and_time(values=date_value1)
    mat_sq = matmul(matrix_t, matrix)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    if (print_matrix) then
        do i=1, size(mat_sq_ref, dim=2), 1
            print*, real(mat_sq(i,1:n_columns), kind=4)
        end do
    end if    

    print*, '============================================================='
    print*, "mattxmat(matrix, matrix)"
    call date_and_time(values=date_value1)
    call mattxmat(mat_sq, matrix, n_samples, n_columns, with_intercept=f_)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    if (print_matrix) then
        do i=1, size(mat_sq_ref, dim=2), 1
            print*, real(mat_sq(i,1:n_columns), kind=4)
        end do
    end if


contains

    subroutine mattxmat_new_naive_r8(mat_out, mat_in, n_samples, n_columns, with_intercept)
        implicit none
        real(kind=8), ALLOCATABLE, intent(inout) :: mat_out(:,:)
        real(kind=8), intent(in)               :: mat_in(n_samples, n_columns)
        integer(kind=8), intent(in)            :: n_samples, n_columns
        logical(kind=4), intent(in)            :: with_intercept
        real(kind=8), allocatable              :: col_sums(:)
        real(kind=8)                           :: tmp_sum

        integer(kind=8) :: i, j, k

        if ( allocated(mat_out) ) deallocate(mat_out)

        if ( with_intercept ) then
            allocate(mat_out(n_columns+1, n_columns+1))
            call sum_of_matrix(col_sums, matrix, n_samples, n_columns, 1_8)

            do j=1, n_columns, 1
                do i=1, n_columns, 1
                    tmp_sum = 0d0
                    do k=1, n_samples, 1
                        tmp_sum = tmp_sum + mat_in(k,i) * mat_in(k,j)
                    end do
                    mat_out(i,j) = tmp_sum
                    mat_out(j,i) = tmp_sum
                end do
            end do

            do j=1, n_columns, 1
                mat_out(j,n_columns+1) = col_sums(j)
                mat_out(n_columns+1,j) = col_sums(j)
            end do
            mat_out(n_columns+1, n_columns+1) = n_samples

        else
            allocate(mat_out(n_columns, n_columns))
            mat_out(:, :) = matmul(transpose(mat_in), mat_in)
        end if
    end subroutine mattxmat_new_naive_r8

    subroutine mattxmat_new_n_samples_optimize_r8(mat_out, mat_in, n_samples, n_columns, with_intercept)
        implicit none
        real(kind=8), ALLOCATABLE, intent(inout) :: mat_out(:,:)
        real(kind=8), intent(in)               :: mat_in(n_samples, n_columns)
        integer(kind=8), intent(in)            :: n_samples, n_columns
        logical(kind=4), intent(in)            :: with_intercept
        real(kind=8), allocatable              :: col_sums(:)
        real(kind=8)                           :: tmp_sum

        integer(kind=8) :: i, j, k, l
        integer(kind=8) :: n_samples_unroll
        real(kind=8)    :: buffer_i(31), buffer_j(31)

        if ( allocated(mat_out) ) deallocate(mat_out)

        n_samples_unroll = n_samples - mod(n_samples, 31)
        if ( with_intercept ) then
            allocate(mat_out(n_columns+1, n_columns+1))
            call sum_of_matrix(col_sums, matrix, n_samples, n_columns, 1_8)

            do j=1, n_columns, 1
                do i=j, n_columns, 1
                    tmp_sum = 0d0
                    do k=1, n_samples_unroll, 31
                        do l=0, 31-1, 1
                            buffer_i(l+1) = mat_in(k+l,i)
                            buffer_j(l+1) = mat_in(k+l,j)
                        end do

                        do l=0, 31-1, 1
                            tmp_sum = tmp_sum + buffer_i(l+1) * buffer_j(l+1)
                        end do
                    end do

                    do k=n_samples_unroll+1, n_samples, 1
                        tmp_sum = tmp_sum + mat_in(k,i) * mat_in(k,j)
                    end do

                    mat_out(i,j) = tmp_sum
                    mat_out(j,i) = tmp_sum
                end do
            end do

            do j=1, n_columns, 1
                mat_out(j,n_columns+1) = col_sums(j)
                mat_out(n_columns+1,j) = col_sums(j)
            end do
            mat_out(n_columns+1, n_columns+1) = n_samples

        else
            allocate(mat_out(n_columns, n_columns))
            mat_out(:, :) = matmul(transpose(mat_in), mat_in)
        end if
    end subroutine mattxmat_new_n_samples_optimize_r8

    subroutine mattxmat_new_n_columns_inner_loop_optimize_r8(mat_out, mat_in, n_samples, n_columns, with_intercept)
        implicit none
        real(kind=8), ALLOCATABLE, intent(inout) :: mat_out(:,:)
        real(kind=8), intent(in)               :: mat_in(n_samples, n_columns)
        integer(kind=8), intent(in)            :: n_samples, n_columns
        logical(kind=4), intent(in)            :: with_intercept
        real(kind=8), allocatable              :: col_sums(:)
        real(kind=8)                           :: tmp_sums(3), tmp_sum, tmp_val
        integer(kind=8) :: buffer_size=3

        integer(kind=8) :: i, j, k, l, one=1
        integer(kind=8) :: n_columns_unroll, j_unroll
        real(kind=8)    :: zero=0

        if ( allocated(mat_out) ) deallocate(mat_out)
        if ( with_intercept ) then
            allocate(mat_out(n_columns+1, n_columns+1))
        else
            allocate(mat_out(n_columns, n_columns))
        end if

        do j=1, n_columns, 1
            j_unroll = j - mod(j, buffer_size)
            do i=1, j_unroll, buffer_size
                tmp_sums = zero
                do k=1, n_samples, 1
                    tmp_val = mat_in(k,j)
                    do l=0, buffer_size-1, 1
                        tmp_sums(l+1) = tmp_sums(l+1) + mat_in(k,i+l) * tmp_val
                    end do
                end do

                do l=0, buffer_size-1, 1
                    mat_out(i+l,j) = tmp_sums(l+1)
                    mat_out(j,i+l) = tmp_sums(l+1)
                end do
            end do

            do i=j_unroll+1, j
                tmp_sum = zero
                do k=1, n_samples, 1
                    tmp_sum = tmp_sum + mat_in(k,i) * mat_in(k,j)
                end do

                mat_out(i,j) = tmp_sum
                mat_out(j,i) = tmp_sum
            end do

        end do


        if ( with_intercept ) then
            call sum_of_matrix(col_sums, matrix, n_samples, n_columns, one)
            do j=1, n_columns, 1
                mat_out(j,n_columns+1) = col_sums(j)
                mat_out(n_columns+1,j) = col_sums(j)
            end do
            mat_out(n_columns+1, n_columns+1) = n_samples
        end if
    end subroutine mattxmat_new_n_columns_inner_loop_optimize_r8

    subroutine mattxmat_new_block_optimize_r8(mat_out, mat_in, n_samples, n_columns, with_intercept)
        implicit none
        real(kind=8), ALLOCATABLE, intent(inout) :: mat_out(:,:)
        real(kind=8), intent(in)                 :: mat_in(n_samples, n_columns)
        integer(kind=8), intent(in)              :: n_samples, n_columns
        logical(kind=4), intent(in)              :: with_intercept
        real(kind=8), allocatable                :: col_sums(:)
        real(kind=8)                             :: tmp_sums(2), tmp_sum, tmp_val
        integer(kind=8) :: buffer_size=2

        integer(kind=8) :: i, j, k, il, jl, one=1
        integer(kind=8) :: n_columns_unroll, j_unroll, i_unroll
        real(kind=8)    :: zero=0

        if ( allocated(mat_out) ) deallocate(mat_out)

        if ( with_intercept ) then
            allocate(mat_out(n_columns+1, n_columns+1))
            n_columns_unroll = n_columns - mod(n_columns, buffer_size)
        else
            allocate(mat_out(n_columns, n_columns))
        end if

        mat_out = zero
            ! Feature_i x Feature_j
            do j=1, n_columns_unroll, buffer_size
                do jl=0, buffer_size-1, 1
                    do i=1, n_columns_unroll, buffer_size
                        tmp_sums = zero
                        do il=0, buffer_size-1, 1
                            do k=1, n_samples, 1
                                tmp_sums(il+1) = tmp_sums(il+1) + mat_in(k,i+il) * mat_in(k,j+jl)
                            end do
                        end do

                        do il=0, buffer_size-1, 1
                            mat_out(i+il,j+jl) = tmp_sums(il+1)
                        end do
                    end do

                    do i=n_columns_unroll+1, n_columns, 1
                        tmp_sum = zero
                        do k=1, n_samples, 1
                            tmp_sum = tmp_sum + mat_in(k,i) * mat_in(k,j+jl)
                        end do
                        mat_out(i,j+jl) = tmp_sum
                    end do
                end do
            end do

            do j=n_columns_unroll+1, n_columns, 1
                do i=1, n_columns_unroll, buffer_size
                    tmp_sums = zero
                    do il=0, buffer_size-1, 1
                        do k=1, n_samples, 1
                            tmp_sums(il+1) = tmp_sums(il+1) + mat_in(k,i+il) * mat_in(k,j)
                        end do
                    end do

                    do il=0, buffer_size-1, 1
                        mat_out(i+il,j) = tmp_sums(il+1)
                    end do
                end do

                do i=n_columns_unroll+1, n_columns, 1
                    tmp_sum = zero
                    do k=1, n_samples, 1
                        tmp_sum = tmp_sum + mat_in(k,i) * mat_in(k,j)
                    end do
                    mat_out(i,j) = tmp_sum
                end do
            end do

        if ( with_intercept ) then
            call sum_of_matrix(col_sums, matrix, n_samples, n_columns, one)
            do j=1, n_columns, 1
                mat_out(j,n_columns+1) = col_sums(j)
                mat_out(n_columns+1,j) = col_sums(j)
            end do
            mat_out(n_columns+1, n_columns+1) = n_samples
        end if
    end subroutine mattxmat_new_block_optimize_r8

end program main_mattxmat
