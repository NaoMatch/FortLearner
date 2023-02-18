!> Kernel module for kernel svm.
!> Kernel values are computed row-wise of Qmatrix, not element-wise.
module mod_kernel
    use mod_stats
    use mod_linalg, only: matrix_sqsum_row
    implicit none

    !> A kernel type for kernel svm.
    type kernel
        integer(kind=8) :: n_samples, n_columns !> number of samples of all training data
        integer(kind=8) :: n_samples_, n_columns_ !> number of samples of shrunk data
        integer(kind=8) :: kernel_int !> kernel type. 1=linear, 
        integer(kind=8) :: degree !> dgree for poly kernle
        real(kind=8) :: w0 !> intercept
        real(kind=8) :: gamma !> scale value
                
        real(kind=8), allocatable :: x_(:,:) !> training result.
        real(kind=8), pointer :: x_ptr(:,:) !> pointer of all training data
        real(kind=8), pointer :: x_ptr_(:,:) !> pointer of shrunk training data

        real(kind=8), allocatable :: x_sq_sum(:) !> squared sum of x by row. used for 'rbf' kernel
    contains
        procedure :: set_training_data
        procedure :: set_shrunk_data
        procedure :: set_training_result
        procedure :: compute_diagonal_elements
        procedure :: compute_kth_row
    end type kernel

contains


    !> Set training result. training samples with meaning infomation (a>0) is stored.
    !! \param x all training samples
    !! \param n_samples number of rows of 'x'
    !! \param n_columns number of columns of 'x'
    !! \param indices_non_zero_a indices to be stored
    !! \param n_non_zero_a number of meaningful samples
    subroutine set_training_result(this, x, n_samples, n_columns, indices_non_zero_a, n_non_zero_a)
        implicit none
        class(kernel) :: this
        real(kind=8), intent(in) :: x(n_samples, n_columns)
        integer(kind=8), intent(in) :: n_samples, n_columns
        integer(kind=8), intent(in) :: indices_non_zero_a(n_non_zero_a)
        integer(kind=8), intent(in) :: n_non_zero_a

        integer(kind=8) :: idx, n

        this%n_samples = n_non_zero_a
        this%n_columns = n_columns
        if (allocated(this%x_)) deallocate(this%x_)
        allocate(this%x_(this%n_samples, this%n_columns))
        do n=1, this%n_samples, 1
            idx = indices_non_zero_a(n)
            this%x_(n,:) = x(idx,:)
        end do

        if (this%kernel_int == 4_8) then
            if (allocated(this%x_sq_sum)) deallocate(this%x_sq_sum)
            allocate(this%x_sq_sum(this%n_samples))
            call matrix_sqsum_row(this%x_, this%x_sq_sum, this%n_samples, this%n_columns)
        end if
    end subroutine set_training_result


    !> Set data before training.
    !! \param x all training samples
    subroutine set_training_data(this, x)
        implicit none
        class(kernel) :: this
        real(kind=8), target :: x(:,:)

        integer(kind=8) :: n_elms

        this%n_samples = size(x, dim=1)
        this%n_columns = size(x, dim=2)
        this%x_ptr => x

        n_elms = this%n_samples * this%n_columns

        this%gamma = 1d0 &
                / (sum(x**2d0) / dble(n_elms) - (sum(x) / dble(n_elms))**2d0) &
                / dble(this%n_columns)

        if (this%kernel_int == 4_8) then
            if (allocated(this%x_sq_sum)) deallocate(this%x_sq_sum)
            allocate(this%x_sq_sum(this%n_samples))
            call matrix_sqsum_row(x, this%x_sq_sum, this%n_samples, this%n_columns)
        end if
    end subroutine set_training_data


    !> Set the data under training.
    !! \param x_ shrunk data set
    subroutine set_shrunk_data(this, x_)
        implicit none
        class(kernel) :: this
        real(kind=8), target :: x_(:,:)

        integer(kind=8) :: n_elms

        this%n_samples_ = size(x_, dim=1)
        this%n_columns_ = size(x_, dim=2)
        this%x_ptr_ => x_
    end subroutine set_shrunk_data


    !> Compute diagonal elements of Qmatrix.
    !! \param vals_diag values of diagonal elements of Qmatrix. computed result.
    !! \param n_vals number of values of 'vals_diag'.
    subroutine compute_diagonal_elements(this, vals_diag, n_vals)
        implicit none
        class(kernel) :: this
        real(kind=8), intent(inout) :: vals_diag(n_vals)
        integer(kind=8), intent(in) :: n_vals

        integer(kind=8) :: n

        if (this%kernel_int == 4_8) then
            vals_diag = 1d0
        elseif (this%kernel_int == 3_8) then
            do n=1, n_vals, 1
                vals_diag(n) = tanh( sum(this%x_ptr(n,:)**2d0)*this%gamma + this%w0 )
            end do
        elseif (this%kernel_int == 2_8) then
            do n=1, n_vals, 1
                vals_diag(n) = ( sum(this%x_ptr(n,:)**2d0)*this%gamma + this%w0 ) ** this%degree
            end do
        elseif (this%kernel_int == 1_8) then
            do n=1, n_vals, 1
                vals_diag(n) = sum(this%x_ptr(n,:)**2d0)
            end do
        end if
    end subroutine compute_diagonal_elements


    !> Compute k-th row's elements of Qmatrix.
    !! \param row_idx row index of Qmatrix to be computed.
    !! \param vals_kth_row values of k-th row's elements of Qmatrix. computed result.
    !! \param n_vals number of values of 'vals_kth_row'.
    subroutine compute_kth_row(this, row_idx, vals_kth_row, n_vals)
        implicit none
        class(kernel) :: this
        integer(kind=8), intent(in) :: row_idx
        real(kind=8), intent(inout) :: vals_kth_row(n_vals)
        integer(kind=8), intent(in) :: n_vals
        
        integer(kind=8) :: n

        if (this%kernel_int == 4_8) then
            vals_kth_row(:) = this%x_sq_sum(:) + this%x_sq_sum(row_idx)
            call dgemv("n", this%n_samples, this%n_columns, &
                    -2d0, this%x_ptr, this%n_samples, &
                    this%x_ptr(row_idx,:), 1_8, 1d0, &
                    vals_kth_row, 1_8)
            vals_kth_row(:) = exp(-vals_kth_row(:)*this%gamma)
        elseif (this%kernel_int == 3_8) then
            vals_kth_row = this%w0
            call dgemv("n", this%n_samples, this%n_columns, &
                    1d0, this%x_ptr, this%n_samples, &
                    this%x_ptr(row_idx,:), 1_8, 1d0, &
                    vals_kth_row, 1_8)
            vals_kth_row = tanh(vals_kth_row*this%gamma)
        elseif (this%kernel_int == 2_8) then
            vals_kth_row = this%w0
            call dgemv("n", this%n_samples, this%n_columns, &
                    1d0, this%x_ptr, this%n_samples, &
                    this%x_ptr(row_idx,:), 1_8, 1d0, &
                    vals_kth_row, 1_8)
            vals_kth_row = (vals_kth_row*this%gamma+this%w0) ** this%degree
        elseif (this%kernel_int == 1_8) then
            call dgemv("n", this%n_samples, this%n_columns, &
                    1d0, this%x_ptr, this%n_samples, &
                    this%x_ptr(row_idx,:), 1_8, 0d0, &
                    vals_kth_row, 1_8)
        end if
    end subroutine compute_kth_row


end module mod_kernel