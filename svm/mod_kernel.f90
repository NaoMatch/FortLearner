module mod_kernel
    use mod_stats
    use mod_linalg, only: matrix_sqsum_row
    implicit none

    type kernel
        integer(kind=8) :: n_samples, n_columns
        integer(kind=8) :: n_samples_, n_columns_
        integer(kind=8) :: kernel_int
        integer(kind=8) :: degree
        real(kind=8) :: w0
        real(kind=8) :: gamma

        real(kind=8)       :: sigma
        real(kind=8)       :: sigma_sq_inv
                
        real(kind=8), allocatable :: x_(:,:)
        real(kind=8), pointer :: x_ptr(:,:)
        real(kind=8), pointer :: x_ptr_(:,:)

        real(kind=8), allocatable :: x_sq_sum(:)
    contains
        procedure :: set_training_data
        procedure :: set_shrunk_data
        procedure :: set_training_result
        procedure :: compute_diagonal_elements
        procedure :: compute_qmatrix
        procedure :: compute_kth_row
    end type kernel


    type rbf_kernel
        real(kind=8)       :: sigma
        real(kind=8)       :: sigma_sq_inv

        real(kind=8), allocatable :: x_(:,:)
        integer(kind=8) :: n_samples, n_columns
    contains
        procedure :: val_ij  => val_ij_rbf_kernel
        procedure :: val_xij => val_xij_rbf_kernel
        procedure :: eval => eval_rbf_kernel
    end type rbf_kernel

    interface rbf_kernel
        procedure :: new_rbf_kernel
    end interface rbf_kernel

contains

    subroutine compute_qmatrix(this, q_mat, n_samples)
        implicit none
        class(kernel) :: this
        real(kind=8), intent(inout) :: q_mat(n_samples, n_samples)
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8) :: i, j

        do i=1, n_samples, 1
            do j=1, n_samples, 1
                q_mat(i,j) = exp(-sum((this%x_ptr(i,:) - this%x_ptr(j,:))**2d0) * this%sigma_sq_inv)
            end do
        end do
    end subroutine 

    subroutine set_training_result(this, x_, n_samples_, n_columns)
        implicit none
        class(kernel) :: this
        real(kind=8), intent(in) :: x_(n_samples_, n_columns)
        integer(kind=8), intent(in) :: n_samples_, n_columns
        this%n_samples = n_samples_
        this%n_columns = n_columns
        allocate(this%x_, source=x_)
    end subroutine set_training_result

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
        if (allocated(this%x_sq_sum)) deallocate(this%x_sq_sum)
        allocate(this%x_sq_sum(this%n_samples))
        call matrix_sqsum_row(x, this%x_sq_sum, this%n_samples, this%n_columns)
    end subroutine set_training_data

    subroutine set_shrunk_data(this, x_)
        implicit none
        class(kernel) :: this
        real(kind=8), target :: x_(:,:)

        integer(kind=8) :: n_elms

        this%n_samples_ = size(x_, dim=1)
        this%n_columns_ = size(x_, dim=2)
        this%x_ptr_ => x_
    end subroutine set_shrunk_data


    subroutine compute_diagonal_elements(this, vals_ith_row, n_vals)
        implicit none
        class(kernel) :: this
        real(kind=8), intent(inout) :: vals_ith_row(n_vals)
        integer(kind=8), intent(in) :: n_vals
        integer(kind=8) :: n

        vals_ith_row = 1d0
    end subroutine compute_diagonal_elements


    subroutine compute_kth_row(this, row_idx, vals_ith_row, n_vals)
        implicit none
        class(kernel) :: this
        integer(kind=8), intent(in) :: row_idx
        real(kind=8), intent(inout) :: vals_ith_row(n_vals)
        integer(kind=8), intent(in) :: n_vals
        
        integer(kind=8) :: n

        vals_ith_row(:) = this%x_sq_sum(:) + this%x_sq_sum(row_idx)
        call dgemv("n", this%n_samples, this%n_columns, &
                -2d0, this%x_ptr, this%n_samples, &
                this%x_ptr(row_idx,:), 1_8, 1d0, &
                vals_ith_row, 1_8)
        vals_ith_row(:) = exp(-vals_ith_row(:)*this%gamma)
    end subroutine compute_kth_row



    function new_rbf_kernel(x, sigma)
        implicit none
        type(rbf_kernel)         :: new_rbf_kernel
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), intent(in) :: sigma
        new_rbf_kernel%sigma = sigma
        new_rbf_kernel%sigma_sq_inv = 1d0 / (2d0*sigma**2d0)
        new_rbf_kernel%x_ = x(:,:)

        new_rbf_kernel%n_samples = size(x, dim=1)
        new_rbf_kernel%n_columns = size(x, dim=2)

        new_rbf_kernel%sigma_sq_inv = 1 /dble(new_rbf_kernel%n_columns)
    end function new_rbf_kernel


    function val_ij_rbf_kernel(this, i, j) result(dist)
        implicit none
        class(rbf_kernel) :: this
        real(kind=8) :: dist
        integer(kind=8), intent(in) :: i, j
        dist = exp(-sum((this%x_(i,:) - this%x_(j,:))**2d0) * this%sigma_sq_inv)
    end function 


    function val_xij_rbf_kernel(this, x_i, x_j, n_columns) result(dist)
        implicit none
        class(rbf_kernel) :: this
        real(kind=8) :: dist
        real(kind=8), intent(in) :: x_i(n_columns), x_j(n_columns)
        integer(kind=8), intent(in) :: n_columns
        dist = exp(-sum((x_i(:) - x_j(:))**2d0) * this%sigma_sq_inv)
    end function 

    
    function eval_rbf_kernel(this, z, n_samples, n_columns, idxs, n_idxs) result(kernel_mat)
        implicit none
        class(rbf_kernel) :: this
        real(kind=8), allocatable :: kernel_mat(:,:)
        real(kind=8), intent(in)  :: z(n_samples, n_columns)
        integer(kind=8), intent(in) :: n_samples, n_columns
        integer(kind=8), intent(in) :: idxs(n_idxs)
        integer(kind=8), intent(in) :: n_idxs
        integer(kind=8) :: i, j, idx

        allocate(kernel_mat(n_samples, n_idxs))
        if (n_idxs == 0_8) return

        kernel_mat(:,:) = 0d0
        do j=1, n_samples, 1
            do i=1, n_idxs, 1
                idx = idxs(i)
                kernel_mat(j,i) = this%val_xij(this%x_(idx,:), z(j,:), n_columns)
            end do
        end do
    end function eval_rbf_kernel



    











end module mod_kernel