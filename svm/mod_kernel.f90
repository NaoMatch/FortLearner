module mod_kernel
    implicit none
    
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