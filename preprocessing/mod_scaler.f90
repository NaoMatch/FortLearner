module mod_scaler
    use mod_const
    use mod_common
    use mod_error
    use mod_stats
    use mod_sort
    use mod_discretizer
    implicit none


    type base_scaler
        character(len=256) :: algo_name
        integer(kind=8) :: n_columns
        logical(kind=4) :: is_fitted=f_
        real(kind=8), ALLOCATABLE :: maxabs_vals(:)
        real(kind=8), ALLOCATABLE :: min_vals(:), max_vals(:)
        real(kind=8), ALLOCATABLE :: mean_vals(:), std_vals(:)
        real(kind=8), ALLOCATABLE :: q1st_vals(:), med_vals(:), q3rd_vals(:)
        real(kind=8), ALLOCATABLE :: shift_vals(:), scale_vals(:)
        logical(kind=4), allocatable :: is_skip(:)
    contains
        procedure :: transform => transform_base_scaler
    end type base_scaler

    type, extends(base_scaler) :: minmax_scaler
        real(kind=8) :: min_val=0d0
        real(kind=8) :: max_val=1d0
    contains
        procedure :: fit => fit_minmax_r8
    end type minmax_scaler

    interface minmax_scaler
        procedure :: new_minmax_scaler
    end interface minmax_scaler

    type, extends(base_scaler) :: maxabs_scaler
    contains
        procedure :: fit => fit_maxabs_r8
    end type maxabs_scaler

    interface maxabs_scaler
        procedure :: new_maxabs_scaler
    end interface maxabs_scaler

    type, extends(base_scaler) :: standard_scaler
        logical(kind=4) :: with_mean = t_
        logical(kind=4) :: with_std = t_
    contains
        procedure :: fit => fit_standard_r8
    end type standard_scaler

    interface standard_scaler
        procedure :: new_standard_scaler
    end interface standard_scaler

    type, extends(base_scaler) :: robust_scaler
        logical(kind=4) :: with_centering = t_
        logical(kind=4) :: with_scaling = t_        
    contains
        procedure :: fit => fit_robust_r8
    end type robust_scaler

    interface robust_scaler
        procedure :: new_robust_scaler
    end interface robust_scaler

contains

    function transform_base_scaler(this, x)
        implicit none
        class(base_scaler)       :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), allocatable :: transform_base_scaler(:,:)

        integer(kind=8) :: shape_x(2), n_samples, n_columns, j, i
        real(kind=8) :: tmp_shift, tmp_scale
        type(error) :: err

        shape_x = shape(x)
        n_samples = shape_x(1)
        n_columns = shape_x(2)
        call err%check_estimator_is_fitted(this%is_fitted, this%algo_name)
        call err%check_number_of_features_mismatch(this%n_columns, n_columns, this%algo_name)

        if (allocated(transform_base_scaler)) deallocate(transform_base_scaler)
        allocate(transform_base_scaler(n_samples, n_columns))
        do j=1, n_columns
            if ( this%is_skip(j) ) cycle
            tmp_shift = this%shift_vals(j)
            tmp_scale = this%scale_vals(j)
            do i=1, n_samples, 1
                transform_base_scaler(i,j) = x(i,j)*tmp_scale + tmp_shift
            end do
        end do
    end function transform_base_scaler


    !> A function to override 'minmax_scaler'
    !! \return returns 'minmax_scaler' object
    !! \param min_val ***optional*** the minimum value after transformation, default 0
    !! \param max_val ***optional*** the maximum value after transformation, default 1
    function new_minmax_scaler(min_val, max_val)
        implicit none
        real(kind=8), optional :: min_val, max_val
        type(minmax_scaler)    :: new_minmax_scaler, tmp
        tmp%algo_name = "minmax_scaler"
        if ( present(min_val) ) tmp%min_val = min_val
        if ( present(max_val) ) tmp%max_val = max_val
        new_minmax_scaler = tmp
    end function new_minmax_scaler
    

    !> A function to override 'maxabs_scaler'
    !! \return returns 'maxabs_scaler' object
    function new_maxabs_scaler()
        implicit none
        type(maxabs_scaler)    :: new_maxabs_scaler, tmp
        tmp%algo_name = "maxabs_scaler"
        new_maxabs_scaler = tmp
    end function new_maxabs_scaler


    !> A function to override 'standard_scaler'
    !! \return returns 'standard_scaler' object
    !! \param with_mean ***optional*** If True, shift to mean 0
    !! \param with_std ***optional*** If True, scale to variance 1
    function new_standard_scaler(with_mean, with_std)
        implicit none
        type(standard_scaler)    :: new_standard_scaler, tmp
        logical(kind=4), OPTIONAL :: with_mean, with_std
        if (present(with_mean)) tmp%with_mean = with_mean
        if (present(with_std)) tmp%with_std = with_std
        tmp%algo_name = "standard_scaler"
        new_standard_scaler = tmp
    end function new_standard_scaler


    !> A function to override 'robust_scaler'
    !! \return returns 'robust_scaler' object
    !! \param with_centering ***optional*** If True, shift by median
    !! \param with_scaling ***optional*** If True, scale by 75%-25%
    function new_robust_scaler(with_centering, with_scaling)
        implicit none
        type(robust_scaler)    :: new_robust_scaler, tmp
        logical(kind=4), OPTIONAL :: with_centering, with_scaling
        if (present(with_centering)) tmp%with_centering = with_centering
        if (present(with_scaling)) tmp%with_scaling = with_scaling
        tmp%algo_name = "robust_scaler"
        new_robust_scaler = tmp
    end function new_robust_scaler


    !> A subroutine to fit 'minmax_scaler'
    !! \return returns fitted 'minmax_scaler' object
    !! \param x input 2-dim matrix
    subroutine fit_minmax_r8(this, x)
        implicit none
        class(minmax_scaler)               :: this
        real(kind=8), intent(in)           :: x(:,:)

        integer(kind=8) :: j
        integer(kind=8) :: x_shape(2), n_samples, n_columns
        real(kind=8)    :: x_min_j, x_max_j
        real(kind=8)    :: tmp_shift, tmp_scale

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)
        if (allocated(this%min_vals)) deallocate(this%min_vals)
        if (allocated(this%max_vals)) deallocate(this%max_vals)
        allocate(this%min_vals(n_columns))
        allocate(this%max_vals(n_columns))

        do j=1, n_columns
            call get_minmax(x_min_j, x_max_j, x(:,j), n_samples)
            this%min_vals(j) = x_min_j
            this%max_vals(j) = x_max_j
        end do
        this % is_fitted = .true.
        this % n_columns = n_columns

        if (allocated(this%shift_vals)) deallocate(this%shift_vals)
        if (allocated(this%scale_vals)) deallocate(this%scale_vals)
        if (allocated(this%is_skip)) deallocate(this%is_skip)
        allocate(this%shift_vals(n_columns))
        allocate(this%scale_vals(n_columns))
        allocate(this%is_skip(n_columns))
        do j=1, n_columns, 1
            if (this%max_vals(j) .eq. this%min_vals(j)) then
                this%is_skip(j) = t_
                cycle
            else
                this%is_skip(j) = f_
                tmp_scale = (this%max_val - this%min_val) / ( this%max_vals(j) - this%min_vals(j) )
                tmp_shift =  this%min_val - this%min_vals(j) * tmp_scale
                this%shift_vals(j) = tmp_shift
                this%scale_vals(j) = tmp_scale
            end if
        end do
    end subroutine fit_minmax_r8


    !> A subroutine to fit 'maxabs_scaler'
    !! \return returns fitted 'maxabs_scaler' object
    !! \param x input 2-dim matrix
    subroutine fit_maxabs_r8(this, x)
        implicit none
        class(maxabs_scaler)               :: this
        real(kind=8), intent(in)           :: x(:,:)

        integer(kind=8) :: j
        integer(kind=8) :: x_shape(2), n_samples, n_columns
        real(kind=8)    :: x_min_j, x_max_j
        real(kind=8)    :: tmp_shift, tmp_scale

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)
        if (allocated(this%maxabs_vals)) deallocate(this%maxabs_vals)
        allocate(this%maxabs_vals(n_columns))

        do j=1, n_columns
            call get_minmax(x_min_j, x_max_j, x(:,j), n_samples)
            this%maxabs_vals(j) = maxval((/abs(x_min_j), abs(x_max_j)/))
        end do
        this % is_fitted = .true.
        this % n_columns = n_columns

        if (allocated(this%shift_vals)) deallocate(this%shift_vals)
        if (allocated(this%scale_vals)) deallocate(this%scale_vals)
        if (allocated(this%is_skip)) deallocate(this%is_skip)
        allocate(this%shift_vals(n_columns))
        allocate(this%scale_vals(n_columns))
        allocate(this%is_skip(n_columns))
        do j=1, n_columns, 1
            if (this%maxabs_vals(j) .eq. 0d0) then
                this%is_skip(j) = t_
                cycle
            else
                this%is_skip(j) = f_
                tmp_scale = 1d0 / this%maxabs_vals(j)
                tmp_shift =  0d0
                this%shift_vals(j) = tmp_shift
                this%scale_vals(j) = tmp_scale
            end if
        end do
    end subroutine fit_maxabs_r8


    !> A subroutine to fit 'standard_scaler'
    !! \return returns fitted 'standard_scaler' object
    !! \param x input 2-dim matrix
    subroutine fit_standard_r8(this, x)
        implicit none
        class(standard_scaler)               :: this
        real(kind=8), intent(in)           :: x(:,:)

        integer(kind=8) :: j
        integer(kind=8) :: x_shape(2), n_samples, n_columns
        real(kind=8), ALLOCATABLE    :: means(:), variances(:)
        real(kind=8)    :: tmp_shift, tmp_scale

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)
        if (allocated(this%mean_vals)) deallocate(this%mean_vals)
        if (allocated(this%std_vals)) deallocate(this%std_vals)
        allocate(this%mean_vals(n_columns))
        allocate(this%std_vals(n_columns))

        allocate(means(n_columns))
        allocate(variances(n_columns))
        means = 0d0
        variances = 1d0
        if (this%with_mean) means = mean(x, n_samples, n_columns)
        if (this%with_std)  variances = variance(x, n_samples, n_columns, means_of_matrix=means)

        do j=1, n_columns
            this%mean_vals(j) = means(j)
            this%std_vals(j) = sqrt(variances(j))
        end do
        this % is_fitted = .true.
        this % n_columns = n_columns

        if (allocated(this%shift_vals)) deallocate(this%shift_vals)
        if (allocated(this%scale_vals)) deallocate(this%scale_vals)
        if (allocated(this%is_skip)) deallocate(this%is_skip)
        allocate(this%shift_vals(n_columns))
        allocate(this%scale_vals(n_columns))
        allocate(this%is_skip(n_columns))
        do j=1, n_columns, 1
            if (this%std_vals(j) .eq. 0d0) then
                this%is_skip(j) = t_
                cycle
            else
                this%is_skip(j) = f_
                tmp_scale = 1d0 / this%std_vals(j)
                tmp_shift = - this%mean_vals(j) * tmp_scale
                this%shift_vals(j) = tmp_shift
                this%scale_vals(j) = tmp_scale
            end if
        end do
    end subroutine fit_standard_r8



    !> A subroutine to fit 'robust_scaler'
    !! \return returns fitted 'robust_scaler' object
    !! \param x input 2-dim matrix
    subroutine fit_robust_r8(this, x)
        implicit none
        class(robust_scaler)               :: this
        real(kind=8), intent(in)           :: x(:,:)

        integer(kind=8) :: j, i, q25, q50, q75, k, qr, q, r,q_stats(3)
        integer(kind=8) :: x_shape(2), n_samples, n_columns
        real(kind=8), ALLOCATABLE    :: means(:), variances(:)
        real(kind=8)    :: tmp_shift, tmp_scale, step, qth
        real(kind=8), ALLOCATABLE :: f_copy(:)
        type(column_discretizer) :: c_disc

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)
        if (allocated(this%q1st_vals)) deallocate(this%q1st_vals)
        if (allocated(this%q3rd_vals)) deallocate(this%q3rd_vals)
        if (allocated(this%med_vals)) deallocate(this%med_vals)
        allocate(this%q1st_vals(n_columns))
        allocate(this%q3rd_vals(n_columns))
        allocate(this%med_vals(n_columns))

        allocate(f_copy(n_samples))
        do j=1, n_columns
            do i=1, n_samples, 1
                f_copy(i) = x(i,j)
            end do
            c_disc = column_discretizer(max_bins=4_8, strategy="quantile")
            call c_disc%fit(f_copy)

            this%q1st_vals(j) = c_disc%thresholds_(1)
            this%med_vals(j)  = c_disc%thresholds_(2)
            this%q3rd_vals(j) = c_disc%thresholds_(3)
        end do
        this % is_fitted = .true.
        this % n_columns = n_columns

        if (allocated(this%shift_vals)) deallocate(this%shift_vals)
        if (allocated(this%scale_vals)) deallocate(this%scale_vals)
        if (allocated(this%is_skip)) deallocate(this%is_skip)
        allocate(this%shift_vals(n_columns))
        allocate(this%scale_vals(n_columns))
        allocate(this%is_skip(n_columns))
        do j=1, n_columns, 1
            if (this%q1st_vals(j) .eq. this%q3rd_vals(j)) then
                this%is_skip(j) = t_
                cycle
            else
                this%is_skip(j) = f_

                if ( this%with_scaling ) then
                    tmp_scale = 1d0 / (this%q3rd_vals(j) - this%q1st_vals(j))
                else
                    tmp_scale = 1d0
                end if
                
                if ( this%with_centering ) then
                    tmp_shift = - this%med_vals(j) * tmp_scale
                else
                    tmp_shift = 0d0
                end if
                
                this%shift_vals(j) = tmp_shift
                this%scale_vals(j) = tmp_scale
            end if
        end do
    end subroutine fit_robust_r8



end module mod_scaler
