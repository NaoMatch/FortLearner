module mod_nipals
    use mod_hyperparameter
    use mod_stats
    use mod_data_holder
    implicit none

    type nipals
        type(hparam_nipals) :: hparam
        real(kind=8), allocatable :: p(:,:)
        real(kind=8), allocatable :: mean_(:)
        integer(kind=8) :: n_samples, n_columns
    contains
        procedure, pass :: fit_nipals_x
        procedure, pass :: fit_nipals_dholder
        generic :: fit => fit_nipals_x, fit_nipals_dholder
        procedure, pass :: fit_transform_nipals_x
        procedure, pass :: fit_transform_nipals_dholder
        generic :: fit_transform => fit_transform_nipals_x, fit_transform_nipals_dholder
        ! procedure :: transform => transform_nipals
    end type nipals

    interface nipals
        module procedure :: new_nipals
    end interface nipals
    
contains
    

    function new_nipals(n_components, max_iteration, tolerance)
        implicit none
        type(nipals) :: new_nipals
        integer(kind=8), optional :: n_components
        integer(kind=8), optional :: max_iteration
        real(kind=8),    optional :: tolerance

        if (present(n_components)) new_nipals%hparam%n_components = n_components
        if (present(max_iteration)) new_nipals%hparam%max_iteration = max_iteration
        if (present(tolerance)) new_nipals%hparam%tolerance = tolerance
    end function new_nipals


    subroutine fit_nipals_x(this, x)
        implicit none
        class(nipals)            :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8)          :: xshape(2)

        real(kind=8), allocatable :: tmp_f(:), tmp_f_old(:), tmp_p(:), x_zero_mean(:,:)
        real(kind=8), allocatable :: tmp_m(:,:)
        integer(kind=8) :: fid, iter
        real(kind=8) :: div_f, div_p

        ! Shape Check
        xshape(:) = shape(x)
        this%n_samples = xshape(1)
        this%n_columns = xshape(2)
        this%hparam%n_components = minval([this%hparam%n_components, this%n_columns])

        ! Get Mean
        this%mean_ = mean(x, this%n_samples, this%n_columns)

        ! Shift
        x_zero_mean = x - spread(this%mean_, dim=1, ncopies=this%n_samples)

        ! Iteration
        allocate(tmp_f(this%n_samples), tmp_p(this%n_columns))
        allocate(tmp_f_old(this%n_samples))
        allocate(tmp_m(this%n_samples, this%n_columns))
        allocate(this%p(this%n_columns, this%hparam%n_components))
        tmp_f_old(:) = huge(0d0)
        do fid=1, this%hparam%n_components, 1
            ! print*, "fid: ", fid
            tmp_f(:) = x_zero_mean(:,fid)
            do iter=1, this%hparam%max_iteration, 1
                ! print*, "    iter: ", iter
                div_f = 1d0 / sum(tmp_f*tmp_f)
                call dgemv("t", this%n_samples, this%n_columns, &
                                div_f, x_zero_mean, this%n_samples, &
                                tmp_f, 1_8, 0d0, &
                                tmp_p, 1_8)
                tmp_p = tmp_p / sqrt(sum(tmp_p*tmp_p))

                call dgemv("n", this%n_samples, this%n_columns, &
                                1d0, x_zero_mean, this%n_samples, &
                                tmp_p, 1_8, 0d0, &
                                tmp_f, 1_8)
            end do
            call vv2mat(tmp_f, tmp_p, tmp_m, this%n_samples, this%n_columns)
            x_zero_mean = x_zero_mean - tmp_m
            this%p(:,fid) = tmp_p
        end do
    end subroutine fit_nipals_x

    
    subroutine fit_nipals_dholder(this, dholder)
        implicit none
        class(nipals) :: this
        type(data_holder), intent(in) :: dholder
        call this%fit_nipals_x(dholder%x_ptr%x_r8_ptr)
    end subroutine fit_nipals_dholder


    function fit_transform_nipals_x(this, x)
        implicit none
        class(nipals)            :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), allocatable :: fit_transform_nipals_x(:,:)
        integer(kind=8)          :: xshape(2)

        real(kind=8), allocatable :: tmp_f(:), tmp_f_old(:), tmp_p(:), x_zero_mean(:,:)
        real(kind=8), allocatable :: tmp_m(:,:)
        integer(kind=8) :: fid, iter
        real(kind=8) :: div_f, div_p

        ! Shape Check
        xshape(:) = shape(x)
        this%n_samples = xshape(1)
        this%n_columns = xshape(2)
        this%hparam%n_components = minval([this%hparam%n_components, this%n_columns])

        ! Get Mean
        this%mean_ = mean(x, this%n_samples, this%n_columns)

        ! Shift
        x_zero_mean = x - spread(this%mean_, dim=1, ncopies=this%n_samples)

        ! Iteration
        allocate(fit_transform_nipals_x(this%n_samples, this%hparam%n_components))
        allocate(tmp_f(this%n_samples), tmp_p(this%n_columns))
        allocate(tmp_f_old(this%n_samples))
        allocate(tmp_m(this%n_samples, this%n_columns))
        tmp_f_old(:) = huge(0d0)
        do fid=1, this%hparam%n_components, 1
            ! print*, "fid: ", fid
            tmp_f(:) = x_zero_mean(:,fid)
            do iter=1, this%hparam%max_iteration, 1
                ! print*, "    iter: ", iter
                div_f = 1d0 / sum(tmp_f*tmp_f)
                call dgemv("t", this%n_samples, this%n_columns, &
                                div_f, x_zero_mean, this%n_samples, &
                                tmp_f, 1_8, 0d0, &
                                tmp_p, 1_8)
                tmp_p = tmp_p / sqrt(sum(tmp_p*tmp_p))

                call dgemv("n", this%n_samples, this%n_columns, &
                                1d0, x_zero_mean, this%n_samples, &
                                tmp_p, 1_8, 0d0, &
                                tmp_f, 1_8)
            end do
            call vv2mat(tmp_f, tmp_p, tmp_m, this%n_samples, this%n_columns)
            x_zero_mean = x_zero_mean - tmp_m
            fit_transform_nipals_x(:,fid) = tmp_f
        end do



    end function fit_transform_nipals_x

    function fit_transform_nipals_dholder(this, dholder)
        implicit none
        class(nipals) :: this
        type(data_holder), intent(in) :: dholder
        real(kind=8), allocatable :: fit_transform_nipals_dholder(:,:)
        fit_transform_nipals_dholder = this%fit_transform_nipals_x(dholder%x_ptr%x_r8_ptr)
    end function fit_transform_nipals_dholder



end module mod_nipals