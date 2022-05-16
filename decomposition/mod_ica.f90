module mod_ica
    use mod_hyperparameter
    implicit none

    type ica
        integer(kind=8)           :: x_shape(2), n_samples, n_columns
        type(hparam_ica)          :: hparam
        real(kind=8), allocatable :: w(:,:)
        real(kind=8), allocatable :: mean_(:)
    contains
        procedure :: fit => fit_ica
    end type ica

    interface ica
        module procedure :: new_ica
    end interface ica
    
contains

    function new_ica(n_components, max_iteration, tolerance)
        implicit none
        type(ica) :: new_ica
        integer(kind=8), optional :: n_components
        integer(kind=8), optional :: max_iteration
        real(kind=8),    optional :: tolerance

        if (present(n_components)) new_ica%hparam%n_components = n_components
        if (present(max_iteration)) new_ica%hparam%max_iteration = max_iteration
        if (present(tolerance)) new_ica%hparam%tolerance = tolerance
    end function new_ica

    subroutine fit_ica(this, x)
        implicit none
        class(ica) :: this
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8) :: c
        real(kind=8), allocatable :: wc(:), wg(:), wh(:)

        this%x_shape(:) = shape(x)
        this%n_samples = this%x_shape(1)
        this%n_columns = this%x_shape(2)
        this%hparam%n_components = minval([this%hparam%n_components, this%n_columns])

        allocate(this%w(this%n_columns, this%hparam%n_components))
        allocate(wc(this%n_columns), wg(this%n_columns))
        call random_number(this%w)
        this%w(:,:) = 2d0*this%w(:,:) - 1d0

        do c=1, this%hparam%n_components
            wc(:) = this%w(:,c)
            call dgemv("n", this%n_samples, this%n_columns, &
                        1d0, x_zero_mean, this%n_samples, &
                        tmp_p, 1_8, 0d0, &
                        tmp_f, 1_8)
            ! wg = tanh()

            this%w(:,c) = wc(:)
        end do


    contains
        elemental function f(x) result(res)
            implicit none
            real(kind=8), intent(in) :: x
            real(kind=8) :: res
            res = log(cosh(x))
        end function f 
        
        elemental function g(x) result(res)
            implicit none
            real(kind=8), intent(in) :: x
            real(kind=8) :: res
            res = tanh(x)
        end function g 
        
        elemental function h(x) result(res)
            implicit none
            real(kind=8), intent(in) :: x
            real(kind=8) :: res
            res = 1d0-tanh(x)**2d0
        end function h
    end subroutine fit_ica

    
end module mod_ica