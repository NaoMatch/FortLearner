program main_optimize
    use mod_const
    use mod_common
    use mod_random
    use mod_timer
    use mod_metric
    use mod_data_holder
    use mod_optimization

    implicit none

    integer(kind=8)    :: n_samples_train, n_columns_train
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE, target :: x_train(:,:), x_test(:,:)
    integer(kind=8), ALLOCATABLE, target :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE, target :: y_train_pred(:,:), y_test_pred(:,:)
    type(metrics)     :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr

    type(steepest_descent) :: steep
    type(newton_method) :: newton
    real(kind=8), allocatable :: x_min(:), x_ini(:)
    integer(kind=8) :: n_vars0, iter, max_iter
    
    n_vars0 = 5
    allocate(x_ini(n_vars0))
    call random_number(x_ini)
    x_ini = x_ini * 100d0 - 50d0


    steep = steepest_descent()
    x_min = steep%optimize(n_vars0, x_ini=x_ini, loss=loss_, grad=grad_)
    print*, "Steepest: ", x_min

    newton = newton_method()
    x_min = newton%optimize(n_vars0, x_ini=x_ini, loss=loss_, grad=grad_, hess=hess_)
    print*, "Newton: ", x_min


contains

    function loss_(x)
        real(kind=8) :: loss_
        real(kind=8), intent(in) :: x(:)
        loss_ = sum(x(:) **2d0)
    end function loss_
        
    function grad_(x)
        real(kind=8), allocatable :: grad_(:)
        real(kind=8), intent(in) :: x(:)
        grad_ = 2d0 * x
    end function grad_

    function hess_(x)
        real(kind=8), ALLOCATABLE :: hess_(:,:)
        real(kind=8), intent(in) :: x(:)

        integer(kind=8) :: n_variable, i, j
        n_variable = size(x)
        allocate(hess_(n_variable, n_variable))
        hess_ = 0d0
        do i=1, n_variable, 1
            hess_(i,i) = 2d0
        end do
    end function hess_



end program main_optimize
