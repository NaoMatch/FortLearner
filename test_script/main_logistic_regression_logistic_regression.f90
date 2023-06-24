program main_logistic_regression_logistic_regression
    use mod_common
    use mod_logistic_regression
    use mod_metric
    use mod_data_holder
    implicit none

    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:)
    integer(kind=8), ALLOCATABLE :: y_train(:,:)
    real(kind=8), ALLOCATABLE :: y_pred(:,:)

    type(metrics) :: metric

    type(logistic_regression) :: log_reg
    type(data_holder), target :: dholder
    type(data_holder), pointer :: dholder_ptr


    file_name_x_train_bin = "../sample_data/make_classification_X_train_0000001000x00005_class=002.bin"
    file_name_y_train_bin = "../sample_data/make_classification_y_train_0000001000x00005_class=002.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)

    dholder = data_holder(x_train, y_train)
    dholder_ptr => dholder

    log_reg = logistic_regression()

    
    call log_reg%fit_newton(dholder)
    y_pred = log_reg%predict(x_train)
    print*, metric%auc(y_train(:,1), y_pred(:,1))

    call log_reg%fit_bfgs(dholder)
    y_pred = log_reg%predict(x_train)
    print*, metric%auc(y_train(:,1), y_pred(:,1))

end program main_logistic_regression_logistic_regression