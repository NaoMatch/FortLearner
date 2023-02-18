program main_boosting_tree_gradient_boosting_adaboost_regressor
    use mod_timer
    use mod_metric
    use mod_common
    use mod_data_holder
    use mod_decision_tree
    use mod_gradient_boost_adaboost
    implicit none
    integer(kind=8)        :: date_value1(8), date_value2(8)
    type(adaboost_regressor) :: ab_reg, ab_reg2

    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)

    type(metrics) :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr

    ! file_name_x_train_bin = "../sample_data/make_regression_X_0000001000x00005.bin"
    ! file_name_y_train_bin = "../sample_data/make_regression_y_0000001000x00005.bin"
    ! call read_bin_2d(file_name_x_train_bin, x_train)
    ! call read_bin_2d(file_name_y_train_bin, y_train)

    file_name_x_train_bin = "../sample_data/make_regression_X_train_0000100000x00100.bin"
    file_name_y_train_bin = "../sample_data/make_regression_y_train_0000100000x00100.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)

    dholder = data_holder(x_train, y_train, is_trans_x=f_)
    dholder_ptr => dholder

    print*, "shape(x_train): ", shape(x_train), sum(x_train)
    print*, "shape(y_train): ", shape(y_train), sum(y_train)
    
    print*, '*********************************************************************************************'
    print*, "AdaboostRegressor"
    ab_reg = new_adaboost_regressor(n_estimators=10_8)
    call date_and_time(values=date_value1)
    call ab_reg%fit(dholder_ptr, x_train, y_train)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    y_train_pred = ab_reg%predict(dholder%x_ptr%x_r8_ptr)
    print*, metric%mean_square_error(y_train_pred(:,1), y_train(:,1))
    call ab_reg%dump("ab_reg.bin")
    
    print*, '*********************************************************************************************'
    print*, "AdaboostRegressor, load check"
    call ab_reg2%load("ab_reg.bin")
    y_train_pred = ab_reg%predict(dholder%x_ptr%x_r8_ptr)
    print*, metric%mean_square_error(y_train_pred(:,1), y_train(:,1))
    

end program main_boosting_tree_gradient_boosting_adaboost_regressor