program main_forest_random_forest
    use mod_metric
    use mod_data_holder
    use mod_random_forest
    implicit none

    integer(kind=8)    :: date_value1(8), date_value2(8)
    integer(kind=8)    :: time_train
    integer(kind=8)    :: time_rf_naive, time_rf_jitted, time_jit, time_pack, time_rf_packed
    integer(kind=8)    :: n_samples_trains(5), n_columns_trains(5)
    integer(kind=8)    :: n_samples_train, n_columns_train 
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)


    type(metrics) :: metric
    type(decision_tree_regressor) :: dt_reg
    type(random_forest_regressor) :: rf_reg, rf_reg2, rf_reg3
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr
    type(forest_jitter) :: jitter
    integer(kind=8) :: node_idx, depth, max_depth, num
    type(node_axis) :: root_node


    file_name_x_train_bin = "../sample_data/make_regression_X_train_0000100000x00100.bin"
    file_name_y_train_bin = "../sample_data/make_regression_y_train_0000100000x00100.bin"
    file_name_x_train_bin = "../sample_data/make_regression_X_train_0001000000x00100.bin"
    file_name_y_train_bin = "../sample_data/make_regression_y_train_0001000000x00100.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)
    dholder = data_holder(x_train, y_train, is_trans_x=f_)
    dholder_ptr => dholder

    print*, '============================================================='
    print*, "Train: "
    rf_reg = random_forest_regressor(&
        n_estimators=10_8, max_depth=2_8, min_samples_leaf=500_8, num_threads=2_8)
    call date_and_time(values=date_value1)
    call rf_reg%fit(dholder)
    ! call rf_reg%fit_random_forest_regressor_ptr(dholder_ptr)
    call date_and_time(values=date_value2)
    time_train = time_diff(date_value1, date_value2)
    print*, "done"
    y_train_pred = rf_reg%predict(x_train)
    print*, metric%mean_square_error(y_train(:,1), y_train_pred(:,1)), time_train
    ! call rf_reg%dump(file_name="rf.bin")
    stop

    ! Load, Test ------------------------------------------------------------------------
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    print*, "Load Trained Model, Test"
    call rf_reg2%load(file_name="rf.bin")
    do num = 1, 8, 1
        rf_reg2%hparam%num_threads = num
        call date_and_time(values=date_value1)
        y_train_pred = rf_reg2%predict(x_train)
        call date_and_time(values=date_value2)
        time_rf_naive = time_diff(date_value1, date_value2)
        print*, num, metric%mean_square_error(y_train(:,1), y_train_pred(:,1)), time_rf_naive
    end do


    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    jitter = forest_jitter()
    call date_and_time(values=date_value1)
    call jitter%jit(rf_reg2%trees)
    call date_and_time(values=date_value2)
    time_jit = time_diff(date_value1, date_value2)

    do num = 1, 8, 1
        rf_reg2%hparam%num_threads = num
        call date_and_time(values=date_value1)
        y_train_pred = rf_reg2%predict(x_train)
        call date_and_time(values=date_value2)
        time_rf_jitted = time_diff(date_value1, date_value2)
        print*, num, metric%mean_square_error(y_train(:,1), y_train_pred(:,1)), time_rf_jitted, time_jit
    end do
        
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    call rf_reg3%load(file_name="rf.bin")
    rf_reg3%packer = forest_packing()
    call date_and_time(values=date_value1)
    call rf_reg3%packer%packing(rf_reg3%trees)
    call date_and_time(values=date_value2)
    time_pack = time_diff(date_value1, date_value2)

    do num = 1, 8, 1
        rf_reg3%hparam%num_threads = num
        call date_and_time(values=date_value1)
        y_train_pred = rf_reg3%predict(x_train)
        call date_and_time(values=date_value2)
        time_rf_packed = time_diff(date_value1, date_value2)
        print*, num, metric%mean_square_error(y_train(:,1), y_train_pred(:,1)), time_rf_packed, time_pack
    end do

end program main_forest_random_forest
