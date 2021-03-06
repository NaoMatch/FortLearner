program main_forest
    use mod_const
    use mod_common
    use mod_timer

    use mod_metric
    use mod_decision_tree
    use mod_extra_tree

    use mod_forest
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8), time_dt, time_et, time_rf, time_ets

    integer(kind=8)    :: n_samples_train, n_columns_train
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:), x_test(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred_dt(:,:), y_test_pred_dt(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred_et(:,:), y_test_pred_et(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred_rf(:,:), y_test_pred_rf(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred_ets(:,:), y_test_pred_ets(:,:)
    
    type(metrics)                 :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr
    type(decision_tree_regressor) :: dt_reg
    type(extra_tree_regressor)    :: et_reg
    type(random_forest_regressor) :: rf_reg
    type(extra_trees_regressor)   :: ets_reg
    integer(kind=8)    :: i, n_leaf_nodes, max_leaf_nodes
    integer(kind=8)    :: iter, max_iter

    print*, '============================================================='
    print*, "Input Data Shape: "
    n_samples_train  = 7654_8
    n_samples_test   = 957_8
    n_columns_train  = 4_8
    n_columns_test   = 4_8
    skip_header = t_
    dtype_in = "r"
    dtype_out = "r"
    print*, "    train (row x col): ", n_samples_train, n_columns_train
    print*, "    test  (row x col): ", n_samples_test, n_columns_test
    print*, "    skip header:       ", skip_header
    print*, "    data type input:   ", dtype_in
    print*, "    data type output:  ", dtype_out

    print*, '============================================================='
    print*, "File Names: "
    file_name_x_train_csv = "./input/Combined_Cycle_x_train.csv"
    file_name_y_train_csv = "./input/Combined_Cycle_y_train.csv"
    file_name_x_train_bin = "./input/Combined_Cycle_x_train.bin"
    file_name_y_train_bin = "./input/Combined_Cycle_y_train.bin"
    file_name_x_test_csv  = "./input/Combined_Cycle_x_test.csv"
    file_name_y_test_csv  = "./input/Combined_Cycle_y_test.csv"
    file_name_x_test_bin  = "./input/Combined_Cycle_x_test.bin"
    file_name_y_test_bin  = "./input/Combined_Cycle_y_test.bin"
    print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)


    ! print*, '============================================================='
    ! print*, "Input Data Shape: "
    ! n_samples_train  = 824_8
    ! n_samples_test   = 103_8
    ! n_columns_train  = 8_8
    ! n_columns_test   = 8_8
    ! skip_header = t_
    ! dtype_in = "r"
    ! dtype_out = "r"
    ! print*, "    train (row x col): ", n_samples_train, n_columns_train
    ! print*, "    test  (row x col): ", n_samples_test, n_columns_test
    ! print*, "    skip header:       ", skip_header
    ! print*, "    data type input:   ", dtype_in
    ! print*, "    data type output:  ", dtype_out

    ! print*, '============================================================='
    ! print*, "File Names: "
    ! file_name_x_train_csv = "./input/Concrete_x_train.csv"
    ! file_name_y_train_csv = "./input/Concrete_y_train.csv"
    ! file_name_x_train_bin = "./input/Concrete_x_train.bin"
    ! file_name_y_train_bin = "./input/Concrete_y_train.bin"
    ! file_name_x_test_csv  = "./input/Concrete_x_test.csv"
    ! file_name_y_test_csv  = "./input/Concrete_y_test.csv"
    ! file_name_x_test_bin  = "./input/Concrete_x_test.bin"
    ! file_name_y_test_bin  = "./input/Concrete_y_test.bin"
    ! print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    ! print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    ! print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    ! print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)

    print*, '============================================================='
    print*, "CSV to Binary"
    print*, "    x_train"
    call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
        n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)
    print*, "    y_train"
    call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
        n_samples_train, 1_8, skip_header, dtype_in, dtype_out)
    print*, "    x_test"
    call read2bin_2d(file_name_x_test_csv, file_name_x_test_bin, &
        n_samples_test, n_columns_test, skip_header, dtype_in, dtype_out)
    print*, "    y_test"
    call read2bin_2d(file_name_y_test_csv, file_name_y_test_bin, &
        n_samples_test, 1_8, skip_header, dtype_in, dtype_out)


    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train"
    call read_bin_2d(file_name_x_train_bin, x_train)
    print*, "    y_train"
    call read_bin_2d(file_name_y_train_bin, y_train)
    print*, "    x_test"
    call read_bin_2d(file_name_x_test_bin, x_test)
    print*, "    y_test"
    call read_bin_2d(file_name_y_test_bin, y_test)

    print*, '============================================================='
    print*, "data_holder"
    dholder = data_holder(x_train, y_train)
    dholder_ptr => dholder
    
    max_iter = 1
    max_leaf_nodes = 20
    do n_leaf_nodes=2, max_leaf_nodes
        dt_reg = decision_tree_regressor(max_leaf_nodes=n_leaf_nodes, fashion="best")
        call date_and_time(values=date_value1)
        do iter=1, max_iter, 1
            call dt_reg%fit(dholder_ptr)
        end do
        call date_and_time(values=date_value2)
        y_train_pred_dt = dt_reg%predict(x_train)
        y_test_pred_dt = dt_reg%predict(x_test)
        time_dt = time_diff(date_value1, date_value2)


        et_reg = extra_tree_regressor(max_leaf_nodes=n_leaf_nodes, fashion="best", n_repeats=1_8)
        call date_and_time(values=date_value1)
        do iter=1, max_iter, 1
            call et_reg%fit(dholder_ptr)
        end do
        call date_and_time(values=date_value2)
        y_train_pred_et = et_reg%predict(x_train)
        y_test_pred_et = et_reg%predict(x_test)
        time_et = time_diff(date_value1, date_value2)


        rf_reg = random_forest_regressor(max_leaf_nodes=n_leaf_nodes, fashion="best")
        call date_and_time(values=date_value1)
        do iter=1, max_iter, 1
            call rf_reg%fit(dholder_ptr)
        end do
        call date_and_time(values=date_value2)
        y_train_pred_rf = rf_reg%predict(x_train)
        y_test_pred_rf = rf_reg%predict(x_test)
        time_rf = time_diff(date_value1, date_value2)


        ets_reg = extra_trees_regressor(max_leaf_nodes=n_leaf_nodes, fashion="best")
        call date_and_time(values=date_value1)
        do iter=1, max_iter, 1
            call ets_reg%fit(dholder_ptr)
        end do
        call date_and_time(values=date_value2)
        y_train_pred_ets = ets_reg%predict(x_train)
        y_test_pred_ets = ets_reg%predict(x_test)
        time_ets = time_diff(date_value1, date_value2)


        print*, "    mse train vs test: ", n_leaf_nodes, &
            ! print*, '============================================================='
            real(metric%mean_square_error(y_train(:,1), y_train_pred_dt(:,1))), &
            real(metric%mean_square_error(y_test(:,1), y_test_pred_dt(:,1))), &
            real(time_dt/dble(max_iter)), "[msec]", &
            ! print*, '============================================================='
            real(metric%mean_square_error(y_train(:,1), y_train_pred_et(:,1))), &
            real(metric%mean_square_error(y_test(:,1), y_test_pred_et(:,1))), &
            real(time_et/dble(max_iter)), "[msec]", &
            ! print*, '============================================================='
            real(metric%mean_square_error(y_train(:,1), y_train_pred_rf(:,1))), &
            real(metric%mean_square_error(y_test(:,1), y_test_pred_rf(:,1))), &
            real(time_rf/dble(max_iter)), "[msec]", &
            ! print*, '============================================================='
            real(metric%mean_square_error(y_train(:,1), y_train_pred_ets(:,1))), &
            real(metric%mean_square_error(y_test(:,1), y_test_pred_ets(:,1))), &
            real(time_ets/dble(max_iter)), "[msec]"
    end do

end program main_forest
