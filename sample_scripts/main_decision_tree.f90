program main_decision_tree
    use mod_const
    use mod_random
    use mod_timer
    use mod_metric
    use mod_data_holder
    use mod_decision_tree
    use mod_extra_tree
    use mod_clouds
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8), time_dt, time_et, time_cl

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
    real(kind=8), ALLOCATABLE :: y_train_pred_cl(:,:), y_test_pred_cl(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)
    
    type(metrics)                 :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr
    type(decision_tree_regressor) :: dt_reg
    type(extra_tree_regressor)    :: et_reg
    type(clouds_regressor)        :: cl_reg
    integer(kind=8)    :: i, n_leaf_nodes, max_leaf_nodes
    integer(kind=8)    :: iter, max_iter

    print*, '============================================================='
    print*, "Input Data Shape: "
    n_samples_train  = 501808
    n_samples_test   = 501808
    n_columns_train  = 310_8
    n_columns_test   = 310_8
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
    file_name_x_train_csv = "../../Numerai/latest_numerai_datasets (1)/numerai_training_data_feature.csv"
    file_name_y_train_csv = "../../Numerai/latest_numerai_datasets (1)/numerai_training_data_target.csv"
    file_name_x_train_bin = "../../Numerai/latest_numerai_datasets (1)/numerai_training_data_feature.bin"
    file_name_y_train_bin = "../../Numerai/latest_numerai_datasets (1)/numerai_training_data_target.bin"
    file_name_x_test_csv = "../../Numerai/latest_numerai_datasets (1)/numerai_training_data_feature.csv"
    file_name_y_test_csv = "../../Numerai/latest_numerai_datasets (1)/numerai_training_data_target.csv"
    file_name_x_test_bin = "../../Numerai/latest_numerai_datasets (1)/numerai_training_data_feature.bin"
    file_name_y_test_bin = "../../Numerai/latest_numerai_datasets (1)/numerai_training_data_target.bin"
    print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)

    print*, '============================================================='
    print*, "Input Data Shape: "
    n_samples_train  = 17010_8
    n_samples_test   = 2127_8
    n_columns_train  = 81_8
    n_columns_test   = 81_8
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
    file_name_x_train_csv = "./input/SuperConductivity_x_train.csv"
    file_name_y_train_csv = "./input/SuperConductivity_y_train.csv"
    file_name_x_train_bin = "./input/SuperConductivity_x_train.bin"
    file_name_y_train_bin = "./input/SuperConductivity_y_train.bin"
    file_name_x_test_csv  = "./input/SuperConductivity_x_test.csv"
    file_name_y_test_csv  = "./input/SuperConductivity_y_test.csv"
    file_name_x_test_bin  = "./input/SuperConductivity_x_test.bin"
    file_name_y_test_bin  = "./input/SuperConductivity_y_test.bin"
    print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)


    ! print*, '============================================================='
    ! print*, "Input Data Shape: "
    ! n_samples_train  = 7654_8
    ! n_samples_test   = 957_8
    ! n_columns_train  = 4_8
    ! n_columns_test   = 4_8
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
    ! file_name_x_train_csv = "./input/Combined_Cycle_x_train.csv"
    ! file_name_y_train_csv = "./input/Combined_Cycle_y_train.csv"
    ! file_name_x_train_bin = "./input/Combined_Cycle_x_train.bin"
    ! file_name_y_train_bin = "./input/Combined_Cycle_y_train.bin"
    ! file_name_x_test_csv  = "./input/Combined_Cycle_x_test.csv"
    ! file_name_y_test_csv  = "./input/Combined_Cycle_y_test.csv"
    ! file_name_x_test_bin  = "./input/Combined_Cycle_x_test.bin"
    ! file_name_y_test_bin  = "./input/Combined_Cycle_y_test.bin"
    ! print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    ! print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    ! print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    ! print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)


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

    print*, '============================================================='
    print*, "Feature Indices"
    allocate(feature_indices(n_columns_train))
    allocate(feature_indices_scanning_range(2))
    do i=1, n_columns_train, 1
        feature_indices(i) = i
    end do
    feature_indices_scanning_range = (/2,4/)

    max_iter = 1
    max_leaf_nodes = 40
    do n_leaf_nodes=5, max_leaf_nodes
        ! print*, "decision_tree_regressor"
        dt_reg = decision_tree_regressor(max_leaf_nodes=n_leaf_nodes, fashion="best")
        call date_and_time(values=date_value1)
        do iter=1, max_iter, 1
            ! call dt_reg%fit(dholder_ptr, & 
            !     feature_indices=feature_indices, & 
            !     feature_indices_scanning_range=feature_indices_scanning_range)
            call dt_reg%fit(dholder_ptr)
        end do
        call date_and_time(values=date_value2)
        y_train_pred_dt = dt_reg%predict(x_train)
        y_test_pred_dt = dt_reg%predict(x_test)
        time_dt = time_diff(date_value1, date_value2)

        ! print*, "extra_tree_regressor"
        et_reg = extra_tree_regressor(max_leaf_nodes=n_leaf_nodes, fashion="best", n_repeats=1_8)
        call date_and_time(values=date_value1)
        do iter=1, max_iter, 1
            ! call et_reg%fit(dholder_ptr, & 
            !     feature_indices=feature_indices, & 
            !     feature_indices_scanning_range=feature_indices_scanning_range)
            call et_reg%fit(dholder_ptr)
        end do
        call date_and_time(values=date_value2)
        y_train_pred_et = et_reg%predict(x_train)
        y_test_pred_et = et_reg%predict(x_test)
        time_et = time_diff(date_value1, date_value2)

        ! print*, "clouds_regressor"
        cl_reg = clouds_regressor(max_leaf_nodes=n_leaf_nodes, fashion="best", max_bins=127_8, strategy="quantile")
        call cl_reg%fit(dholder_ptr, print_node=f_)
        call date_and_time(values=date_value1)
        do iter=1, max_iter, 1
            ! dholder = data_holder(x_train, y_train)
            ! call cl_reg%fit(dholder_ptr, & 
            !     feature_indices=feature_indices, & 
            !     feature_indices_scanning_range=feature_indices_scanning_range)
            call cl_reg%fit(dholder_ptr, print_node=f_)
        end do
        call date_and_time(values=date_value2)
        y_train_pred_cl = cl_reg%predict(x_train)
        y_test_pred_cl = cl_reg%predict(x_test)
        time_cl = time_diff(date_value1, date_value2)

        print*, "    mse train vs test: ", n_leaf_nodes, &
            ! print*, '============================================================='
            ! real(metric%mean_square_error(y_train(:,1), y_train_pred_dt(:,1))), &
            real(metric%mean_square_error(y_test(:,1), y_test_pred_dt(:,1))), &
            real(time_dt/dble(max_iter)), "[msec]", &
            ! print*, '============================================================='
            ! real(metric%mean_square_error(y_train(:,1), y_train_pred_et(:,1))), &
            real(metric%mean_square_error(y_test(:,1), y_test_pred_et(:,1))), &
            real(time_et/dble(max_iter)), "[msec]", &
            ! print*, '============================================================='
            ! real(metric%mean_square_error(y_train(:,1), y_train_pred_cl(:,1))), &
            real(metric%mean_square_error(y_test(:,1), y_test_pred_cl(:,1))), &
            real(time_cl/dble(max_iter)), "[msec]"

    end do
end program main_decision_tree
