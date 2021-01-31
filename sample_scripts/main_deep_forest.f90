program main_deep_forest    
    use mod_const
    use mod_common
    use mod_timer

    use mod_metric
    use mod_gradient_boosting_tree
    use mod_deep_forest
    implicit none

    type(deep_forest_regressor) :: df_reg

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
    real(kind=8), ALLOCATABLE :: y_train_pred_gbdt(:,:), y_test_pred_gbdt(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)
    
    type(metrics)                 :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr
    type(data_holder), target     :: dholder_test
    type(data_holder), pointer    :: dholder_test_ptr
    type(decision_tree_regressor) :: dt_reg
    type(extra_tree_regressor)    :: et_reg
    integer(kind=8)    :: i, n_leaf_nodes, max_leaf_nodes
    integer(kind=8)    :: iter, max_iter
    type(gradient_boosting_tree_regressor) :: gbt_reg



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


    print*, '============================================================='
    print*, "Input Data Shape: "
    n_samples_train  = 824_8
    n_samples_test   = 103_8
    n_columns_train  = 8_8
    n_columns_test   = 8_8
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
    file_name_x_train_csv = "./input/Concrete_x_train.csv"
    file_name_y_train_csv = "./input/Concrete_y_train.csv"
    file_name_x_train_bin = "./input/Concrete_x_train.bin"
    file_name_y_train_bin = "./input/Concrete_y_train.bin"
    file_name_x_test_csv  = "./input/Concrete_x_test.csv"
    file_name_y_test_csv  = "./input/Concrete_y_test.csv"
    file_name_x_test_bin  = "./input/Concrete_x_test.bin"
    file_name_y_test_bin  = "./input/Concrete_y_test.bin"
    print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)

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
    dholder_test = data_holder(x_test, y_test)
    dholder_test_ptr => dholder_test

    df_reg = deep_forest_regressor(& 
        n_estimators=100_8, &
        feature_fractions=(/1d0/2d0, 1d0/3d0, 1d0/4d0/), &
        step_size_for_multi_grain=1_8, &
        min_columns_in_grain=1_8, &
        max_leaf_nodes = 10_8, &
        n_cascades=10_8, &
        n_forest_per_layer = 4_8)

    call df_reg%fit_mgrain(dholder_ptr)



    dholder = data_holder(x_train, y_train)
    dholder_ptr => dholder
    gbt_reg = gradient_boosting_tree_regressor(n_estimators=100_8, &
        max_leaf_nodes=10_8, fashion="best", min_samples_leaf=10_8, learning_rate=0.1d0)
    call gbt_reg%fit(dholder_ptr)
    y_train_pred_gbdt = gbt_reg%predict(x_train)
    print*, real(metric%mean_square_error(y_train(:,1), y_train_pred_gbdt(:,1)))

    y_test_pred_gbdt = gbt_reg%predict(x_test)
    print*, real(metric%mean_square_error(y_test(:,1), y_test_pred_gbdt(:,1)))

end program main_deep_forest
