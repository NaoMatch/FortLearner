program main_extra_tree
    use mod_const
    use mod_random
    use mod_timer
    use mod_metric
    use mod_data_holder
    use mod_extra_tree
    implicit none
    
    integer(kind=8) :: date_value1(8), date_value2(8), time_dt, time_et, time_cl, time_lw
    
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
    real(kind=8), ALLOCATABLE :: y_train_pred_lw(:,:), y_test_pred_lw(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)
    
    type(metrics)                 :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr
    type(extra_tree_regressor)    :: et_reg

    integer(kind=8)    :: i, n_leaf_nodes, max_leaf_nodes
    integer(kind=8)    :: iter, max_iter, power


    print*, '============================================================='
    print*, "Input Data Shape: "
    n_samples_train  = 9000000_8
    n_samples_test   = 1000000_8
    n_columns_train  = 28_8
    n_columns_test   = 28_8
    skip_header = f_
    dtype_in = "r"
    dtype_out = "r"
    print*, "    train (row x col): ", n_samples_train, n_columns_train
    print*, "    test  (row x col): ", n_samples_test, n_columns_test
    print*, "    skip header:       ", skip_header
    print*, "    data type input:   ", dtype_in
    print*, "    data type output:  ", dtype_out

    print*, '============================================================='
    print*, "File Names: "
    file_name_x_train_csv = "../../../uci_data/98_Higgs/HIGGS_train_x.csv"
    file_name_y_train_csv = "../../../uci_data/98_Higgs/HIGGS_train_y.csv"
    file_name_x_train_bin = "../../../uci_data/98_Higgs/HIGGS_train_x.bin"
    file_name_y_train_bin = "../../../uci_data/98_Higgs/HIGGS_train_y.bin"
    file_name_x_test_csv  = "../../../uci_data/98_Higgs/HIGGS_valid_x.csv"
    file_name_y_test_csv  = "../../../uci_data/98_Higgs/HIGGS_valid_y.csv"
    file_name_x_test_bin  = "../../../uci_data/98_Higgs/HIGGS_valid_x.bin"
    file_name_y_test_bin  = "../../../uci_data/98_Higgs/HIGGS_valid_y.bin"
    print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
    print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)
    print*, "   x_test  csv -> bin: ", trim(file_name_x_test_csv),  " -> ", trim(file_name_x_test_bin)
    print*, "   y_test  csv -> bin: ", trim(file_name_y_test_csv),  " -> ", trim(file_name_y_test_bin)


    ! print*, '============================================================='
    ! print*, "CSV to Binary"
    ! print*, "    x_train"
    ! call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
    !     n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)
    ! print*, "    y_train"
    ! call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
    !     n_samples_train, 1_8, skip_header, dtype_in, dtype_out)
    ! print*, "    x_test"
    ! call read2bin_2d(file_name_x_test_csv, file_name_x_test_bin, &
    !     n_samples_test, n_columns_test, skip_header, dtype_in, dtype_out)
    ! print*, "    y_test"
    ! call read2bin_2d(file_name_y_test_csv, file_name_y_test_bin, &
    !     n_samples_test, 1_8, skip_header, dtype_in, dtype_out)

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

    call date_and_time(values=date_value1)
    et_reg = extra_tree_regressor(max_leaf_nodes=100_8, fashion="best", n_repeats=1_8, & 
            min_samples_leaf=1_8)
    call et_reg%fit(dholder_ptr)
    call date_and_time(values=date_value2)
    y_train_pred_et = et_reg%predict(x_train)
    y_test_pred_et = et_reg%predict(x_test)
    time_et = time_diff(date_value1, date_value2)
    print*, "=============================================================================="
    print*, " ----- ExtraTree, Naive Implementation max_leaf_node=100"
    print*, "TrainMSE: ", real(metric%mean_square_error(y_train(:,1), y_train_pred_et(:,1)))
    print*, "Test_MSE: ", real(metric%mean_square_error(y_test(:,1), y_test_pred_et(:,1)))
    print*, "TIme    : ", real(time_et), "[msec]"


    print*, "=============================================================================="
    print*, "=============================================================================="
    print*, "=============================================================================="
    call date_and_time(values=date_value1)
    et_reg = extra_tree_regressor(max_leaf_nodes=100_8, fashion="best", n_repeats=1_8, & 
            min_samples_leaf=1_8)
    call et_reg%fit_extra_tree_regressor_spped_up(dholder_ptr)
    call date_and_time(values=date_value2)
    y_train_pred_et = et_reg%predict(x_train)
    y_test_pred_et = et_reg%predict(x_test)
    time_et = time_diff(date_value1, date_value2)
    print*, "=============================================================================="
    print*, " ----- ExtraTree, Naive Implementation max_leaf_node=100"
    print*, "TrainMSE: ", real(metric%mean_square_error(y_train(:,1), y_train_pred_et(:,1)))
    print*, "Test_MSE: ", real(metric%mean_square_error(y_test(:,1), y_test_pred_et(:,1)))
    print*, "TIme    : ", real(time_et), "[msec]"

end program main_extra_tree
