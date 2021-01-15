program main_decision_tree
    use mod_const
    use mod_timer
    use mod_metric
    use mod_data_holder
    use mod_decision_tree
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

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
    real(kind=8), ALLOCATABLE :: y_train_pred(:,:)
    real(kind=8), ALLOCATABLE :: y_test_pred(:,:)
    
    type(metrics)                 :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr
    type(decision_tree_regressor) :: dt_reg
    integer(kind=8)    :: i, n_leaf_nodes, max_leaf_nodes
    integer(kind=8)    :: iter, max_iter



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

    max_leaf_nodes = 100
    do n_leaf_nodes=2, max_leaf_nodes
        dt_reg = decision_tree_regressor(max_leaf_nodes=n_leaf_nodes, fashion="best")
        call dt_reg%fit(dholder_ptr)
        y_train_pred = dt_reg%predict(x_train)
        y_test_pred = dt_reg%predict(x_test)
        print*, "    mse train vs test: ", n_leaf_nodes, &
            metric%mean_square_error(y_train(:,1), y_train_pred(:,1)), &
            metric%mean_square_error(y_test(:,1), y_test_pred(:,1))
    end do
end program main_decision_tree
