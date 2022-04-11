program main
    use mod_metric
    use mod_data_holder
    use mod_decision_tree
    use mod_random_rotation_decision_tree
    use mod_sadt
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8), time_dt, time_et, time_cl, time_lw
    integer(kind=8)    :: n_samples_trains(5), n_columns_trains(5)
    integer(kind=8)    :: n_samples_train, n_columns_train 
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:), x_test(:,:)
    real(kind=8), ALLOCATABLE :: x_train_t(:,:), x_test_t(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred_dt(:,:), y_test_pred_dt(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred_et(:,:), y_test_pred_et(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred_cl(:,:), y_test_pred_cl(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred_lw(:,:), y_test_pred_lw(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)
    type(metrics)                 :: metric
    integer(kind=8)    :: i, n_leaf_nodes, max_leaf_nodes, iii, jjj, n_iters(5), n_iter
    integer(kind=8)    :: iter, max_iter, power

    type(decision_tree_regressor) :: dt_reg
    type(sadt_regressor) :: sadt_reg
    type(random_rotation_decision_tree_regressor) :: rrdt_reg
    type(data_holder), target :: dholder
    type(data_holder), pointer :: dholder_ptr

    n_samples_trains = (/100_8, 1000_8, 10000_8, 100000_8, 1000000_8/)
    n_columns_trains = (/10_8, 50_8, 100_8, 200_8, 400_8/)
    n_iters = (/1000_8, 100_8, 10_8, 1_8, 1_8/)


    

    open(100, file="timer.csv")
    do iii=1, 5, 1
        print*, '============================================================='
        print*, "Input Data Shape: "
        n_samples_train  = n_samples_trains(iii)
        n_iter = n_iters(iii)
        do jjj=1, 5, 1
            n_columns_train  = n_columns_trains(jjj) 

            skip_header = f_
            dtype_in = "r"
            dtype_out = "r"
            print*, "    train (row x col): ", n_samples_train, n_columns_train
            print*, "    skip header:       ", skip_header
            print*, "    data type input:   ", dtype_in
            print*, "    data type output:  ", dtype_out

            print*, '============================================================='
            print*, "File Names: "
            ! file_name_x_train_csv = "../../../uci_data/97_make_regression/make_regression_x_0000001000x00100.csv"
            ! file_name_y_train_csv = "../../../uci_data/97_make_regression/make_regression_y_0000001000x00100.csv"
            ! file_name_x_train_bin = "../../../uci_data/97_make_regression/make_regression_x_0000001000x00100.bin"
            ! file_name_y_train_bin = "../../../uci_data/97_make_regression/make_regression_y_0000001000x00100.bin"

            write (file_name_x_train_csv, & 
                    '("../../../uci_data/97_make_regression/make_regression_x_", i10.10, "x", i5.5, ".csv")') & 
                    n_samples_train, n_columns_train
            write (file_name_y_train_csv, & 
                    '("../../../uci_data/97_make_regression/make_regression_y_", i10.10, "x", i5.5, ".csv")') & 
                    n_samples_train, n_columns_train
            write (file_name_x_train_bin, & 
                    '("../../../uci_data/97_make_regression/make_regression_x_", i10.10, "x", i5.5, ".bin")') & 
                    n_samples_train, n_columns_train
            write (file_name_y_train_bin, & 
                    '("../../../uci_data/97_make_regression/make_regression_y_", i10.10, "x", i5.5, ".bin")') & 
                    n_samples_train, n_columns_train

            print*, file_name_x_train_csv
            print*, file_name_y_train_csv
            print*, file_name_x_train_bin
            print*, file_name_y_train_bin
            print*, "   x_train csv -> bin: ", trim(file_name_x_train_csv), " -> ", trim(file_name_x_train_bin)
            print*, "   y_train csv -> bin: ", trim(file_name_y_train_csv), " -> ", trim(file_name_y_train_bin)


            ! print*, '============================================================='
            ! print*, "CSV to Binary"
            ! print*, "    x_train"
            ! call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
            !     n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)
            ! print*, "    y_train"
            ! call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
            !     n_samples_train, 1_8, skip_header, dtype_in, dtype_out)
            ! cycle

            print*, '============================================================='
            print*, "Read Binary"
            print*, "    x_train"
            call read_bin_2d(file_name_x_train_bin, x_train)
            print*, "    y_train"
            call read_bin_2d(file_name_y_train_bin, y_train)

            print*, '============================================================='
            print*, "data_holder"
            dholder = data_holder(x_train, y_train, is_trans_x=f_)
            dholder_ptr => dholder

            max_leaf_nodes = 100
            print*, '============================================================='
            print*, "Start Training RandomRotationCART"
            sadt_reg = sadt_regressor(max_leaf_nodes=max_leaf_nodes, fashion="best", & 
                    min_samples_leaf=10_8)
            call date_and_time(values=date_value1)
            do iter=1, 1
                call sadt_reg%fit(dholder_ptr)
            end do
            call date_and_time(values=date_value2)
            y_train_pred_et = sadt_reg%predict(x_train)
            time_et = time_diff(date_value1, date_value2)
            print*, "=============================================================================="
            print*, " ----- DecisionTree, Naive Implementation max_leaf_node=100"
            print*, "TrainMSE: ", real(metric%mean_square_error(y_train(:,1), y_train_pred_et(:,1)))
            print*, "TIme    : ", n_samples_train, n_columns_train, real(time_et), "[msec]", real(time_et)/1, "[msec]"
            write(100, *) "NORMAL", n_samples_train, n_columns_train, real(time_et), "[msec]", real(time_et)/1, "[msec]"

            ! call date_and_time(values=date_value1)
            ! do iter=1, n_iter
            !     call et_reg_fast%fit(dholder_ptr)
            ! end do
            ! call date_and_time(values=date_value2)
            ! y_train_pred_et = et_reg_fast%predict(x_train)
            ! time_et = time_diff(date_value1, date_value2)
            ! print*, "=============================================================================="
            ! print*, " ----- DecisionTree, Naive Implementation max_leaf_node=100"
            ! print*, "TrainMSE: ", real(metric%mean_square_error(y_train(:,1), y_train_pred_et(:,1)))
            ! print*, "TIme_SECOND : ", n_samples_train, n_columns_train, real(time_et), "[msec]", real(time_et)/n_iter, "[msec]"
            ! write(100, *) "NORMAL", n_samples_train, n_columns_train, real(time_et), "[msec]", real(time_et)/n_iter, "[msec]"
            ! ! stop
        end do
    end do



end program main
