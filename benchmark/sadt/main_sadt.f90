program main_sadt
    use mod_const
    use mod_random
    use mod_timer
    use mod_metric
    use mod_data_holder
    use mod_decision_tree
    use mod_extra_tree
    use mod_sadt
    use mod_scaler
    implicit none
    

    integer(kind=8)              :: date_value1(8), date_value2(8), time_sadt
    integer(kind=8), allocatable :: n_samples_trains(:), n_columns_trains(:), n_iters(:)
    integer(kind=8)              :: n_samples_train, n_columns_train 
    integer(kind=8)              :: n_samples_test, n_columns_test
    integer(kind=8)              :: iii, jjj, n_iter, iter
    logical(kind=4)              :: skip_header
    CHARACTER(len=1)             :: dtype_in, dtype_out
    CHARACTER(len=256)           :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256)           :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256)           :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256)           :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE    :: x_train(:,:), x_test(:,:)
    real(kind=8), ALLOCATABLE    :: x_train_t(:,:), x_test_t(:,:)
    real(kind=8), ALLOCATABLE    :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_dt(:,:), y_test_pred_dt(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_et(:,:), y_test_pred_et(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_cl(:,:), y_test_pred_cl(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_lw(:,:), y_test_pred_lw(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)

    type(metrics)                 :: metric
    type(data_holder), target     :: dholder, dholder_t
    type(data_holder), pointer    :: dholder_ptr, dholder_t_ptr
    type(sadt_regressor)          :: sadt_reg
    type(decision_tree_regressor) :: dt_reg
    type(extra_tree_regressor)    :: et_reg
    integer(kind=8)               :: max_leaf_nodes
    type(minmax_scaler)           :: mm_scaler

    n_samples_trains = (/100_8, 1000_8, 10000_8, 100000_8, 1000000_8/)
    n_columns_trains = (/10_8, 50_8, 100_8, 200_8, 400_8/)
    n_iters          = (/100_8, 100_8, 10_8, 5_8, 5_8/)
    n_samples_trains = (/10000_8, 1000_8, 10000_8, 100000_8, 1000000_8/)
    n_columns_trains = (/50_8, 50_8, 100_8, 200_8, 400_8/)
    n_iters = 10
    n_iters = 1
    max_leaf_nodes = 64

    ! n_samples_trains = n_samples_trains(size(n_samples_trains):1:-1)
    ! n_columns_trains = n_columns_trains(size(n_columns_trains):1:-1)
    ! n_iters          = n_iters(size(n_iters):1:-1)
    mm_scaler = minmax_scaler(min_val=-1d0, max_val=1d0)

    open(100, file="timer.csv")
    do iii=1, size(n_samples_trains), 1
        print*, '============================================================='
        print*, "Input Data Shape: "
        n_samples_train  = n_samples_trains(iii)
        n_samples_train = 412206
        n_iter = n_iters(iii)
        ! n_iter = 1_8
        allocate(y_train_pred_et(n_samples_train,1))
        do jjj=1, size(n_columns_trains), 1
            n_columns_train  = n_columns_trains(jjj) 
            n_columns_train = 90
            skip_header = f_
            dtype_in = "r"
            dtype_out = "r"
            print*, "    train (row x col): ", n_samples_train, n_columns_train
            print*, "    skip header:       ", skip_header
            print*, "    data type input:   ", dtype_in
            print*, "    data type output:  ", dtype_out

            print*, '============================================================='
            print*, "File Names: "
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

            file_name_x_train_csv = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_x_train.txt"
            file_name_y_train_csv = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_y_train.txt"
            file_name_x_train_bin = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_x_train.bin"
            file_name_y_train_bin = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_y_train.bin"
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
            !     n_samples_train, 1_8, skip_header, "i", dtype_out)

            print*, '============================================================='
            print*, "Read Binary"
            print*, "    x_train"
            call read_bin_2d(file_name_x_train_bin, x_train)
            print*, "    y_train"
            call read_bin_2d(file_name_y_train_bin, y_train)

            print*, '============================================================='
            print*, "Scaling"
            call mm_scaler%fit(x_train)
            x_train = mm_scaler%transform(x_train)
            call mm_scaler%fit(y_train)
            y_train = mm_scaler%transform(y_train)

            print*, '============================================================='
            print*, "data_holder"
            allocate(x_train_t(n_columns_train, n_samples_train))
            x_train_t = transpose(x_train)
            dholder   = data_holder(x_train, y_train, is_trans_x=f_)
            dholder_t = data_holder(x_train_t, y_train, is_trans_x=t_)
            dholder_ptr => dholder
            dholder_t_ptr => dholder_t

            print*, '============================================================='
            print*, "Start Training SADT_Regressor"
            sadt_reg = sadt_regressor(max_leaf_nodes=max_leaf_nodes, fashion="best", min_samples_leaf=1_8, &
                cooling_rate=0.999d0, max_epoch=800_8)
            call date_and_time(values=date_value1)
            do iter=1, n_iter
                call sadt_reg%fit(dholder_ptr, print_node=f_)
            end do
            call date_and_time(values=date_value2)
            y_train_pred_et = sadt_reg%predict(x_train)
            time_sadt = time_diff(date_value1, date_value2)
            print*, "=============================================================================="
            print*, " ----- SADT_Regressor, Naive Implementation max_leaf_node=100"
            print*, "TrainMSE: ", real(metric%mean_square_error(y_train(:,1), y_train_pred_et(:,1)))
            print*, "TIme    : ", n_samples_train, n_columns_train, real(time_sadt), "[msec]", real(time_sadt)/n_iter, "[msec]"
            ! write(100, *) "NORMAL", n_samples_train, n_columns_train, real(time_sadt), "[msec]", real(time_sadt)/n_iter, "[msec]"


            print*, '============================================================='
            print*, "Start Training decision_tree_Regressor"
            dt_reg = decision_tree_regressor(max_leaf_nodes=max_leaf_nodes, fashion="best", min_samples_leaf=1_8, & 
                max_features=45_8, boot_strap=f_)
            call date_and_time(values=date_value1)
            y_train_pred_et = 0d0
            do iter=1, n_iter
                call dt_reg%fit(dholder_ptr, print_node=f_)
                y_train_pred_et = y_train_pred_et + dt_reg%predict(x_train)
            end do
            y_train_pred_et = y_train_pred_et / dble(n_iter)
            call date_and_time(values=date_value2)
            ! y_train_pred_et = dt_reg%predict(x_train)
            time_sadt = time_diff(date_value1, date_value2)
            print*, "=============================================================================="
            print*, " ----- decision_tree_Regressor, Naive Implementation max_leaf_node=100"
            print*, "TrainMSE: ", real(metric%mean_square_error(y_train(:,1), y_train_pred_et(:,1)))
            print*, "TIme    : ", n_samples_train, n_columns_train, real(time_sadt), "[msec]", real(time_sadt)/n_iter, "[msec]"
            ! write(100, *) "NORMAL", n_samples_train, n_columns_train, real(time_sadt), "[msec]", real(time_sadt)/n_iter, "[msec]"

            print*, '============================================================='
            print*, "Start Training extra_tree_regressor"
            et_reg = extra_tree_regressor(max_leaf_nodes=max_leaf_nodes, fashion="best", min_samples_leaf=1_8, n_repeats=1_8)
            call date_and_time(values=date_value1)
            y_train_pred_et = 0d0
            do iter=1, n_iter
                call et_reg%fit(dholder_ptr, print_node=f_)
                y_train_pred_et = y_train_pred_et + et_reg%predict(x_train)
            end do
            y_train_pred_et = y_train_pred_et / dble(n_iter)
            call date_and_time(values=date_value2)
            ! y_train_pred_et = et_reg%predict(x_train)
            time_sadt = time_diff(date_value1, date_value2)
            print*, "=============================================================================="
            print*, " ----- extra_tree_regressor, Naive Implementation max_leaf_node=100"
            print*, "TrainMSE: ", real(metric%mean_square_error(y_train(:,1), y_train_pred_et(:,1)))
            print*, "TIme    : ", n_samples_train, n_columns_train, real(time_sadt), "[msec]", real(time_sadt)/n_iter, "[msec]"
            ! write(100, *) "NORMAL", n_samples_train, n_columns_train, real(time_sadt), "[msec]", real(time_sadt)/n_iter, "[msec]"

            print*, '============================================================='
            print*, "Start Training extra_tree_regressor"
            et_reg = extra_tree_regressor(max_leaf_nodes=max_leaf_nodes, fashion="best", min_samples_leaf=1_8, n_repeats=1_8, & 
                n_threads=4_8)
            call date_and_time(values=date_value1)
            y_train_pred_et = 0d0
            do iter=1, n_iter
                call et_reg%fit_faster(dholder_t_ptr, print_node=f_)
                y_train_pred_et = y_train_pred_et + et_reg%predict(x_train)
            end do
            y_train_pred_et = y_train_pred_et / dble(n_iter)
            call date_and_time(values=date_value2)
            ! y_train_pred_et = et_reg%predict(x_train)
            time_sadt = time_diff(date_value1, date_value2)
            print*, "=============================================================================="
            print*, " ----- extra_tree_regressor, Naive Implementation max_leaf_node=100"
            print*, "TrainMSE: ", real(metric%mean_square_error(y_train(:,1), y_train_pred_et(:,1)))
            print*, "TIme    : ", n_samples_train, n_columns_train, real(time_sadt), "[msec]", real(time_sadt)/n_iter, "[msec]"
            ! write(100, *) "NORMAL", n_samples_train, n_columns_train, real(time_sadt), "[msec]", real(time_sadt)/n_iter, "[msec]"


            deallocate(x_train_t)
            stop
        end do
        deallocate(y_train_pred_et)
    end do






end program main_sadt
