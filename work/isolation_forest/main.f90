program main
    use mod_const
    use mod_random
    use mod_timer
    use mod_metric
    use mod_scaler
    use mod_data_holder
    use mod_base_tree
    use mod_isolation_tree
    use mod_isolation_forest
    implicit none

    integer(kind=8)              :: date_value1(8), date_value2(8), time_sadt
    integer(kind=8), allocatable :: n_samples_trains(:), n_columns_trains(:), n_iters(:)
    integer(kind=8)              :: n_samples_train, n_columns_train 
    integer(kind=8)              :: n_samples_test, n_columns_test
    integer(kind=8)              :: n_trees, n_check
    integer(kind=8)              :: iii, jjj, n_iter, iter, idx
    logical(kind=4)              :: skip_header
    CHARACTER(len=1)             :: dtype_in, dtype_out
    CHARACTER(len=256)           :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256)           :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256)           :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256)           :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE    :: x_train(:,:), x_test(:,:)
    real(kind=8), ALLOCATABLE    :: x_train_t(:,:), x_test_t(:,:)
    integer(kind=8), ALLOCATABLE    :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE    :: y_pred_depth(:,:), y_pred_score(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_dt(:,:), y_test_pred_dt(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_et(:,:), y_test_pred_et(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_cl(:,:), y_test_pred_cl(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_lw(:,:), y_test_pred_lw(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:), indices(:)


    real(kind=8)  :: auc
    type(metrics) :: metric
    type(minmax_scaler) :: mm
    type(data_holder), target :: dholder
    type(data_holder), pointer :: dholder_ptr
    type(isolation_tree) :: itree
    type(isolation_forest) :: iforest

    file_name_x_train_csv = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_x_train.txt"
    file_name_x_train_bin = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_x_train.bin"
    file_name_y_train_csv = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_y_train.txt"
    file_name_y_train_bin = "../../../uci_data/05_YearPredictionMSD/YearPredictionMSD_y_train.bin"
    n_samples_train = 412206
    n_columns_train = 90
    dtype_in = "r"
    dtype_out = "r"
    skip_header = f_

    ! file_name_x_train_csv = "../../../uci_data/test_anomaly_detection_x.csv"
    ! file_name_x_train_bin = "../../../uci_data/test_anomaly_detection_x.bin"
    ! file_name_y_train_csv = "../../../uci_data/test_anomaly_detection_y.csv"
    ! file_name_y_train_bin = "../../../uci_data/test_anomaly_detection_y.bin"
    ! n_samples_train = 210
    ! n_columns_train = 2
    ! dtype_in = "r"
    ! dtype_out = "r"
    ! skip_header = t_

    ! print*, '============================================================='
    ! print*, "CSV to Binary"
    ! print*, "    x_train"
    ! call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
    !     n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)
    ! call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
    !     n_samples_train, 1_8, skip_header, "i", "i")

    file_name_x_train_csv = "../../../kdd_dataset_x_enc.csv"
    file_name_x_train_bin = "../../../kdd_dataset_x_enc.bin"
    file_name_y_train_csv = "../../../kdd_dataset_y_enc.csv"
    file_name_y_train_bin = "../../../kdd_dataset_y_enc.bin"
    n_samples_train = 148517
    ! n_samples_train = 10000
    n_columns_train = 40
    dtype_in = "r"
    dtype_out = "r"
    skip_header = t_

    print*, '============================================================='
    print*, "CSV to Binary"
    print*, "    x_train"
    call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
        n_samples_train, n_columns_train, skip_header, dtype_in, dtype_out)
    call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
        n_samples_train, 1_8, skip_header, "i", "i")

    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)

    dholder = data_holder(x_train, y_train)
    dholder_ptr => dholder

    itree = isolation_tree(max_samples=256_8)

    ! call date_and_time(values=date_value1)
    ! call itree%fit(dholder_ptr)
    ! call date_and_time(values=date_value2)
    ! print*, time_diff(date_value1, date_value2)

    ! call date_and_time(values=date_value1)
    ! y_train = itree%predict(x_train)
    ! call date_and_time(values=date_value2)
    ! print*, time_diff(date_value1, date_value2)


    n_check = 100
    n_trees = 100
    allocate(indices(n_samples_train))
    allocate(y_pred_depth(n_samples_train,1))
    allocate(y_pred_score(n_samples_train,1))
    y_pred_depth = 0d0
    do iii=1, n_samples_train, 1
        indices(iii) = iii
    end do
    ! y_train = 0d0
    itree = isolation_tree(max_samples=1024_8)
    do iii=1, n_trees, 1
        call itree%fit(dholder_ptr, print_node=f_)
        y_pred_depth = y_pred_depth + itree%predict(x_train, return_depth=t_)
    end do
    y_pred_depth = y_pred_depth / dble(n_trees)
    y_pred_score(:,1) = 2d0**(-y_pred_depth(:,1)/avg_depth(n_samples_train))

    print*, "compute auc"
    auc = metric%auc(y_train(:,1), y_pred_score(:,1))
    print*, "auc: ", auc

    iforest = isolation_forest(n_estimators=n_trees, max_samples=1024_8)
    call iforest%fit(dholder_ptr)
    y_pred_score = iforest%predict(x_train)


    print*, "compute auc"
    auc = metric%auc(y_train(:,1), y_pred_score(:,1))
    print*, "auc: ", auc



end program main
