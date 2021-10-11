program main
    use mod_const
    use mod_random
    use mod_timer
    use mod_stats
    use mod_metric
    use mod_scaler
    use mod_data_holder
    use mod_base_tree
    use mod_isolation_tree
    use mod_isolation_forest
    implicit none

    integer(kind=8)              :: date_value1(8), date_value2(8), time_sadt
    integer(kind=8), allocatable :: times(:)
    integer(kind=8), allocatable :: n_samples_trains(:), n_columns_trains(:), n_iters(:)
    integer(kind=8)              :: n_samples_train, n_columns_train 
    integer(kind=8)              :: n_samples_test, n_columns_test
    integer(kind=8)              :: n_trees, n_check
    integer(kind=8)              :: iii, jjj, n_iter, iter, idx
    logical(kind=4)              :: skip_header
    CHARACTER(len=1)             :: x_dtype_in, x_dtype_out
    CHARACTER(len=1)             :: y_dtype_in, y_dtype_out
    CHARACTER(len=256)           :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256)           :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256)           :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256)           :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE    :: x_train(:,:), x_test(:,:)
    real(kind=8), ALLOCATABLE    :: x_train_t(:,:), x_test_t(:,:)
    integer(kind=8), ALLOCATABLE    :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE    :: y_pred_train_score(:,:), y_pred_test_score(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_dt(:,:), y_test_pred_dt(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_et(:,:), y_test_pred_et(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_cl(:,:), y_test_pred_cl(:,:)
    real(kind=8), ALLOCATABLE    :: y_train_pred_lw(:,:), y_test_pred_lw(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:), indices(:)


    real(kind=8)  :: time_mean, time_var
    real(kind=8)  :: auc
    type(metrics) :: metric
    type(minmax_scaler) :: mm
    type(data_holder), target :: dholder
    type(data_holder), pointer :: dholder_ptr
    type(isolation_tree) :: itree
    type(isolation_forest) :: iforest

    file_name_x_train_csv = "../../../uci_data/kdd_dataset_x_train_enc.data"
    file_name_x_train_bin = "../../../uci_data/kdd_dataset_x_train_enc.bin"
    file_name_y_train_csv = "../../../uci_data/kdd_dataset_y_train_enc.data"
    file_name_y_train_bin = "../../../uci_data/kdd_dataset_y_train_enc.bin"
    n_samples_train = 120000
    n_columns_train = 40
    file_name_x_test_csv = "../../../uci_data/kdd_dataset_x_test_enc.data"
    file_name_x_test_bin = "../../../uci_data/kdd_dataset_x_test_enc.bin"
    file_name_y_test_csv = "../../../uci_data/kdd_dataset_y_test_enc.data"
    file_name_y_test_bin = "../../../uci_data/kdd_dataset_y_test_enc.bin"
    n_samples_test = 28517
    n_columns_test = 40
    x_dtype_in  = "r"
    x_dtype_out = "r"
    y_dtype_in  = "i"
    y_dtype_out = "i"
    skip_header = t_

    ! file_name_x_train_csv = "../../../uci_data/covtype_x_train.data"
    ! file_name_x_train_bin = "../../../uci_data/covtype_x_train.bin"
    ! file_name_y_train_csv = "../../../uci_data/covtype_y_train.data"
    ! file_name_y_train_bin = "../../../uci_data/covtype_y_train.bin"
    ! n_samples_train = 240000
    ! n_columns_train = 10
    ! file_name_x_test_csv = "../../../uci_data/covtype_x_test.data"
    ! file_name_x_test_bin = "../../../uci_data/covtype_x_test.bin"
    ! file_name_y_test_csv = "../../../uci_data/covtype_y_test.data"
    ! file_name_y_test_bin = "../../../uci_data/covtype_y_test.bin"
    ! n_samples_test = 46048
    ! n_columns_test = 10
    ! x_dtype_in  = "r"
    ! x_dtype_out = "r"
    ! y_dtype_in  = "i"
    ! y_dtype_out = "i"
    ! skip_header = t_

    print*, '============================================================='
    print*, "CSV to Binary"
    print*, "    x_train & y_train"
    call read2bin_2d(file_name_x_train_csv, file_name_x_train_bin, &
        n_samples_train, n_columns_train, skip_header, x_dtype_in, x_dtype_out)
    call read2bin_2d(file_name_y_train_csv, file_name_y_train_bin, &
        n_samples_train, 1_8, skip_header, y_dtype_in, y_dtype_out)
    print*, "    x_test & y_test"
    call read2bin_2d(file_name_x_test_csv, file_name_x_test_bin, &
        n_samples_test, n_columns_test, skip_header, x_dtype_in, x_dtype_out)
    call read2bin_2d(file_name_y_test_csv, file_name_y_test_bin, &
        n_samples_test, 1_8, skip_header, y_dtype_in, y_dtype_out)

    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train & y_train"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)
    print*, "    x_test & y_test"
    call read_bin_2d(file_name_x_test_bin, x_test)
    call read_bin_2d(file_name_y_test_bin, y_test)

    dholder = data_holder(x_train, y_train)
    dholder_ptr => dholder

    n_check = 100
    n_trees = 1000
    n_iter = 100
    allocate(indices(n_samples_train))
    allocate(y_pred_train_score(n_samples_train,1))
    allocate(y_pred_test_score(n_samples_test,1))
    allocate(times(n_iter))

    times=0
    do iter=1, n_iter, 1
        iforest = isolation_forest(n_estimators=n_trees, max_samples=256_8)
        call date_and_time(values=date_value1)
        call iforest%fit(dholder_ptr)
        call date_and_time(values=date_value2)
        times(iter) = time_diff(date_value1, date_value2)
    end do
    time_mean = mean(times,n_iter)
    time_var  = sqrt(variance(times, n_iter))
    print*, "Train Time: ", time_mean, time_var 

    call date_and_time(values=date_value1)
    print*, "compute auc"
    y_pred_train_score = iforest%predict(x_train)
    auc = metric%auc(y_train(:,1), y_pred_train_score(:,1))
    print*, "auc_train: ", auc
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    y_pred_test_score = iforest%predict(x_test)
    auc = metric%auc(y_test(:,1), y_pred_test_score(:,1))
    print*, "auc_test : ", auc
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)



end program main
