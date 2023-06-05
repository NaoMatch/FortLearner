program main_decision_tree_decision_tree
    use :: iso_c_binding
    use mod_const
    use mod_timer
    use mod_decision_tree, only: decision_tree_regressor
    use mod_metric
    use mod_data_holder
    use mod_woodworking_tools
    use mod_common
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(kind=4) :: time_naive, time_branchless, time_branchless_subset, time_branched

    integer(kind=8)    :: n_samples_trains(5), n_columns_trains(5)
    integer(kind=8)    :: n_samples_train, n_columns_train 
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred(:,:), y_train_pred2(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)
    integer(kind=8) :: i, iter, max_iter
    integer(c_int), parameter :: RTLD_LAZY=1

    type(decision_tree_regressor) :: dt, dt2, dt3
    type(metrics)                 :: metric
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr

    integer(kind=8), allocatable :: features(:)
    real(kind=8), allocatable :: thresholds(:)
    real(kind=8), allocatable :: responses(:,:)
    real(kind=8) :: mse_naive, mse_branchless, mse_branchless_subset, mse_branched

    type(node_axis) :: root_node
    integer(kind=8) :: node_idx=0_8, depth=0_8, max_depth=0_8, d
    character(:), allocatable :: fname, cmd


    file_name_x_train_bin = "../sample_data/make_regression_X_train_0000100000x00100.bin"
    file_name_y_train_bin = "../sample_data/make_regression_y_train_0000100000x00100.bin"
    ! file_name_x_train_bin = "../sample_data/make_regression_X_train_0001000000x00100.bin"
    ! file_name_y_train_bin = "../sample_data/make_regression_y_train_0001000000x00100.bin"
    ! file_name_x_train_bin = "../sample_data/diabetes_data_x.bin"
    ! file_name_y_train_bin = "../sample_data/diabetes_data_y.bin"
    file_name_x_train_bin = "../sample_data/linnerud_exercise_x.bin"
    file_name_y_train_bin = "../sample_data/linnerud_exercise_y.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)
    dholder = data_holder(x_train, y_train, is_trans_x=f_)
    dholder_ptr => dholder

    ! Train, Test, Dump -----------------------------------------------------------------
    ! print*, "Train, Test, Dump Trained Model"
    do d=9, 12, 1
        print*, '*********************************************************************************************'
        print*, '*********************************************************************************************'
        print*, '*********************************************************************************************'
        print*, '*********************************************************************************************'
        print*, '*********************************************************************************************'
        dt = decision_tree_regressor(max_depth=d, min_samples_leaf=2_8)
        call dt%fit(dholder_ptr)
        ! call dt%dump(file_name="dt.bin")

        ! Load, Test ------------------------------------------------------------------------
        ! print*, "Load Trained Model, Test"
        ! dt2 = decision_tree_regressor()
        max_iter = 10
        max_iter = 1
        ! call dt2%load(file_name="dt.bin")

        print*, 1
        call date_and_time(values=date_value1)
        do iter=1, max_iter, 1
            y_train_pred = dt%predict(x_train)
        end do
        call date_and_time(values=date_value2)
        time_naive = time_diff(date_value1, date_value2)
        mse_naive = metric%mean_square_error(y_train(:,1), y_train_pred(:,1))

        print*, 2
        call dt%branched_jit()
        call date_and_time(values=date_value1)
        do iter=1, max_iter, 1
            y_train_pred = dt%predict(x_train)
        end do
        call date_and_time(values=date_value2)
        time_branched = time_diff(date_value1, date_value2)
        mse_branched = metric%mean_square_error(y_train(:,1), y_train_pred(:,1))
                
        print*, 3
        call dt%branchless_jit()
        call date_and_time(values=date_value1)
        do iter=1, max_iter, 1
            y_train_pred = dt%predict(x_train)
        end do
        call date_and_time(values=date_value2)
        time_branchless = time_diff(date_value1, date_value2)
        mse_branchless = metric%mean_square_error(y_train(:,1), y_train_pred(:,1))

        print*, d, &
                " :: ", time_naive, time_branched, time_branchless, &
                " :: ", mse_naive, mse_branched, mse_branchless
    end do

        
    ! call dt2%branched_jit()
    ! call date_and_time(values=date_value1)
    ! do iter=1, max_iter, 1
    !     y_train_pred = dt2%predict(x_train)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "BranchedJIT:              ", &
    !     metric%mean_square_error(y_train(:,1), y_train_pred(:,1)), time_diff(date_value1, date_value2)
    !     dt2%proc_addr_branched = C_NULL_FUNPTR


    stop


! print*, '*********************************************************************************************'    
!     call dt2%branchless_jit()
!     call date_and_time(values=date_value1)
!     do iter=1, max_iter, 1
!         y_train_pred = dt2%predict(x_train)
!     end do
!     call date_and_time(values=date_value2)
!     print*, "BranchlessJIT:     ", metric%mean_square_error(y_train(:,1), y_train_pred(:,1)), time_diff(date_value1, date_value2)


! print*, '*********************************************************************************************'    
!     call dt2%branched_batch_jit()
!     call date_and_time(values=date_value1)
!     do iter=1, max_iter, 1
!         y_train_pred = dt2%predict(x_train)
!     end do
!     call date_and_time(values=date_value2)
!     print*, "BranchedJIT Batch: ", metric%mean_square_error(y_train(:,1), y_train_pred(:,1)), time_diff(date_value1, date_value2)
    
contains



end program main_decision_tree_decision_tree
