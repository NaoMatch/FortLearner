program main
    use mod_timer
    use mod_extra_tree
    use mod_data_holder
    use mod_common
    use mod_metric
    implicit none
    integer(kind=8)        :: date_value1(8), date_value2(8)
    type(metrics)                 :: metric

    type(extra_tree_regressor) :: etree, etree_fast, etree_fast_new, etree_fast_rep_para
    type(data_holder), target :: dholder, dholder_t
    type(data_holder), pointer :: dholder_ptr, dholder_t_ptr
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin

    integer(kind=8) :: max_leaf_nodes, n_repeats, min_samples_leaf, n_threads, max_depth

    integer(kind=8) :: n_samples, n_columns, shape_x(2)
    real(kind=8), allocatable :: x(:,:), x_t(:,:)
    real(kind=8), allocatable :: y(:,:)
    real(kind=8), allocatable :: x_train(:,:), x_train_t(:,:)
    real(kind=8), allocatable :: y_train(:,:)
    real(kind=8), ALLOCATABLE :: y_pred(:,:)

    file_name_x_train_bin = "../../sample_data/make_regression_X_train_0001000000x00100.bin"
    file_name_y_train_bin = "../../sample_data/make_regression_y_train_0001000000x00100.bin"
    print*, '============================================================='
    print*, "Read Binary"
    print*, "    x_train"
    call read_bin_2d(file_name_x_train_bin, x_train)
    print*, "    y_train"
    call read_bin_2d(file_name_y_train_bin, y_train)    

    n_samples = 1000000
    n_columns = 256
    ! allocate(x(n_samples, n_columns))
    ! allocate(x_t(n_columns, n_samples))
    ! allocate(y(n_samples, 1))
    ! call random_number(x)
    ! call random_number(y)
    ! x_t = transpose(x)
    ! allocate(x_t, source=transpose(x))
    shape_x = shape(x_train)
    allocate(x_train_t(shape_x(2), shape_x(1)))
    x_train_t = transpose(x_train)

    dholder = data_holder(x_train, y_train, is_trans_x=.false.)
    dholder_t = data_holder(x_train_t, y_train, is_trans_x=.true.)
    dholder_ptr => dholder
    dholder_t_ptr => dholder_t

    max_depth = 6
    max_leaf_nodes = 128
    n_repeats = 4
    min_samples_leaf = 10_8
    n_threads = 4

    ! etree = extra_tree_regressor(max_leaf_nodes=max_leaf_nodes, fashion="best", n_repeats=n_repeats, & 
    !     min_samples_leaf=min_samples_leaf, n_threads=n_threads)
    ! etree_fast = extra_tree_regressor(max_leaf_nodes=max_leaf_nodes, fashion="best", n_repeats=n_repeats, & 
    !     min_samples_leaf=min_samples_leaf, n_threads=n_threads)
    ! etree_fast_new = extra_tree_regressor(max_leaf_nodes=max_leaf_nodes, fashion="best", n_repeats=n_repeats, & 
    !     min_samples_leaf=min_samples_leaf, n_threads=n_threads)
    ! etree_fast_rep_para = extra_tree_regressor(max_leaf_nodes=max_leaf_nodes, fashion="best", n_repeats=n_repeats, & 
    !     min_samples_leaf=min_samples_leaf, n_threads=n_threads)

    etree = extra_tree_regressor(max_depth=max_depth, fashion="depth", n_repeats=n_repeats, & 
        min_samples_leaf=min_samples_leaf, n_threads=n_threads)
    etree_fast = extra_tree_regressor(max_depth=max_depth, fashion="depth", n_repeats=n_repeats, & 
        min_samples_leaf=min_samples_leaf, n_threads=n_threads)
    etree_fast_new = extra_tree_regressor(max_depth=max_depth, fashion="depth", n_repeats=n_repeats, & 
        min_samples_leaf=min_samples_leaf, n_threads=n_threads)
    etree_fast_rep_para = extra_tree_regressor(max_depth=max_depth, fashion="depth", n_repeats=n_repeats, & 
        min_samples_leaf=min_samples_leaf, n_threads=n_threads)

    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    call etree%fit(dholder_ptr)
    call date_and_time(values=date_value2)
    y_pred = etree%predict(x_train)
    print*, "etree%fit(dholder_ptr): ", time_diff(date_value1, date_value2), &
        real(metric%mean_square_error(y_train(:,1), y_pred(:,1)))
    
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    call etree_fast%fit_faster(dholder_t_ptr)
    call date_and_time(values=date_value2)
    y_pred = etree_fast%predict(x_train)
    print*, "etree_fast%fit_faster(dholder_t_ptr): ", time_diff(date_value1, date_value2), &
        real(metric%mean_square_error(y_train(:,1), y_pred(:,1)))
    
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    call etree_fast_new%fit_faster_new(dholder_t_ptr)
    call date_and_time(values=date_value2)
    y_pred = etree_fast_new%predict(x_train)
    print*, "etree_fast_new%fit_faster_new(dholder_t_ptr): ", time_diff(date_value1, date_value2), &
        real(metric%mean_square_error(y_train(:,1), y_pred(:,1)))
    
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    call date_and_time(values=date_value1)
    call etree_fast_rep_para%fit_faster_new_rep_para(dholder_t_ptr)
    call date_and_time(values=date_value2)
    y_pred = etree_fast_rep_para%predict(x_train)
    print*, "etree_fast_new_%fit_faster_new(dholder_t_ptr): ", time_diff(date_value1, date_value2), &
        real(metric%mean_square_error(y_train(:,1), y_pred(:,1)))
        

end program main