program main_neighbour_k_nearest_neighbor_regressor
    use mod_const
    use mod_timer
    use mod_common
    use mod_k_nearest_neighbor_regressor
    use mod_metric
    use mod_data_holder
    implicit none

    type(k_nearest_neighbor_regressor) :: knn_reg

    integer(kind=8)        :: date_value1(8), date_value2(8)
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:), x_test(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE :: pred_train(:,:), pred_test(:,:)
    type(metrics) :: metric
    type(data_holder) :: dholder

    character(:), allocatable :: dstructures(:), weights(:), kernels(:)
    character(:), allocatable :: dstruct, weight, krnl
    integer(kind=8) :: d, w, k, n_test


    file_name_x_train_bin = "../sample_data/make_regression_X_train_0000010000x00100.bin"
    file_name_y_train_bin = "../sample_data/make_regression_y_train_0000010000x00100.bin"
    call read_bin_2d(file_name_x_train_bin, x_train, print_log=f_)
    call read_bin_2d(file_name_y_train_bin, y_train, print_log=f_)

    file_name_x_test_bin = "../sample_data/make_regression_X_test_0000010000x00100.bin"
    file_name_y_test_bin = "../sample_data/make_regression_y_test_0000010000x00100.bin"
    call read_bin_2d(file_name_x_test_bin, x_test, print_log=f_)
    call read_bin_2d(file_name_y_test_bin, y_test, print_log=f_)
    dholder = data_holder(x_train, y_train, is_trans_x=f_)

    dstructures = [ &
        "brute_force", &
        "kd_tree    ", &
        "ball_tree  "  &
    ]

    weights = [&
        "uniform ", &
        "distance" &
    ]

    kernels = [&
        "linear      ", &
        "exponential ", &
        "epanechnikov", &
        "tricubic    "  &
    ]

    n_test = size(x_test, dim=1)
    do d=1, size(dstructures), 1
        dstruct = dstructures(d)
        do w=1, size(weights), 1
            weight = weights(w)
            do k=1, size(kernels), 1
                krnl = kernels(k)
                if ((weight == "uniform") .and. (krnl /= "linear")) cycle
                knn_reg = k_nearest_neighbor_regressor(&
                    algorithm=dstruct, weight=weight, kernel=krnl, &
                    min_samples_in_leaf=256_8)

                print*, "D: " // dstruct // ",  W: " // weight //  ",  K: " // krnl
                call knn_reg%fit(x_train, y_train)
                pred_train = knn_reg%predict(x_train(:,:))
                pred_test = knn_reg%predict(x_test(:,:))
                pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
                pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
                print*, metric%mean_square_error(y_train(:,1), pred_train(:,1)), &
                    metric%mean_square_error(y_test(:,1),  pred_test(:,1))
                call knn_reg%fit(dholder)
                pred_train = knn_reg%predict(x_train(:,:))
                pred_test = knn_reg%predict(x_test(:,:))
                pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
                pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
                print*, metric%mean_square_error(y_train(:,1), pred_train(:,1)), &
                    metric%mean_square_error(y_test(:,1),  pred_test(:,1))
            end do
        end do
    end do
    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** brute_force, weight=uniform"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="brute_force")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))

    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** kd_tree, weight=uniform"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="kd_tree")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))

    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** ball_tree, weight=uniform"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="ball_tree", split_algo="most_spread")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))

    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** brute_force, weight=distance"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="brute_force", weight="distance")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))
    
    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** kd_tree, weight=distance"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="kd_tree", weight="distance")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))
    
    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** ball_tree, weight=distance"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="ball_tree", split_algo="most_spread", weight="distance")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))

    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** brute_force, weight=distance, kernel=exponential"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="brute_force", weight="distance", &
    !             kernel="exponential")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))
    
    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** kd_tree, weight=distance, kernel=exponential"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="kd_tree", weight="distance", &
    !             kernel="exponential")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))
    
    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** ball_tree, weight=distance, kernel=exponential"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="ball_tree", split_algo="most_spread", weight="distance", &
    !             kernel="exponential")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))

    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** brute_force, weight=distance, kernel=epanechnikov"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="brute_force", weight="distance", &
    !             kernel="epanechnikov")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))
    
    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** kd_tree, weight=distance, kernel=epanechnikov"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="kd_tree", weight="distance", &
    !             kernel="epanechnikov")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))
    
    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** ball_tree, weight=distance, kernel=epanechnikov"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="ball_tree", split_algo="most_spread", weight="distance", &
    !             kernel="epanechnikov")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))

    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** brute_force, weight=distance, kernel=tricubic"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="brute_force", weight="distance", &
    !             kernel="tricubic")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))
    
    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** kd_tree, weight=distance, kernel=tricubic"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="kd_tree", weight="distance", &
    !             kernel="tricubic")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))
    
    ! print*, "  "
    ! print*, "  "
    ! print*, " **************** ball_tree, weight=distance, kernel=tricubic"
    ! knn_reg = k_nearest_neighbor_regressor(algorithm="ball_tree", split_algo="most_spread", weight="distance", &
    !             kernel="tricubic")
    ! call knn_reg%fit(x_train, y_train)
    ! pred_train = knn_reg%predict(x_train)
    ! pred_test = knn_reg%predict(x_test)
    ! print*, metric%mean_square_error(y_train(:,1), pred_train(:,1))
    ! print*, metric%mean_square_error(y_test(:,1), pred_test(:,1))
    ! stop

end program main_neighbour_k_nearest_neighbor_regressor