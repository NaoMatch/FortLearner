program main_neighbour_k_nearest_neighbor
    use mod_const
    use mod_timer
    use mod_common
    use mod_k_nearest_neighbor_classifier
    use mod_metric
    use mod_encoder
    implicit none

    type(k_nearest_neighbor_classifier) :: knn_clf

    integer(kind=8)        :: date_value1(8), date_value2(8), n_test
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:), x_test(:,:)
    integer(kind=8), ALLOCATABLE :: y_train(:,:), y_test(:,:)
    integer(kind=8), ALLOCATABLE :: y_oh_train(:,:), y_oh_test(:,:)
    integer(kind=8), ALLOCATABLE :: pred_train(:,:), pred_test(:,:)

    type(metrics) :: metric
    type(one_hot_encoder) :: oh_enc

    file_name_x_train_bin = "../sample_data/make_classification_X_train_0000010000x00010_class=002.bin"
    file_name_y_train_bin = "../sample_data/make_classification_y_train_0000010000x00010_class=002.bin"
    call read_bin_2d(file_name_x_train_bin, x_train, print_log=f_)
    call read_bin_2d(file_name_y_train_bin, y_train, print_log=f_); y_train=y_train+1

    file_name_x_test_bin = "../sample_data/make_classification_X_test_0000010000x00010_class=002.bin"
    file_name_y_test_bin = "../sample_data/make_classification_y_test_0000010000x00010_class=002.bin"
    call read_bin_2d(file_name_x_test_bin, x_test, print_log=f_)
    call read_bin_2d(file_name_y_test_bin, y_test, print_log=f_); y_test=y_test+1

    oh_enc = one_hot_encoder()
    y_oh_train = oh_enc%transform(y_train)
    n_test = size(y_test, dim=1)




    print*, "  "
    print*, "  "
    print*, " **************** brute_force, weight=uniform"
    knn_clf = k_nearest_neighbor_classifier(algorithm="brute_force", weight="uniform", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

    print*, "  "
    print*, "  "
    print*, " **************** kd_tree, weight=uniform"
    knn_clf = k_nearest_neighbor_classifier(algorithm="kd_tree", weight="uniform", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

    print*, "  "
    print*, "  "
    print*, " **************** ball_tree, weight=uniform"
    knn_clf = k_nearest_neighbor_classifier(algorithm="ball_tree", weight="uniform", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1), pred_test(:n_test,1))


    print*, "  "
    print*, "  "
    print*, " **************** brute_force, weight=distance"
    knn_clf = k_nearest_neighbor_classifier(algorithm="brute_force", weight="distance", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

    print*, "  "
    print*, "  "
    print*, " **************** kd_tree, weight=distance"
    knn_clf = k_nearest_neighbor_classifier(algorithm="kd_tree", weight="distance", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

    print*, "  "
    print*, "  "
    print*, " **************** ball_tree, weight=distance"
    knn_clf = k_nearest_neighbor_classifier(algorithm="ball_tree", weight="distance", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))


    print*, "  "
    print*, "  "
    print*, " **************** brute_force, weight=distance, kernel=exponential"
    knn_clf = k_nearest_neighbor_classifier(algorithm="brute_force", weight="distance", &
            kernel="exponential", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

    print*, "  "
    print*, "  "
    print*, " **************** kd_tree, weight=distance, kernel=exponential"
    knn_clf = k_nearest_neighbor_classifier(algorithm="kd_tree", weight="distance", &
            kernel="exponential", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

    print*, "  "
    print*, "  "
    print*, " **************** ball_tree, weight=distance, kernel=exponential"
    knn_clf = k_nearest_neighbor_classifier(algorithm="ball_tree", weight="distance", &
            kernel="exponential", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))


    print*, "  "
    print*, "  "
    print*, " **************** brute_force, weight=distance, kernel=epanechnikov"
    knn_clf = k_nearest_neighbor_classifier(algorithm="brute_force", weight="distance", &
            kernel="epanechnikov", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

    print*, "  "
    print*, "  "
    print*, " **************** kd_tree, weight=distance, kernel=epanechnikov"
    knn_clf = k_nearest_neighbor_classifier(algorithm="kd_tree", weight="distance", &
            kernel="epanechnikov", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

    print*, "  "
    print*, "  "
    print*, " **************** ball_tree, weight=distance, kernel=epanechnikov"
    knn_clf = k_nearest_neighbor_classifier(algorithm="ball_tree", weight="distance", &
            kernel="epanechnikov", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))


    print*, "  "
    print*, "  "
    print*, " **************** brute_force, weight=distance, kernel=tricubic"
    knn_clf = k_nearest_neighbor_classifier(algorithm="brute_force", weight="distance", &
            kernel="tricubic", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

    print*, "  "
    print*, "  "
    print*, " **************** kd_tree, weight=distance, kernel=tricubic"
    knn_clf = k_nearest_neighbor_classifier(algorithm="kd_tree", weight="distance", &
            kernel="tricubic", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

    print*, "  "
    print*, "  "
    print*, " **************** ball_tree, weight=distance, kernel=tricubic"
    knn_clf = k_nearest_neighbor_classifier(algorithm="ball_tree", weight="distance", &
            kernel="tricubic", min_samples_in_leaf=256_8)
    call knn_clf%fit(x_train, y_oh_train)
    pred_train = knn_clf%predict(x_train(:n_test,:))
    pred_test = knn_clf%predict(x_test(:n_test,:))
    pred_train = reshape(maxloc(pred_train, dim=2), shape=[size(pred_train, dim=1), 1])
    pred_test  = reshape(maxloc(pred_test, dim=2), shape=[size(pred_test, dim=1), 1])
    print*, metric%accuracy(y_train(:n_test,1), pred_train(:n_test,1))
    print*, metric%accuracy(y_test(:n_test,1),  pred_test(:n_test,1))

end program main_neighbour_k_nearest_neighbor