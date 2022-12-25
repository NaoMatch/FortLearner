program main_svm_linear_svm
    use mod_linear_svm
    use mod_metric
    implicit none

    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    real(kind=8), allocatable :: x_train(:,:), x_train_soft(:,:)
    integer(kind=8), allocatable :: y_train(:,:), y_train_soft(:,:), labels_pred(:,:)

    type(metrics) :: mtrc
    integer(kind=8) :: i

    type(linear_svm_classifier) :: lin_svm_clf

    file_name_x_train_bin = "../sample_data/svm_hard_x_40x2.bin"
    file_name_y_train_bin = "../sample_data/svm_hard_y_40x1.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)

    lin_svm_clf = linear_svm_classifier(c=1d0)

    call lin_svm_clf%fit_linear_svm_classifier_hard(x_train, y_train)
    
    labels_pred = lin_svm_clf%predict(x_train)

    print*, "HARD: ", mtrc%accuracy(y_train(:,1), labels_pred(:,1))

    file_name_x_train_bin = "../sample_data/svm_soft_x_40x2.bin"
    file_name_y_train_bin = "../sample_data/svm_soft_y_40x1.bin"
    call read_bin_2d(file_name_x_train_bin, x_train_soft)
    call read_bin_2d(file_name_y_train_bin, y_train_soft)

    lin_svm_clf = linear_svm_classifier(c=10d0)

    call lin_svm_clf%fit(x_train_soft, y_train_soft)
    
    labels_pred = lin_svm_clf%predict(x_train_soft)

    print*, "SOFT: ", mtrc%accuracy(y_train_soft(:,1), labels_pred(:,1))

    print*, "model.w_ = np.array([", lin_svm_clf%w_(1), ", ", lin_svm_clf%w_(2), "])"
    print*, "model.w0_ = ", lin_svm_clf%w0_
end program main_svm_linear_svm