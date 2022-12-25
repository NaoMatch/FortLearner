program main_svm_kernel_svm
    use mod_kernel_svm
    use mod_metric
    implicit none
    
    type(metrics) :: mtrc
    type(kernel_svm_classifier) :: ksvm_clf

    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    real(kind=8), allocatable :: x_train(:,:), x_train_soft(:,:)
    integer(kind=8), allocatable :: y_train(:,:), y_train_soft(:,:), labels_pred(:,:)

    file_name_x_train_bin = "../sample_data/svm_soft_x_40x2.bin"
    file_name_y_train_bin = "../sample_data/svm_soft_y_40x1.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)

    ksvm_clf = kernel_svm_classifier(c=1d0, sigma=1d0)
    call ksvm_clf%fit(x_train, y_train)

    labels_pred = ksvm_clf%predict(x_train)
    print*, labels_pred


    print*, "SOFT: ", mtrc%accuracy(y_train(:,1), labels_pred(:,1))

end program main_svm_kernel_svm