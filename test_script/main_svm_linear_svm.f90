program main_svm_linear_svm
    use mod_linear_svm
    use mod_metric
    use mod_timer
    use mod_scaler
    implicit none

    integer(kind=8)     :: date_value1(8), date_value2(8)
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    real(kind=8), allocatable :: x_train(:,:), x_train_soft(:,:)
    integer(kind=8), allocatable :: y_train(:,:), y_train_soft(:,:), labels_pred(:,:)
    integer(kind=8), allocatable :: times(:)

    type(metrics) :: mtrc
    integer(kind=8) :: i, n_iter, n_times
    type(standard_scaler) :: ss_sclr

    type(linear_svm_classifier) :: lin_svm_clf
    type(linear_svm_classifier) :: lin_svm_clf_02
    type(linear_svm_classifier) :: lin_svm_clf_03
    type(linear_svm_classifier) :: lin_svm_clf_04
    type(linear_svm_classifier) :: lin_svm_clf_05
    type(linear_svm_classifier) :: lin_svm_clf_06

    ! file_name_x_train_bin = "../sample_data/svm_hard_x_100x2.bin"
    ! file_name_y_train_bin = "../sample_data/svm_hard_y_100x1.bin"
    ! call read_bin_2d(file_name_x_train_bin, x_train)
    ! call read_bin_2d(file_name_y_train_bin, y_train)

    ! lin_svm_clf = linear_svm_classifier(c=1d0)

    ! call lin_svm_clf%fit_linear_svm_classifier_hard(x_train, y_train)
    
    ! labels_pred = lin_svm_clf%predict(x_train)

    ! print*, "HARD: ", mtrc%accuracy(y_train(:,1), labels_pred(:,1))

    ! n_iter = 5000
    ! n_iter = 500
    ! n_iter = 50
    n_iter = 10
    ! n_iter = 5
    n_iter = 1
    ! file_name_x_train_bin = "../sample_data/make_classification_X_train_0000000100x00005_class=002.bin"
    ! file_name_y_train_bin = "../sample_data/make_classification_y_train_0000000100x00005_class=002.bin"
    ! file_name_x_train_bin = "../sample_data/make_classification_X_train_0000001000x00005_class=002.bin"
    ! file_name_y_train_bin = "../sample_data/make_classification_y_train_0000001000x00005_class=002.bin"
    ! file_name_x_train_bin = "../sample_data/make_classification_X_train_0000010000x00005_class=002.bin"
    ! file_name_y_train_bin = "../sample_data/make_classification_y_train_0000010000x00005_class=002.bin"
    file_name_x_train_bin = "../sample_data/make_classification_X_train_0000001000x00050_class=002.bin"
    file_name_y_train_bin = "../sample_data/make_classification_y_train_0000001000x00050_class=002.bin"
    ! file_name_x_train_bin = "../sample_data/make_classification_X_train_0000010000x00050_class=002.bin"
    ! file_name_y_train_bin = "../sample_data/make_classification_y_train_0000010000x00050_class=002.bin"
    call read_bin_2d(file_name_x_train_bin, x_train_soft)
    call read_bin_2d(file_name_y_train_bin, y_train_soft)

    ss_sclr = standard_scaler()
    call ss_sclr%fit(x_train_soft)
    x_train_soft = ss_sclr%transform(x_train_soft)
    print*, "Start -----------------"
    n_times = 8
    allocate(times(n_times))

    ! print*, '*********************************************************************************************'
    ! print*, '*********************************************************************************************'
    ! print*, '*********************************************************************************************'
    ! times = 0
    ! lin_svm_clf = linear_svm_classifier(c=1d0)
    ! call date_and_time(values=date_value1)
    ! do i=1, n_iter, 1
    !     call lin_svm_clf%fit(x_train_soft, y_train_soft, times)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Timer_ver01:    ", time_diff(date_value1, date_value2) / dble(n_iter), "[msec]"
    ! print*, "    1. Initialize :                   ", times(0+1) / dble(n_iter)
    ! print*, "    2. Compute y_f nabla f(alpha) :   ", times(1+1) / dble(n_iter)
    ! print*, "    3. Select efficient working set : ", times(2+1) / dble(n_iter)
    ! print*, "    4. Update ay and ay2:             ", times(3+1) / dble(n_iter)
    ! print*, "    5. Compute ai and aj:             ", times(4+1) / dble(n_iter)
    ! print*, "    6. Update selecting Conditions:   ", times(5+1) / dble(n_iter)
    ! print*, "    7. Compute weight and bias:       ", times(6+1) / dble(n_iter)
    ! print*, "    8. Iteration Count:               ", times(7+1)

    ! print*, "Timer_ver01:    ", sum(times(1:n_times-1)) / dble(n_iter), "[msec]"
    ! labels_pred = lin_svm_clf%predict(x_train_soft)
    ! print*, "Accuracy_ver01: ", mtrc%accuracy(y_train_soft(:,1), labels_pred(:,1))
    ! print*, "model.w_ = np.array([", lin_svm_clf%w_(:), "])"
    ! print*, "model.w0_ = ", lin_svm_clf%w0_
    
    ! print*, '*********************************************************************************************'
    ! print*, '*********************************************************************************************'
    ! print*, '*********************************************************************************************'
    ! times = 0_8
    ! lin_svm_clf_02 = linear_svm_classifier(c=1d0)
    ! call date_and_time(values=date_value1)
    ! do i=1, n_iter, 1
    !     call lin_svm_clf_02%fit_linear_svm_classifier_ver02(x_train_soft, y_train_soft, times)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Timer_ver02:    ", time_diff(date_value1, date_value2) / dble(n_iter), "[msec]"
    ! print*, "    1. Initialize :                   ", times(0+1) / dble(n_iter)
    ! print*, "    2. Compute y_f nabla f(alpha) :   ", times(1+1) / dble(n_iter)
    ! print*, "    3. Select efficient working set : ", times(2+1) / dble(n_iter)
    ! print*, "    4. Update ay and ay2:             ", times(3+1) / dble(n_iter)
    ! print*, "    5. Compute ai and aj:             ", times(4+1) / dble(n_iter)
    ! print*, "    6. Update selecting Conditions:   ", times(5+1) / dble(n_iter)
    ! print*, "    7. Compute weight and bias:       ", times(6+1) / dble(n_iter)
    ! print*, "    8. Iteration Count:               ", times(7+1)

    ! print*, "Timer_ver02:    ", sum(times(1:n_times-1)) / dble(n_iter), "[msec]"

    ! labels_pred = lin_svm_clf_02%predict(x_train_soft)
    ! print*, "Accuracy_ver02: ", mtrc%accuracy(y_train_soft(:,1), labels_pred(:,1))
    ! print*, "model.w_ = np.array([", lin_svm_clf_02%w_(:), "])"
    ! print*, "model.w0_ = ", lin_svm_clf_02%w0_
    
    ! print*, '*********************************************************************************************'
    ! print*, '*********************************************************************************************'
    ! print*, '*********************************************************************************************'
    ! times = 0_8
    ! lin_svm_clf_03 = linear_svm_classifier(c=1d0)
    ! call date_and_time(values=date_value1)
    ! do i=1, n_iter, 1
    !     call lin_svm_clf_03%fit_linear_svm_classifier_ver03(x_train_soft, y_train_soft, times)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Timer_ver03:    ", time_diff(date_value1, date_value2) / dble(n_iter), "[msec]"
    ! print*, "    1. Initialize :                   ", times(0+1) / dble(n_iter)
    ! print*, "    2. Compute y_f nabla f(alpha) :   ", times(1+1) / dble(n_iter)
    ! print*, "    3. Select efficient working set : ", times(2+1) / dble(n_iter)
    ! print*, "    4. Update ay and ay2:             ", times(3+1) / dble(n_iter)
    ! print*, "    5. Compute ai and aj:             ", times(4+1) / dble(n_iter)
    ! print*, "    6. Update selecting Conditions:   ", times(5+1) / dble(n_iter)
    ! print*, "    7. Compute weight and bias:       ", times(6+1) / dble(n_iter)
    ! print*, "    8. Iteration Count:               ", times(7+1)

    ! print*, "Timer_ver03:    ", sum(times(1:n_times-1)) / dble(n_iter), "[msec]"
    
    ! labels_pred = lin_svm_clf_03%predict(x_train_soft)
    ! print*, "Accuracy_ver03: ", mtrc%accuracy(y_train_soft(:,1), labels_pred(:,1))
    ! print*, "model.w_ = np.array([", lin_svm_clf_03%w_(:), "])"
    ! print*, "model.w0_ = ", lin_svm_clf_03%w0_

    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    times = 0_8
    lin_svm_clf_04 = linear_svm_classifier(c=1d0)
    call date_and_time(values=date_value1)
    do i=1, n_iter, 1
        call lin_svm_clf_04%fit_linear_svm_classifier_ver04(x_train_soft, y_train_soft, times)
    end do
    call date_and_time(values=date_value2)
    print*, "Timer_ver04:    ", time_diff(date_value1, date_value2) / dble(n_iter), "[msec]"
    print*, "    1. Initialize :                   ", times(0+1) / dble(n_iter)
    print*, "    2. Compute y_f nabla f(alpha) :   ", times(1+1) / dble(n_iter)
    print*, "    3. Select efficient working set : ", times(2+1) / dble(n_iter)
    print*, "    4. Update ay and ay2:             ", times(3+1) / dble(n_iter)
    print*, "    5. Compute ai and aj:             ", times(4+1) / dble(n_iter)
    print*, "    6. Update selecting Conditions:   ", times(5+1) / dble(n_iter)
    print*, "    7. Compute weight and bias:       ", times(6+1) / dble(n_iter)
    print*, "    8. Iteration Count:               ", times(7+1)

    print*, "Timer_ver04:    ", sum(times(1:n_times-1)) / dble(n_iter), "[msec]"
    
    labels_pred = lin_svm_clf_04%predict(x_train_soft)
    print*, "Accuracy_ver04: ", mtrc%accuracy(y_train_soft(:,1), labels_pred(:,1))
    print*, "model.w_ = np.array([", lin_svm_clf_04%w_(:), "])"
    print*, "model.w0_ = ", lin_svm_clf_04%w0_

    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    times = 0_8
    lin_svm_clf_05 = linear_svm_classifier(c=1d0)
    call date_and_time(values=date_value1)
    do i=1, n_iter, 1
        call lin_svm_clf_05%fit_linear_svm_classifier_ver05(x_train_soft, y_train_soft, times)
    end do
    call date_and_time(values=date_value2)
    print*, "Timer_ver05:    ", time_diff(date_value1, date_value2) / dble(n_iter), "[msec]"
    print*, "    1. Initialize :                   ", times(0+1) / dble(n_iter)
    print*, "    2. Compute y_f nabla f(alpha) :   ", times(1+1) / dble(n_iter)
    print*, "    3. Select efficient working set : ", times(2+1) / dble(n_iter)
    print*, "    4. Update ay and ay2:             ", times(3+1) / dble(n_iter)
    print*, "    5. Compute ai and aj:             ", times(4+1) / dble(n_iter)
    print*, "    6. Update selecting Conditions:   ", times(5+1) / dble(n_iter)
    print*, "    7. Compute weight and bias:       ", times(6+1) / dble(n_iter)
    print*, "    8. Iteration Count:               ", times(7+1)

    print*, "Timer_ver05:    ", sum(times(1:n_times-1)) / dble(n_iter), "[msec]"
    
    labels_pred = lin_svm_clf_05%predict(x_train_soft)
    print*, "Accuracy_ver05: ", mtrc%accuracy(y_train_soft(:,1), labels_pred(:,1))
    print*, "model.w_ = np.array([", lin_svm_clf_05%w_(:), "])"
    print*, "model.w0_ = ", lin_svm_clf_05%w0_

    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    times = 0_8
    lin_svm_clf_06 = linear_svm_classifier(c=1d0)
    call date_and_time(values=date_value1)
    do i=1, n_iter, 1
        call lin_svm_clf_06%fit_linear_svm_classifier_ver06(x_train_soft, y_train_soft, times)
    end do
    call date_and_time(values=date_value2)
    print*, "Timer_ver06:    ", time_diff(date_value1, date_value2) / dble(n_iter), "[msec]"
    print*, "    1. Initialize :                   ", times(0+1) / dble(n_iter)
    print*, "    2. Compute y_f nabla f(alpha) :   ", times(1+1) / dble(n_iter)
    print*, "    3. Select efficient working set : ", times(2+1) / dble(n_iter)
    print*, "    4. Update ay and ay2:             ", times(3+1) / dble(n_iter)
    print*, "    5. Compute ai and aj:             ", times(4+1) / dble(n_iter)
    print*, "    6. Update selecting Conditions:   ", times(5+1) / dble(n_iter)
    print*, "    7. Compute weight and bias:       ", times(6+1) / dble(n_iter)
    print*, "    8. Iteration Count:               ", times(7+1)

    print*, "Timer_ver06:    ", sum(times(1:n_times-1)) / dble(n_iter), "[msec]"
    
    labels_pred = lin_svm_clf_06%predict(x_train_soft)
    print*, "Accuracy_ver06: ", mtrc%accuracy(y_train_soft(:,1), labels_pred(:,1))
    print*, "model.w_ = np.array([", lin_svm_clf_06%w_(:), "])"
    print*, "model.w0_ = ", lin_svm_clf_06%w0_
end program main_svm_linear_svm
