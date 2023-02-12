! 
! ループ終了時に学習結果を反映できていない
! exitのあとに
! a(indices_) = a_
! がないから精度が悪いのでは？特にループ回数が少ない場合に
! 

program main_svm_kernel_svm
    use mod_kernel_svm
    use mod_linear_svm
    use mod_metric
    use mod_scaler
    use mod_timer
    implicit none

    integer(kind=8)        :: date_value1(8), date_value2(8)

    type(kernel_svm_classifier) :: svc_poly
    type(linear_svm_classifier) :: svc_linear
    type(metrics) :: mtrc
    type(standard_scaler) :: ss_sclr

    real(kind=8), allocatable    :: x_train(:,:), x_test(:,:)
    integer(kind=8), allocatable :: y_train(:,:), y_test(:,:)
    integer(kind=8), allocatable :: pred_train(:,:), pred_test(:,:)

    CHARACTER(len=256), allocatable :: fns_x_train(:), fns_y_train(:)
    CHARACTER(len=256), allocatable :: fns_x_test(:), fns_y_test(:)

    real(kind=8), allocatable :: sk_metrics(:)

    integer(kind=8) :: i, iter, n_iter

    fns_x_train = [ &
        "../sample_data/make_classification_X_train_0000000100x00005_class=002.bin",&
        "../sample_data/make_classification_X_train_0000000100x00010_class=002.bin",&
        "../sample_data/make_classification_X_train_0000000100x00050_class=002.bin",&
        "../sample_data/make_classification_X_train_0000000100x00100_class=002.bin",&
        "../sample_data/make_classification_X_train_0000000100x00200_class=002.bin",&
        "../sample_data/make_classification_X_train_0000000100x00400_class=002.bin",&
        "../sample_data/make_classification_X_train_0000001000x00005_class=002.bin",&
        "../sample_data/make_classification_X_train_0000001000x00010_class=002.bin",&
        "../sample_data/make_classification_X_train_0000001000x00050_class=002.bin",&
        "../sample_data/make_classification_X_train_0000001000x00100_class=002.bin",&
        "../sample_data/make_classification_X_train_0000001000x00200_class=002.bin",&
        "../sample_data/make_classification_X_train_0000001000x00400_class=002.bin",&
        "../sample_data/make_classification_X_train_0000010000x00005_class=002.bin",&
        "../sample_data/make_classification_X_train_0000010000x00010_class=002.bin",&
        "../sample_data/make_classification_X_train_0000010000x00050_class=002.bin",&
        "../sample_data/make_classification_X_train_0000010000x00100_class=002.bin",&
        "../sample_data/make_classification_X_train_0000010000x00200_class=002.bin",&
        "../sample_data/make_classification_X_train_0000010000x00400_class=002.bin"&
    ]
    fns_y_train = [ &
        "../sample_data/make_classification_y_train_0000000100x00005_class=002.bin",&
        "../sample_data/make_classification_y_train_0000000100x00010_class=002.bin",&
        "../sample_data/make_classification_y_train_0000000100x00050_class=002.bin",&
        "../sample_data/make_classification_y_train_0000000100x00100_class=002.bin",&
        "../sample_data/make_classification_y_train_0000000100x00200_class=002.bin",&
        "../sample_data/make_classification_y_train_0000000100x00400_class=002.bin",&
        "../sample_data/make_classification_y_train_0000001000x00005_class=002.bin",&
        "../sample_data/make_classification_y_train_0000001000x00010_class=002.bin",&
        "../sample_data/make_classification_y_train_0000001000x00050_class=002.bin",&
        "../sample_data/make_classification_y_train_0000001000x00100_class=002.bin",&
        "../sample_data/make_classification_y_train_0000001000x00200_class=002.bin",&
        "../sample_data/make_classification_y_train_0000001000x00400_class=002.bin",&
        "../sample_data/make_classification_y_train_0000010000x00005_class=002.bin",&
        "../sample_data/make_classification_y_train_0000010000x00010_class=002.bin",&
        "../sample_data/make_classification_y_train_0000010000x00050_class=002.bin",&
        "../sample_data/make_classification_y_train_0000010000x00100_class=002.bin",&
        "../sample_data/make_classification_y_train_0000010000x00200_class=002.bin",&
        "../sample_data/make_classification_y_train_0000010000x00400_class=002.bin"&
    ]

    fns_x_test = [ &
        "../sample_data/make_classification_X_test_0000000100x00005_class=002.bin",&
        "../sample_data/make_classification_X_test_0000000100x00010_class=002.bin",&
        "../sample_data/make_classification_X_test_0000000100x00050_class=002.bin",&
        "../sample_data/make_classification_X_test_0000000100x00100_class=002.bin",&
        "../sample_data/make_classification_X_test_0000000100x00200_class=002.bin",&
        "../sample_data/make_classification_X_test_0000000100x00400_class=002.bin",&
        "../sample_data/make_classification_X_test_0000001000x00005_class=002.bin",&
        "../sample_data/make_classification_X_test_0000001000x00010_class=002.bin",&
        "../sample_data/make_classification_X_test_0000001000x00050_class=002.bin",&
        "../sample_data/make_classification_X_test_0000001000x00100_class=002.bin",&
        "../sample_data/make_classification_X_test_0000001000x00200_class=002.bin",&
        "../sample_data/make_classification_X_test_0000001000x00400_class=002.bin",&
        "../sample_data/make_classification_X_test_0000010000x00005_class=002.bin",&
        "../sample_data/make_classification_X_test_0000010000x00010_class=002.bin",&
        "../sample_data/make_classification_X_test_0000010000x00050_class=002.bin",&
        "../sample_data/make_classification_X_test_0000010000x00100_class=002.bin",&
        "../sample_data/make_classification_X_test_0000010000x00200_class=002.bin",&
        "../sample_data/make_classification_X_test_0000010000x00400_class=002.bin"&
    ]
    fns_y_test = [ &
        "../sample_data/make_classification_y_test_0000000100x00005_class=002.bin",&
        "../sample_data/make_classification_y_test_0000000100x00010_class=002.bin",&
        "../sample_data/make_classification_y_test_0000000100x00050_class=002.bin",&
        "../sample_data/make_classification_y_test_0000000100x00100_class=002.bin",&
        "../sample_data/make_classification_y_test_0000000100x00200_class=002.bin",&
        "../sample_data/make_classification_y_test_0000000100x00400_class=002.bin",&
        "../sample_data/make_classification_y_test_0000001000x00005_class=002.bin",&
        "../sample_data/make_classification_y_test_0000001000x00010_class=002.bin",&
        "../sample_data/make_classification_y_test_0000001000x00050_class=002.bin",&
        "../sample_data/make_classification_y_test_0000001000x00100_class=002.bin",&
        "../sample_data/make_classification_y_test_0000001000x00200_class=002.bin",&
        "../sample_data/make_classification_y_test_0000001000x00400_class=002.bin",&
        "../sample_data/make_classification_y_test_0000010000x00005_class=002.bin",&
        "../sample_data/make_classification_y_test_0000010000x00010_class=002.bin",&
        "../sample_data/make_classification_y_test_0000010000x00050_class=002.bin",&
        "../sample_data/make_classification_y_test_0000010000x00100_class=002.bin",&
        "../sample_data/make_classification_y_test_0000010000x00200_class=002.bin",&
        "../sample_data/make_classification_y_test_0000010000x00400_class=002.bin"&
    ]

    allocate(sk_metrics(size(fns_y_test)))
    sk_metrics = 0d0
    sk_metrics(:6)    = [1d0, 0.75d0, 0.75d0, 0.85d0, 0.65d0, 0.65d0]
    sk_metrics(7:12)  = [0.9d0, 0.965d0, 0.88d0, 0.745d0, 0.965d0, 0.87d0]
    sk_metrics(13:18) = [0.9015d0, 0.926d0, 0.9225d0, 0.925d0, 0.9135d0, 0.867d0]

    

    n_iter = 1000
    n_iter = 1
    ! do i=13, size(fns_x_train), 1000
    ! do i=2, size(fns_x_train), 10000
    do i=13, 18, 1
        print*, fns_x_train(i)
        call read_bin_2d(fns_x_train(i), x_train, print_log=f_)
        call read_bin_2d(fns_y_train(i), y_train, print_log=f_)

        call read_bin_2d(fns_x_test(i), x_test, print_log=f_)
        call read_bin_2d(fns_y_test(i), y_test, print_log=f_)

        ss_sclr = standard_scaler()
        call ss_sclr%fit(x_train)
        x_train = ss_sclr%transform(x_train)
        x_test = ss_sclr%transform(x_test)

        ! svc_linear = linear_svm_classifier(tolerance=1d-3)
        ! call date_and_time(values=date_value1)
        ! do iter=1, n_iter, 1
        !     call svc_linear%fit(x_train, y_train)
        !     call date_and_time(values=date_value2)
        !     if (time_diff(date_value1, date_value2) >= 10_8*1000_8) exit
        ! end do
        ! call date_and_time(values=date_value2)
        ! print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        ! pred_train = svc_linear%predict(x_train)
        ! pred_test  = svc_linear%predict(x_test)
        ! print*, "  Train:    ", mtrc%accuracy(y_train(:,1), pred_train(:,1))
        ! print*, "  Test:     ", mtrc%accuracy(y_test(:,1), pred_test(:,1))

        ! svc_poly = kernel_svm_classifier(kernel="poly", degree=3_8, tolerance=1d-3)
        ! call date_and_time(values=date_value1)
        ! do iter=1, n_iter, 1
        !     call svc_poly%fit(x_train, y_train)
        !     call date_and_time(values=date_value2)
        !     if (time_diff(date_value1, date_value2) >= 10_8*1000_8) exit
        ! end do
        ! call date_and_time(values=date_value2)
        ! print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        ! pred_train = svc_poly%predict(x_train)
        ! pred_test  = svc_poly%predict(x_test)
        ! print*, "  Train:    ", mtrc%accuracy(y_train(:,1), pred_train(:,1))
        ! print*, "  Test:     ", mtrc%accuracy(y_test(:,1), pred_test(:,1))

        ! svc_poly = kernel_svm_classifier(kernel="poly", degree=3_8, tolerance=1d-4)
        ! call date_and_time(values=date_value1)
        ! do iter=1, n_iter, 1
        !     call svc_poly%fit_kernel_svm_classifier_ver01(x_train, y_train)
        !     call date_and_time(values=date_value2)
        !     if (time_diff(date_value1, date_value2) >= 10_8*1000_8) exit
        ! end do
        ! call date_and_time(values=date_value2)
        ! print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        ! pred_train = svc_poly%predict(x_train)
        ! pred_test  = svc_poly%predict(x_test)
        ! print*, "  Train:    ", mtrc%accuracy(y_train(:,1), pred_train(:,1))
        ! print*, "  Test:     ", mtrc%accuracy(y_test(:,1), pred_test(:,1))

        ! svc_poly = kernel_svm_classifier(kernel="poly", degree=3_8, tolerance=1d-4)
        ! call date_and_time(values=date_value1)
        ! do iter=1, n_iter, 1
        !     call svc_poly%fit_kernel_svm_classifier_ver02(x_train, y_train)
        !     call date_and_time(values=date_value2)
        !     if (time_diff(date_value1, date_value2) >= 10_8*1000_8) exit
        ! end do
        ! call date_and_time(values=date_value2)
        ! print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        ! pred_train = svc_poly%predict(x_train)
        ! pred_test  = svc_poly%predict(x_test)
        ! print*, "  Train:    ", mtrc%accuracy(y_train(:,1), pred_train(:,1))
        ! print*, "  Test:     ", mtrc%accuracy(y_test(:,1), pred_test(:,1))
        ! print*, "  Iter:     ", svc_poly%n_iter_

        ! svc_poly = kernel_svm_classifier(kernel="poly", degree=3_8, tolerance=1d-4)
        ! call date_and_time(values=date_value1)
        ! do iter=1, n_iter, 1
        !     call svc_poly%fit_kernel_svm_classifier_ver03(x_train, y_train)
        !     call date_and_time(values=date_value2)
        !     if (time_diff(date_value1, date_value2) >= 10_8*1000_8) exit
        ! end do
        ! call date_and_time(values=date_value2)
        ! print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        ! pred_train = svc_poly%predict(x_train)
        ! pred_test  = svc_poly%predict(x_test)
        ! print*, "  Train:    ", mtrc%accuracy(y_train(:,1), pred_train(:,1))
        ! print*, "  Test:     ", mtrc%accuracy(y_test(:,1), pred_test(:,1))
        ! print*, "  Iter:     ", svc_poly%n_iter_

        ! svc_poly = kernel_svm_classifier(kernel="poly", degree=3_8, tolerance=1d-4)
        ! call date_and_time(values=date_value1)
        ! do iter=1, n_iter, 1
        !     call svc_poly%fit_kernel_svm_classifier_ver04(x_train, y_train)
        !     call date_and_time(values=date_value2)
        !     if (time_diff(date_value1, date_value2) >= 10_8*1000_8) exit
        ! end do
        ! call date_and_time(values=date_value2)
        ! print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        ! pred_train = svc_poly%predict(x_train)
        ! pred_test  = svc_poly%predict(x_test)
        ! print*, "  Train:    ", mtrc%accuracy(y_train(:,1), pred_train(:,1))
        ! print*, "  Test:     ", mtrc%accuracy(y_test(:,1), pred_test(:,1))
        ! print*, "  Iter:     ", svc_poly%n_iter_

        svc_poly = kernel_svm_classifier(kernel="rbf", tolerance=1d-3, cache_size=1024_8)
        call date_and_time(values=date_value1)
        do iter=1, n_iter, 1
            print*,  "iter: fit_kernel_svm_classifier_ver05", iter
            call svc_poly%fit_kernel_svm_classifier_ver05(x_train, y_train)
            call date_and_time(values=date_value2)
            if (time_diff(date_value1, date_value2) >= 10_8*1000_8) exit
        end do
        call date_and_time(values=date_value2)
        print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        pred_train = svc_poly%predict(x_train)
        pred_test  = svc_poly%predict(x_test)
        print*, "  Train:    ", mtrc%accuracy(y_train(:,1), pred_train(:,1))
        print*, "  Test:     ", mtrc%accuracy(y_test(:,1), pred_test(:,1)), sk_metrics(i)
        print*, "  Iter:     ", svc_poly%n_iter_

        ! print*, '*********************************************************************************************'
        ! print*, ""
        ! print*, ""
        ! print*, trim(fns_x_train(i))
        ! call date_and_time(values=date_value1)
        ! do iter=1, n_iter, 1
        !     call li_svc%fit(x_train, y_train)
        !     call date_and_time(values=date_value2)
        !     if (time_diff(date_value1, date_value2) >= 60_8*1000_8) exit
        ! end do
        ! call date_and_time(values=date_value2)
        ! print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        ! pred_train = li_svc%predict(x_train)
        ! pred_test  = li_svc%predict(x_test)
        ! print*, "  Train:    ", mtrc%accuracy(y_train(:,1), pred_train(:,1))
        ! print*, "  Test:     ", mtrc%accuracy(y_test(:,1), pred_test(:,1))
        ! print*, "  Iter:     ", li_svc%n_iter_
    end do



end program main_svm_kernel_svm