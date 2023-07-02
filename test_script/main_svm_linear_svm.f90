program main_svm_linear_svm
    use mod_linear_svm
    use mod_metric
    use mod_timer
    use mod_scaler
    use mod_data_holder
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8), allocatable    :: x_train(:,:), x_test(:,:)
    integer(kind=8), allocatable :: y_train(:,:), y_test(:,:)
    integer(kind=8), allocatable :: pred_train(:,:), pred_test(:,:)
    integer(kind=8) :: i, n_iter, iter

    type(metrics) :: mtrc
    type(standard_scaler) :: ss_sclr
    type(linear_svm_classifier) :: li_svc
    type(data_holder) :: dholder

    CHARACTER(len=256), allocatable :: fns_x_train(:), fns_y_train(:)
    CHARACTER(len=256), allocatable :: fns_x_test(:), fns_y_test(:)

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

    n_iter = 5000
    do i=1, size(fns_x_train), 1
        call read_bin_2d(fns_x_train(i), x_train, print_log=f_)
        call read_bin_2d(fns_y_train(i), y_train, print_log=f_)

        call read_bin_2d(fns_x_test(i), x_test, print_log=f_)
        call read_bin_2d(fns_y_test(i), y_test, print_log=f_)

        ss_sclr = standard_scaler()
        call ss_sclr%fit(x_train)
        x_train = ss_sclr%transform(x_train)
        x_test = ss_sclr%transform(x_test)

        dholder = data_holder(x_train, y_train)

        li_svc = linear_svm_classifier()
        print*, '*********************************************************************************************'
        print*, ""
        print*, ""
        print*, trim(fns_x_train(i))
        call date_and_time(values=date_value1)
        do iter=1, n_iter, 1
            call li_svc%fit(x_train, y_train)
            call date_and_time(values=date_value2)
            if (time_diff(date_value1, date_value2) >= 60_8*1000_8) exit
        end do
        call date_and_time(values=date_value2)
        print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        pred_train = li_svc%predict(x_train)
        pred_test  = li_svc%predict(x_test)
        print*, "  Train:    ", mtrc%accuracy(y_train(:,1), pred_train(:,1))
        print*, "  Test:     ", mtrc%accuracy(y_test(:,1), pred_test(:,1))
        print*, "  Iter:     ", li_svc%n_iter_
        print*, '*********************************************************************************************'
        print*, ""
        print*, ""
        print*, trim(fns_x_train(i))
        call date_and_time(values=date_value1)
        do iter=1, n_iter, 1
            call li_svc%fit(dholder)
            call date_and_time(values=date_value2)
            if (time_diff(date_value1, date_value2) >= 60_8*1000_8) exit
        end do
        call date_and_time(values=date_value2)
        print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        pred_train = li_svc%predict(x_train)
        pred_test  = li_svc%predict(x_test)
        print*, "  Train:    ", mtrc%accuracy(y_train(:,1), pred_train(:,1))
        print*, "  Test:     ", mtrc%accuracy(y_test(:,1), pred_test(:,1))
        print*, "  Iter:     ", li_svc%n_iter_
    end do
end program main_svm_linear_svm
