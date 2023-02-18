! 
! ループ終了時に学習結果を反映できていない
! exitのあとに
! a(indices_) = a_
! がないから精度が悪いのでは？特にループ回数が少ない場合に
! 

program main_svm_kernel_svm
    use mod_kernel_svm, only: kernel_svm_classifier
    use mod_linear_svm, only: linear_svm_classifier
    use mod_metric
    use mod_scaler
    use mod_timer
    implicit none

    integer(kind=8)        :: dval1(8), dval2(8)
    integer(kind=8)        :: times(5)

    type(kernel_svm_classifier) :: ksvc_poly, ksvc_rbf, ksvc_linear, ksvc_sigmoid
    type(linear_svm_classifier) :: lsvc
    type(metrics) :: mtrc
    type(standard_scaler) :: ss_sclr
    real(kind=8)    :: acc_train(5)
    real(kind=8)    :: acc_test(5)

    real(kind=8), allocatable    :: x_train(:,:), x_test(:,:)
    integer(kind=8), allocatable :: y_train(:,:), y_test(:,:)

    integer(kind=8), allocatable :: pred_train_lsvc(:,:), pred_test_lsvc(:,:)
    integer(kind=8), allocatable :: pred_train_poly(:,:), pred_test_poly(:,:)
    integer(kind=8), allocatable :: pred_train_rbf(:,:), pred_test_rbf(:,:)
    integer(kind=8), allocatable :: pred_train_linear(:,:), pred_test_linear(:,:)
    integer(kind=8), allocatable :: pred_train_sigmoid(:,:), pred_test_sigmoid(:,:)

    CHARACTER(len=256), allocatable :: fns_x_train(:), fns_y_train(:)
    CHARACTER(len=256), allocatable :: fns_x_test(:), fns_y_test(:)

    CHARACTER(len=256), allocatable :: data_desc(:)

    character(len=1024) :: fmt, fmt1, fmt2
    character(len=1024) :: header, header1, header2, header3, header4, header5

    integer(kind=8) :: i, iter, n_iter, min_seconds

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
        "../sample_data/make_classification_X_train_0000010000x00400_class=002.bin",&
        "../sample_data/make_classification_X_train_0000100000x00005_class=002.bin",&
        "../sample_data/make_classification_X_train_0000100000x00010_class=002.bin",&
        "../sample_data/make_classification_X_train_0000100000x00050_class=002.bin",&
        "../sample_data/make_classification_X_train_0000100000x00100_class=002.bin",&
        "../sample_data/make_classification_X_train_0000100000x00200_class=002.bin",&
        "../sample_data/make_classification_X_train_0000100000x00400_class=002.bin"&
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
        "../sample_data/make_classification_y_train_0000010000x00400_class=002.bin",&
        "../sample_data/make_classification_y_train_0000100000x00005_class=002.bin",&
        "../sample_data/make_classification_y_train_0000100000x00010_class=002.bin",&
        "../sample_data/make_classification_y_train_0000100000x00050_class=002.bin",&
        "../sample_data/make_classification_y_train_0000100000x00100_class=002.bin",&
        "../sample_data/make_classification_y_train_0000100000x00200_class=002.bin",&
        "../sample_data/make_classification_y_train_0000100000x00400_class=002.bin"&
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
        "../sample_data/make_classification_X_test_0000010000x00400_class=002.bin",&
        "../sample_data/make_classification_X_test_0000100000x00005_class=002.bin",&
        "../sample_data/make_classification_X_test_0000100000x00010_class=002.bin",&
        "../sample_data/make_classification_X_test_0000100000x00050_class=002.bin",&
        "../sample_data/make_classification_X_test_0000100000x00100_class=002.bin",&
        "../sample_data/make_classification_X_test_0000100000x00200_class=002.bin",&
        "../sample_data/make_classification_X_test_0000100000x00400_class=002.bin"&
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
        "../sample_data/make_classification_y_test_0000010000x00400_class=002.bin",&
        "../sample_data/make_classification_y_test_0000100000x00005_class=002.bin",&
        "../sample_data/make_classification_y_test_0000100000x00010_class=002.bin",&
        "../sample_data/make_classification_y_test_0000100000x00050_class=002.bin",&
        "../sample_data/make_classification_y_test_0000100000x00100_class=002.bin",&
        "../sample_data/make_classification_y_test_0000100000x00200_class=002.bin",&
        "../sample_data/make_classification_y_test_0000100000x00400_class=002.bin"&
    ]

    data_desc = [ &
        "0000000100x00005_class=002", &
        "0000000100x00010_class=002", &
        "0000000100x00050_class=002", &
        "0000000100x00100_class=002", &
        "0000000100x00200_class=002", &
        "0000000100x00400_class=002", &
        "0000001000x00005_class=002", &
        "0000001000x00010_class=002", &
        "0000001000x00050_class=002", &
        "0000001000x00100_class=002", &
        "0000001000x00200_class=002", &
        "0000001000x00400_class=002", &
        "0000010000x00005_class=002", &
        "0000010000x00010_class=002", &
        "0000010000x00050_class=002", &
        "0000010000x00100_class=002", &
        "0000010000x00200_class=002", &
        "0000010000x00400_class=002", &
        "0000100000x00005_class=002", &
        "0000100000x00010_class=002", &
        "0000100000x00050_class=002", &
        "0000100000x00100_class=002", &
        "0000100000x00200_class=002", &
        "0000100000x00400_class=002"  &
    ]

    n_iter = 1000
    min_seconds = 30

    header3 = "                               -----------------   TRAIN TIME   ------------------------"
    header4 = "   ----------          TRAIN ACCURACY           ------------ "
    header5 = "   ----------          TEST  ACCURACY           ------------ "
    header1 = " -----------------------       LinearSVC KSVC_linear   KSVC_poly    KSVC_sig    KSVC_rbf     "
    header2 = "     LinearSVC KSVC_linear   KSVC_poly    KSVC_sig    KSVC_rbf     "

    header = trim(header1) // trim(header2) // trim(header2)
    print*, header3(1:len(trim(header1))) // trim(header4) // trim(header5)
    print*, trim(header)
    do i=1, size(fns_x_train), 1
        ! print*, fns_x_train(i)
        call read_bin_2d(fns_x_train(i), x_train, print_log=f_)
        call read_bin_2d(fns_y_train(i), y_train, print_log=f_)

        call read_bin_2d(fns_x_test(i), x_test, print_log=f_)
        call read_bin_2d(fns_y_test(i), y_test, print_log=f_)

        ss_sclr = standard_scaler()
        call ss_sclr%fit(x_train)
        x_train = ss_sclr%transform(x_train)
        x_test = ss_sclr%transform(x_test)

        lsvc         = linear_svm_classifier(tolerance=1d-4, cache_size=1024_8, shrinking=t_)
        ksvc_linear  = kernel_svm_classifier(kernel="linear",  tolerance=1d-4, cache_size=1024_8, shrinking=t_)
        ksvc_poly    = kernel_svm_classifier(kernel="poly",    tolerance=1d-4, cache_size=1024_8, shrinking=t_, degree=3_8)
        ksvc_sigmoid = kernel_svm_classifier(kernel="sigmoid", tolerance=1d-4, cache_size=1024_8, shrinking=t_)
        ksvc_rbf     = kernel_svm_classifier(kernel="rbf",     tolerance=1d-4, cache_size=1024_8, shrinking=t_)

        call date_and_time(values=dval1)
        call lsvc%fit(x_train, y_train)
        call date_and_time(values=dval2)
        times(1) = time_diff(dval1, dval2)
        pred_train_lsvc = lsvc%predict(x_train)
        pred_test_lsvc = lsvc%predict(x_test)

        call date_and_time(values=dval1)
        call ksvc_linear%fit(x_train, y_train)
        call date_and_time(values=dval2)
        times(2) = time_diff(dval1, dval2)
        pred_train_linear = ksvc_linear%predict(x_train)
        pred_test_linear = ksvc_linear%predict(x_test)

        call date_and_time(values=dval1)
        call ksvc_poly%fit(x_train, y_train)
        call date_and_time(values=dval2)
        times(3) = time_diff(dval1, dval2)
        pred_train_poly = ksvc_poly%predict(x_train)
        pred_test_poly = ksvc_poly%predict(x_test)

        call date_and_time(values=dval1)
        call ksvc_sigmoid%fit(x_train, y_train)
        call date_and_time(values=dval2)
        times(4) = time_diff(dval1, dval2)
        pred_train_sigmoid = ksvc_sigmoid%predict(x_train)
        pred_test_sigmoid = ksvc_sigmoid%predict(x_test)

        call date_and_time(values=dval1)
        call ksvc_rbf%fit(x_train, y_train)
        call date_and_time(values=dval2)
        times(5) = time_diff(dval1, dval2)
        pred_train_rbf = ksvc_rbf%predict(x_train)
        pred_test_rbf = ksvc_rbf%predict(x_test)
        
        
        acc_train(1) = mtrc%accuracy(y_train(:,1), pred_train_lsvc(:,1))
        acc_train(2) = mtrc%accuracy(y_train(:,1), pred_train_linear(:,1))
        acc_train(3) = mtrc%accuracy(y_train(:,1), pred_train_poly(:,1))
        acc_train(4) = mtrc%accuracy(y_train(:,1), pred_train_sigmoid(:,1))
        acc_train(5) = mtrc%accuracy(y_train(:,1), pred_train_rbf(:,1))
        
        acc_test(1) = mtrc%accuracy(y_test(:,1), pred_test_lsvc(:,1))
        acc_test(2) = mtrc%accuracy(y_test(:,1), pred_test_linear(:,1))
        acc_test(3) = mtrc%accuracy(y_test(:,1), pred_test_poly(:,1))
        acc_test(4) = mtrc%accuracy(y_test(:,1), pred_test_sigmoid(:,1))
        acc_test(5) = mtrc%accuracy(y_test(:,1), pred_test_rbf(:,1))

        fmt1 = '(a, "  || ", i10, "  ", i10, "  ", i10, "  ", i10, "  ", i10, "  "'
        fmt2 = ', f10.5, "  ", f10.5, "  ", f10.5, "  ", f10.5, "  ", f10.5'
        fmt = trim(fmt1) // ' "||" ' // trim(fmt2) //  ' " || " ' // trim(fmt2) // ")"
        print   trim(fmt), trim(data_desc(i)), times, acc_train, acc_test
    end do

end program main_svm_kernel_svm