program main_forest_isolation_forest
    use mod_isolation_forest
    use mod_extended_isolation_tree
    use mod_metric
    use mod_data_holder
    use mod_scaler
    implicit none

    integer(kind=8)    :: date_value1(8), date_value2(8)
    integer(kind=8)    :: date_value1_(8), date_value2_(8)
    integer(kind=8)    :: n_samples_trains(5), n_columns_trains(5)
    integer(kind=8)    :: n_samples_train, n_columns_train 
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    CHARACTER(len=256) :: file_name_x_valid_csv, file_name_y_valid_csv
    CHARACTER(len=256) :: file_name_x_valid_bin, file_name_y_valid_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:), x_valid(:,:), x_test(:,:)
    integer(kind=8), ALLOCATABLE :: y_train(:,:), y_valid(:,:), y_test(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred(:,:), y_test_valid(:,:), y_test_pred(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)

    CHARACTER(len=256), allocatable :: fns_x_train(:), fns_x_valid(:), fns_x_test(:)
    CHARACTER(len=256), allocatable :: fns_y_train(:), fns_y_valid(:), fns_y_test(:)
    CHARACTER(len=256) :: fn_x_train, fn_x_test
    CHARACTER(len=256) :: fn_y_train, fn_y_test

    type(metrics) :: metric
    type(standard_scaler) :: ss
    integer(kind=8) :: i, iter, max_iter
    type(isolation_forest) :: iforest
    type(data_holder), target     :: dholder
    type(data_holder), pointer    :: dholder_ptr

    character(len=13) :: feature_selection(3)
    character(len=13) :: split_selection(3)
    character(len=13) :: depth(2)
    logical(kind=4) :: thinning(2)
    integer(kind=8) :: f, s, d, t


    integer(kind=8) :: time_fit, time_pred
    real(kind=8), allocatable :: aucs_trian(:), aucs_test(:)
    real(kind=8), allocatable :: avgp_trian(:), avgp_test(:)


    fns_x_train = [ &
        "../sample_data/creditcard_X_train.bin ", &
        "../sample_data/annthyroid_X_train.bin ", &
        "../sample_data/arrhythmia_X_train.bin ", &
        "../sample_data/breastw_X_train.bin    ", &
        "../sample_data/cardio_X_train.bin     ", &
        "../sample_data/cover_X_train.bin      ", &
        "../sample_data/glass_X_train.bin      ", &
        "../sample_data/http_X_train.bin       ", &
        "../sample_data/ionosphere_X_train.bin ", &
        "../sample_data/letter_X_train.bin     ", &
        "../sample_data/lympho_X_train.bin     ", &
        "../sample_data/mammography_X_train.bin", &
        "../sample_data/mnist_X_train.bin      ", &
        "../sample_data/musk_X_train.bin       ", &
        "../sample_data/optdigits_X_train.bin  ", &
        "../sample_data/pendigits_X_train.bin  ", &
        "../sample_data/pima_X_train.bin       ", &
        "../sample_data/satellite_X_train.bin  ", &
        "../sample_data/satimage-2_X_train.bin ", &
        "../sample_data/shuttle_X_train.bin    ", &
        "../sample_data/smtp_X_train.bin       ", &
        "../sample_data/speech_X_train.bin     ", &
        "../sample_data/thyroid_X_train.bin    ", &
        "../sample_data/vertebral_X_train.bin  ", &
        "../sample_data/vowels_X_train.bin     ", &
        "../sample_data/wbc_X_train.bin        ", &
        "../sample_data/wine_X_train.bin       "  &
    ]

    fns_x_valid = [ &
        "../sample_data/creditcard_X_valid.bin ", &
        "../sample_data/annthyroid_X_valid.bin ", &
        "../sample_data/arrhythmia_X_valid.bin ", &
        "../sample_data/breastw_X_valid.bin    ", &
        "../sample_data/cardio_X_valid.bin     ", &
        "../sample_data/cover_X_valid.bin      ", &
        "../sample_data/glass_X_valid.bin      ", &
        "../sample_data/http_X_valid.bin       ", &
        "../sample_data/ionosphere_X_valid.bin ", &
        "../sample_data/letter_X_valid.bin     ", &
        "../sample_data/lympho_X_valid.bin     ", &
        "../sample_data/mammography_X_valid.bin", &
        "../sample_data/mnist_X_valid.bin      ", &
        "../sample_data/musk_X_valid.bin       ", &
        "../sample_data/optdigits_X_valid.bin  ", &
        "../sample_data/pendigits_X_valid.bin  ", &
        "../sample_data/pima_X_valid.bin       ", &
        "../sample_data/satellite_X_valid.bin  ", &
        "../sample_data/satimage-2_X_valid.bin ", &
        "../sample_data/shuttle_X_valid.bin    ", &
        "../sample_data/smtp_X_valid.bin       ", &
        "../sample_data/speech_X_valid.bin     ", &
        "../sample_data/thyroid_X_valid.bin    ", &
        "../sample_data/vertebral_X_valid.bin  ", &
        "../sample_data/vowels_X_valid.bin     ", &
        "../sample_data/wbc_X_valid.bin        ", &
        "../sample_data/wine_X_valid.bin       "  &
    ]

    fns_x_test = [ &
        "../sample_data/creditcard_X_test.bin ", &
        "../sample_data/annthyroid_X_test.bin ", &
        "../sample_data/arrhythmia_X_test.bin ", &
        "../sample_data/breastw_X_test.bin    ", &
        "../sample_data/cardio_X_test.bin     ", &
        "../sample_data/cover_X_test.bin      ", &
        "../sample_data/glass_X_test.bin      ", &
        "../sample_data/http_X_test.bin       ", &
        "../sample_data/ionosphere_X_test.bin ", &
        "../sample_data/letter_X_test.bin     ", &
        "../sample_data/lympho_X_test.bin     ", &
        "../sample_data/mammography_X_test.bin", &
        "../sample_data/mnist_X_test.bin      ", &
        "../sample_data/musk_X_test.bin       ", &
        "../sample_data/optdigits_X_test.bin  ", &
        "../sample_data/pendigits_X_test.bin  ", &
        "../sample_data/pima_X_test.bin       ", &
        "../sample_data/satellite_X_test.bin  ", &
        "../sample_data/satimage-2_X_test.bin ", &
        "../sample_data/shuttle_X_test.bin    ", &
        "../sample_data/smtp_X_test.bin       ", &
        "../sample_data/speech_X_test.bin     ", &
        "../sample_data/thyroid_X_test.bin    ", &
        "../sample_data/vertebral_X_test.bin  ", &
        "../sample_data/vowels_X_test.bin     ", &
        "../sample_data/wbc_X_test.bin        ", &
        "../sample_data/wine_X_test.bin       "  &
    ]

    fns_y_train = [ &
        "../sample_data/creditcard_y_train.bin ", &
        "../sample_data/annthyroid_y_train.bin ", &
        "../sample_data/arrhythmia_y_train.bin ", &
        "../sample_data/breastw_y_train.bin    ", &
        "../sample_data/cardio_y_train.bin     ", &
        "../sample_data/cover_y_train.bin      ", &
        "../sample_data/glass_y_train.bin      ", &
        "../sample_data/http_y_train.bin       ", &
        "../sample_data/ionosphere_y_train.bin ", &
        "../sample_data/letter_y_train.bin     ", &
        "../sample_data/lympho_y_train.bin     ", &
        "../sample_data/mammography_y_train.bin", &
        "../sample_data/mnist_y_train.bin      ", &
        "../sample_data/musk_y_train.bin       ", &
        "../sample_data/optdigits_y_train.bin  ", &
        "../sample_data/pendigits_y_train.bin  ", &
        "../sample_data/pima_y_train.bin       ", &
        "../sample_data/satellite_y_train.bin  ", &
        "../sample_data/satimage-2_y_train.bin ", &
        "../sample_data/shuttle_y_train.bin    ", &
        "../sample_data/smtp_y_train.bin       ", &
        "../sample_data/speech_y_train.bin     ", &
        "../sample_data/thyroid_y_train.bin    ", &
        "../sample_data/vertebral_y_train.bin  ", &
        "../sample_data/vowels_y_train.bin     ", &
        "../sample_data/wbc_y_train.bin        ", &
        "../sample_data/wine_y_train.bin       "  &
    ]

    fns_y_valid = [ &
        "../sample_data/creditcard_y_valid.bin ", &
        "../sample_data/annthyroid_y_valid.bin ", &
        "../sample_data/arrhythmia_y_valid.bin ", &
        "../sample_data/breastw_y_valid.bin    ", &
        "../sample_data/cardio_y_valid.bin     ", &
        "../sample_data/cover_y_valid.bin      ", &
        "../sample_data/glass_y_valid.bin      ", &
        "../sample_data/http_y_valid.bin       ", &
        "../sample_data/ionosphere_y_valid.bin ", &
        "../sample_data/letter_y_valid.bin     ", &
        "../sample_data/lympho_y_valid.bin     ", &
        "../sample_data/mammography_y_valid.bin", &
        "../sample_data/mnist_y_valid.bin      ", &
        "../sample_data/musk_y_valid.bin       ", &
        "../sample_data/optdigits_y_valid.bin  ", &
        "../sample_data/pendigits_y_valid.bin  ", &
        "../sample_data/pima_y_valid.bin       ", &
        "../sample_data/satellite_y_valid.bin  ", &
        "../sample_data/satimage-2_y_valid.bin ", &
        "../sample_data/shuttle_y_valid.bin    ", &
        "../sample_data/smtp_y_valid.bin       ", &
        "../sample_data/speech_y_valid.bin     ", &
        "../sample_data/thyroid_y_valid.bin    ", &
        "../sample_data/vertebral_y_valid.bin  ", &
        "../sample_data/vowels_y_valid.bin     ", &
        "../sample_data/wbc_y_valid.bin        ", &
        "../sample_data/wine_y_valid.bin       "  &
    ]

    fns_y_test = [ &
        "../sample_data/creditcard_y_test.bin ", &
        "../sample_data/annthyroid_y_test.bin ", &
        "../sample_data/arrhythmia_y_test.bin ", &
        "../sample_data/breastw_y_test.bin    ", &
        "../sample_data/cardio_y_test.bin     ", &
        "../sample_data/cover_y_test.bin      ", &
        "../sample_data/glass_y_test.bin      ", &
        "../sample_data/http_y_test.bin       ", &
        "../sample_data/ionosphere_y_test.bin ", &
        "../sample_data/letter_y_test.bin     ", &
        "../sample_data/lympho_y_test.bin     ", &
        "../sample_data/mammography_y_test.bin", &
        "../sample_data/mnist_y_test.bin      ", &
        "../sample_data/musk_y_test.bin       ", &
        "../sample_data/optdigits_y_test.bin  ", &
        "../sample_data/pendigits_y_test.bin  ", &
        "../sample_data/pima_y_test.bin       ", &
        "../sample_data/satellite_y_test.bin  ", &
        "../sample_data/satimage-2_y_test.bin ", &
        "../sample_data/shuttle_y_test.bin    ", &
        "../sample_data/smtp_y_test.bin       ", &
        "../sample_data/speech_y_test.bin     ", &
        "../sample_data/thyroid_y_test.bin    ", &
        "../sample_data/vertebral_y_test.bin  ", &
        "../sample_data/vowels_y_test.bin     ", &
        "../sample_data/wbc_y_test.bin        ", &
        "../sample_data/wine_y_test.bin       "  &
    ]

    ! Hyperparameters
    feature_selection = feature_selection_list
    split_selection = split_selection_list
    depth = depth_list
    thinning = [t_, f_]

    ! #Iteration
    max_iter = 100
    max_iter = 10
    max_iter = 1

    do i=2, size(fns_x_train), 1
    ! do i=14, size(fns_x_train), 1
        call read_bin_2d(fns_x_train(i), x_train, print_log=f_)
        call read_bin_2d(fns_y_train(i), y_train, print_log=f_)
        
        call read_bin_2d(fns_x_valid(i), x_valid, print_log=f_)
        call read_bin_2d(fns_y_valid(i), y_valid, print_log=f_)
        
        call read_bin_2d(fns_x_test(i), x_test, print_log=f_)
        call read_bin_2d(fns_y_test(i), y_test, print_log=f_)

        ss = standard_scaler()
        call ss%fit(x_train)
        x_train = ss%transform(x_train)
        x_valid = ss%transform(x_valid)
        x_test = ss%transform(x_test)
           
        dholder = data_holder(x_train, y_train)
        dholder_ptr => dholder

        print*, '*********************************************************************************************'
        print*, trim(fns_x_train(i)), shape(x_train)
        do f=1, size(feature_selection), 1
            do s=1, size(split_selection), 1
                do d=1, size(depth), 1
                    do t=1, size(thinning), 1
                        call compute_score_and_time(dholder_ptr, &
                                x_train, y_train, x_valid, y_valid, x_test, y_test, thinning(t), &
                                aucs_trian, aucs_test, avgp_trian, avgp_test, &
                                time_fit, time_pred, max_iter)
                        print*, &
                            (feature_selection(f)), (split_selection(s)), (depth(d)), thinning(t), &
                            real(mean(aucs_trian, max_iter)), real(mean(aucs_test, max_iter)), &
                            real(mean(avgp_trian, max_iter)), real(mean(avgp_test, max_iter)), &
                            int(time_fit), int(time_pred)
                    end do
                end do
            end do
        end do
    end do

contains

    subroutine compute_score_and_time(&
        dholder_ptr, &
        x_train, y_train, &
        x_valid, y_valid, &
        x_test, y_test, thin, &
        auc_train, auc_test, &
        avgp_train, avgp_test, &
        time_fit, time_pred, &
        n_iter)
        implicit none
        type(isolation_forest)      :: iforest
        type(data_holder), pointer  :: dholder_ptr
        real(kind=8), intent(in)    :: x_train(:,:), x_valid(:,:), x_test(:,:)
        integer(kind=8), intent(in) :: y_train(:,:), y_valid(:,:), y_test(:,:)
        logical(kind=4), intent(in) :: thin
        real(kind=8), allocatable, intent(inout) :: auc_train(:), auc_test(:)
        real(kind=8), allocatable, intent(inout) :: avgp_train(:), avgp_test(:)
        integer(kind=8), intent(inout) :: time_fit, time_pred
        integer(kind=8), intent(in) :: n_iter

        type(metrics) :: metric
        real(kind=8), allocatable :: y_pred_train(:,:), y_test_pred(:,:)
        real(kind=8) :: auc_score_train, auc_score_test
        real(kind=8) :: avgp_score_train, avgp_score_test
        integer(kind=8) :: iter, n_estimators

        n_estimators = 100_8
        if (thin) n_estimators = 1000_8
        iforest = isolation_forest(&
                n_estimators=n_estimators, &
                max_samples=256_8, &
                thinning_rate=0.9d0, &
                num_threads=8_8, &
                feature_selection=feature_selection(f), &
                split_selection=split_selection(s), &
                depth=depth(d))
        time_fit = 0
        time_pred = 0
        if (allocated(auc_train)) deallocate(auc_train); allocate(auc_train(n_iter))
        if (allocated(auc_test))  deallocate(auc_test);  allocate(auc_test(n_iter))
        if (allocated(avgp_train)) deallocate(avgp_train); allocate(avgp_train(n_iter))
        if (allocated(avgp_test))  deallocate(avgp_test);  allocate(avgp_test(n_iter))
        do iter=1, n_iter, 1
            ! call progress_bar(iter, n_iter, 1_8)

            call date_and_time(values=date_value1)
            call iforest%fit(dholder_ptr)
            call date_and_time(values=date_value2)
            time_fit = time_fit + time_diff(date_value1, date_value2)

            if (thin) call iforest%thinning(x_train)

            call date_and_time(values=date_value1)
            y_train_pred = iforest%predict(x_train, parallel=t_)
            y_test_pred = iforest%predict(x_test, parallel=t_)
            call date_and_time(values=date_value2)
            time_pred = time_pred + time_diff(date_value1, date_value2)
            
            auc_score_train = metric%auc(y_train(:,1), y_train_pred(:,1))
            auc_score_test  = metric%auc(y_test(:,1),  y_test_pred(:,1))
            auc_train(iter) = auc_score_train
            auc_test(iter) = auc_score_test
                        
            avgp_score_train = metric%average_precision(y_train(:,1), y_train_pred(:,1))
            avgp_score_test  = metric%average_precision(y_test(:,1),  y_test_pred(:,1))
            avgp_train(iter) = avgp_score_train
            avgp_test(iter) = avgp_score_test
        end do
    end subroutine




    function recall_i8(y_true, y_pred) result(recall)
        implicit none
        real(kind=8) :: recall
        integer(kind=8), intent(in) :: y_true(:)
        integer(kind=8), intent(in) :: y_pred(:)

        integer(kind=8) :: n, i, tp, fn
        integer(kind=8) :: t, p, f

        n = size(y_true)

        tp = 0
        fn = 0
        do i=1, n, 1
            t = y_true(i)
            p = y_pred(i)

            f = (t==1_8 .and. p==1_8)
            tp = tp + f

            f = (t==1_8 .and. p==0_8)
            fn = fn + f
        end do

        recall = dble(tp) / dble(tp+fn)
    end function recall_i8

    
    function precision_i8(y_true, y_pred) result(precision)
        implicit none
        real(kind=8) :: precision
        integer(kind=8), intent(in) :: y_true(:)
        integer(kind=8), intent(in) :: y_pred(:)

        integer(kind=8) :: n, i, tp, fp
        integer(kind=8) :: t, p, f

        n = size(y_true)

        tp = 0
        fp = 0
        do i=1, n, 1
            t = y_true(i)
            p = y_pred(i)

            f = (t==1_8 .and. p==1_8)
            tp = tp + f

            f = (t==0_8 .and. p==1_8)
            fp = fp + f
        end do

        precision = dble(tp) / dble(tp+fp)
    end function precision_i8

end program main_forest_isolation_forest