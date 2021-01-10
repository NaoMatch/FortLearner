program main_metric
    use mod_metric, only: metrics
    implicit none

    integer(kind=4) :: n_samples, n_class
    real(kind=4), ALLOCATABLE :: y_true_r4(:), y_pred_r4(:)
    integer(kind=4), ALLOCATABLE :: y_true_i4(:), y_pred_i4(:)
    type(metrics) :: metric

    n_class = 2
    n_samples = 1000000
    allocate(y_true_r4(n_samples))
    allocate(y_pred_r4(n_samples))
    allocate(y_true_i4(n_samples))
    allocate(y_pred_i4(n_samples))

    print*, '============================================================='
    print*, "Randomizaion (DUMMY DATA!) "
    call RANDOM_NUMBER(y_true_r4)    
    call RANDOM_NUMBER(y_pred_r4)
    y_true_i4 = int(y_true_r4*n_class, kind=4)
    y_pred_i4 = int(y_pred_r4*n_class, kind=4)


    print*, '============================================================='
    print*, "Mean Square Error"
    print*, "    ", metric%mean_square_error(y_true_r4, y_pred_r4)
    

    print*, '============================================================='
    print*, "Root Mean Square Error"
    print*, "    ", metric%root_mean_square_error(y_true_r4, y_pred_r4)
    

    print*, '============================================================='
    print*, "Accuracy"
    print*, "    ", metric%accuracy(y_true_i4, y_pred_i4)
    

    print*, '============================================================='
    print*, "ROC_AUC"
    print*, "    ", metric%auc(y_true_i4, y_pred_r4)


    print*, '============================================================='
    print*, "LogLoss"
    print*, "    ", metric%logloss(y_true_i4, y_pred_r4)


end program main_metric
