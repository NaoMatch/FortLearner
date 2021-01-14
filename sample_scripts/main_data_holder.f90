program main_data_holder
    use mod_data_holder
    implicit none

    integer(kind=8) :: n_uniques
    integer(kind=8) :: n_samples, n_columns, n_outputs
    real(kind=8), ALLOCATABLE :: x_train(:,:), y_train(:,:)
    type(data_holder) :: dholder

    n_samples = 90000
    n_uniques = sqrt(n_samples+0.0)
    n_columns = 28
    n_outputs = 2

    print*, '============================================================='
    print*, "MATRIX: ", n_samples, n_columns
    allocate(x_train(n_samples, n_columns))
    allocate(y_train(n_samples, n_outputs))

    print*, '============================================================='
    print*, "RANDOMIZATION"
    call RANDOM_NUMBER(x_train)
    call RANDOM_NUMBER(y_train)
    x_train = int(x_train * n_uniques, kind=8)
    y_train = int(y_train * n_uniques, kind=8)

    print*, '============================================================='
    print*, "Data Holder"
    dholder = data_holder(x_train, y_train)

    print*, '============================================================='
    print*, "Random Rotation"
    call dholder%preprocess_random_rotate()

    print*, '============================================================='
    print*, "Histgram: uniform"
    call dholder%preprocess_hist(max_bins=10_8, strategy="uniform")

    print*, '============================================================='
    print*, "Histgram: quantile"
    call dholder%preprocess_hist(max_bins=10_8, strategy="quantile")

    print*, '============================================================='
    print*, "Histgram: kmeans"
    call dholder%preprocess_hist(max_bins=10_8, strategy="kmeans")

    print*, '============================================================='
    print*, "Histgram: greedy"
    call dholder%preprocess_hist(max_bins=10_8, strategy="greedy")

    print*, '============================================================='
    print*, "Histgram: modified_greedy"
    call dholder%preprocess_hist(max_bins=10_8, strategy="modified_greedy")


    ! Error
    ! print*, '============================================================='
    ! print*, "Histgram: hogehoge"
    ! call dholder%preprocess_hist(max_bins=10_8, strategy="hogehoge")

    

end program main_data_holder
