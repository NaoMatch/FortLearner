program main_clustering_minibatch_kmeans
    use mod_minibatch_kmeans
    use mod_kmeans
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8), time_dt, time_et, time_cl, time_lw

    integer(kind=8)    :: n_samples_trains(5), n_columns_trains(5)
    integer(kind=8)    :: n_samples_train, n_columns_train 
    integer(kind=8)    :: n_samples_test, n_columns_test
    logical(kind=4)    :: skip_header
    CHARACTER(len=1)   :: dtype_in, dtype_out
    CHARACTER(len=256) :: file_name_x_train_csv, file_name_y_train_csv
    CHARACTER(len=256) :: file_name_x_train_bin, file_name_y_train_bin
    CHARACTER(len=256) :: file_name_x_test_csv, file_name_y_test_csv
    CHARACTER(len=256) :: file_name_x_test_bin, file_name_y_test_bin
    real(kind=8), ALLOCATABLE :: x_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train(:,:)
    real(kind=8), ALLOCATABLE :: y_train_pred(:,:)
    integer(kind=8), ALLOCATABLE :: feature_indices(:), feature_indices_scanning_range(:)

    real(kind=8) :: score_1, score_2
    type(minibatch_kmeans) :: mbkm, mbkm2
    type(kmeans) :: km, km2

    file_name_x_train_bin = "../sample_data/make_regression_X_0000100000x00100.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    
    print*, '============================================================='
    print*, "Train: Minibatch-Kmeans"
    mbkm = minibatch_kmeans(n_clusters=10_8, max_iter=1000_8, max_samples=512_8)
    call mbkm%fit(x_train)
    print*, mbkm%score(x_train)

    print*, '============================================================='
    print*, "Dump: "
    call mbkm%dump(file_name="mbkm.bin")
    
    print*, '============================================================='
    print*, "Load: "
    call mbkm2%load(file_name="mbkm.bin")
    print*, mbkm2%score(x_train)


end program main_clustering_minibatch_kmeans