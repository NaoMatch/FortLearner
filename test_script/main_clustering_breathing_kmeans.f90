program main_clustering_breathing_kmeans
    use mod_breathing_kmeans
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

    type(breathing_kmeans) :: km, km2

    file_name_x_train_bin = "../sample_data/make_regression_X_train_0000010000x00100.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    
    print*, '============================================================='
    print*, "Train: "
    km = breathing_kmeans(n_clusters=10_8, n_clusters_breathing_in=5_8)
    call km%fit(x_train)
    print*, km%score(x_train)
    
    print*, '============================================================='
    print*, "Dump: "
    call km%dump(file_name="km.bin")
    
    print*, '============================================================='
    print*, "Load: "
    call km2%load(file_name="km.bin")
    print*, km2%score(x_train)

end program main_clustering_breathing_kmeans