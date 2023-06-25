program main_clustering_kmeans
    use mod_kmeans
    use mod_data_holder
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

    type(kmeans) :: km, km2
    type(data_holder) :: dholder

    file_name_x_train_bin = "../sample_data/make_regression_X_train_0000100000x00100.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    x_train = x_train(:,:10)
    dholder = data_holder(x_train)
    
    print*, '============================================================='
    print*, "Train: "
    km = kmeans(n_clusters=10_8, random_state=42_8)
    call date_and_time(values=date_value1)
    call km%fit(x_train)
    call date_and_time(values=date_value2)
    print*, "fit:       ", km%score(x_train), time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    call km%fit(dholder)
    call date_and_time(values=date_value2)
    print*, "fit2:      ", km%score(x_train), time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    call km%fit_dgemm(x_train)
    call date_and_time(values=date_value2)
    print*, "fit_dgemm: ", km%score(x_train), time_diff(date_value1, date_value2)

    call date_and_time(values=date_value1)
    call km%fit_dgemm(dholder)
    call date_and_time(values=date_value2)
    print*, "fit_dgemm2:", km%score(x_train), time_diff(date_value1, date_value2)
    
    call date_and_time(values=date_value1)
    call km%fit_dgemv(x_train)
    call date_and_time(values=date_value2)
    print*, "fit_dgemv: ", km%score(x_train), time_diff(date_value1, date_value2)
    
    call date_and_time(values=date_value1)
    call km%fit_dgemv(dholder)
    call date_and_time(values=date_value2)
    print*, "fit_dgemv2:", km%score(x_train), time_diff(date_value1, date_value2)
    
    call date_and_time(values=date_value1)
    call km%fit_elkan(x_train)
    call date_and_time(values=date_value2)
    print*, "fit_elkan: ", km%score(x_train), time_diff(date_value1, date_value2), ", Not consistent with the above results."
    
    call date_and_time(values=date_value1)
    call km%fit_elkan(dholder)
    call date_and_time(values=date_value2)
    print*, "fit_elkan2:", km%score(x_train), time_diff(date_value1, date_value2), ", Not consistent with the above results."
    
    print*, '============================================================='
    print*, "Dump: "
    call km%dump(file_name="km.bin")
    
    print*, '============================================================='
    print*, "Load: "
    call km2%load(file_name="km.bin")
    print*, km2%score(x_train)


end program main_clustering_kmeans