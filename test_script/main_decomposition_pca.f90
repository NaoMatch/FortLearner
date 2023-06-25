program main_decomposition_pca
    use mod_pca
    use mod_pca
    use mod_timer
    implicit none
    
    integer(kind=8)        :: date_value1(8), date_value2(8)
    real(kind=8), allocatable :: x(:,:), x_trans_pca(:,:), x_trans_pca(:,:)
    type(pca) :: decomp_pca
    type(pca) :: decomp_pca
    integer(kind=8) :: i, j

    decomp_pca = pca(n_components=3_8, tolerance=1d-6, max_iteration=10000_8)
    decomp_pca = pca()

    allocate(x(100,8))
    call random_number(x)

    call date_and_time(values=date_value1)
    x_trans_pca = decomp_pca%fit_transform(x)
    call date_and_time(values=date_value2)
    print*, "pca: ", time_diff(date_value1, date_value2)
        
    call date_and_time(values=date_value1)
    call decomp_pca%fit(x)
    x_trans_pca = decomp_pca%transform(x)
    call date_and_time(values=date_value2)
    print*, "PCA: ", time_diff(date_value1, date_value2)


    print*, '*********************************************************************************************'
    print*, "pca: head 10 rows"
    do i=1, 10, 1
        print*, x_trans_pca(i,:)
    end do
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    print*, "PCA: head 10 rows"
    do i=1, 10, 1
        print*, x_trans_pca(i,:)
    end do

    call date_and_time(values=date_value1)
    x_trans_pca = decomp_pca%fit_transform(x)
    call date_and_time(values=date_value2)
    print*, "pca: ", time_diff(date_value1, date_value2)
        
    call date_and_time(values=date_value1)
    call decomp_pca%fit(x)
    x_trans_pca = decomp_pca%transform(x)
    call date_and_time(values=date_value2)
    print*, "PCA: ", time_diff(date_value1, date_value2)


    print*, '*********************************************************************************************'
    print*, "pca: head 10 rows"
    do i=1, 10, 1
        print*, x_trans_pca(i,:)
    end do
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    print*, '*********************************************************************************************'
    print*, "PCA: head 10 rows"
    do i=1, 10, 1
        print*, x_trans_pca(i,:)
    end do



end program main_decomposition_pca