program main_mean_variance_covariance
    use mod_timer
    use mod_stats, only: mean, variance, covariance, covariance_matrix
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    integer(kind=8)           :: n_samples, n_columns
    real(kind=8), allocatable :: vector_x(:), vector_y(:), matrix(:,:)
    real(kind=8), allocatable :: mean_cols(:), var_cols(:)
    real(kind=8), allocatable :: covar_mat(:,:)

    real(kind=8)    :: mean_vec_x, mean_vec_y, var_vec, cocar_vec, tmp_mean
    real(kind=8)    :: tmp_mean_i, tmp_mean_j, covar_ij
    integer(kind=8) :: i, j, k

    n_samples = 10000000
    n_columns = 10
    allocate(vector_x(n_samples))
    allocate(vector_y(n_samples))
    allocate(matrix(n_samples, n_columns))
    allocate(mean_cols(n_columns))
    allocate(var_cols(n_columns))
    allocate(covar_mat(n_columns, n_columns))


    print*, '============================================================='
    print*, "Randomization"
    call random_number(vector_x)
    call random_number(vector_y)
    call random_number(matrix)


    print*, '============================================================='
    print*, "VECTOR MEAN: NAIVE"
    call date_and_time(values=date_value1)
    mean_vec_x = sum(vector_x) / dble(n_samples)
    call date_and_time(values=date_value2)
    print*, "    naive time[msec]:", time_diff(date_value1, date_value2)
    print*, "    mean:", mean_vec_x
    
    print*, '============================================================='
    print*, "VECTOR MEAN: My Function"
    call date_and_time(values=date_value1)
    mean_vec_x = mean(vector_x, n_samples)
    call date_and_time(values=date_value2)
    print*, "    my function time[msec]:", time_diff(date_value1, date_value2)
    print*, "    mean:", mean_vec_x


    print*, '============================================================='
    print*, "MATRIX MEAN: NAIVE"
    call date_and_time(values=date_value1)
    mean_cols = sum(matrix, dim=1) / dble(n_samples)
    call date_and_time(values=date_value2)
    print*, "    naive time[msec]:", time_diff(date_value1, date_value2)
    print*, "    mean:", mean_cols
    
    print*, '============================================================='
    print*, "MATRIX MEAN: My Function"
    call date_and_time(values=date_value1)
    mean_cols = mean(matrix, n_samples, n_columns)
    call date_and_time(values=date_value2)
    print*, "    my function time[msec]:", time_diff(date_value1, date_value2)
    print*, "    mean:", mean_cols


    print*, '============================================================='
    print*, "VECTOR Variance: NAIVE"
    call date_and_time(values=date_value1)
    mean_vec_x = sum(vector_x) / dble(n_samples)
    var_vec = 0d0
    do i=1, n_samples, 1
        var_vec = var_vec + (vector_x(i)-mean_vec_x)**2d0
    end do
    var_vec = var_vec / dble(n_samples)
    call date_and_time(values=date_value2)
    print*, "    naive time[msec]:", time_diff(date_value1, date_value2)
    print*, "    variace:", var_vec

    print*, '============================================================='
    print*, "VECTOR Variance: My Function"
    call date_and_time(values=date_value1)
    var_vec = variance(vector_x, n_samples)
    call date_and_time(values=date_value2)
    print*, "    My Function time[msec]:", time_diff(date_value1, date_value2)
    print*, "    variace:", var_vec

    print*, '============================================================='
    print*, "MATRIX Variance: NAIVE"
    call date_and_time(values=date_value1)
    mean_cols = sum(matrix, dim=1) / dble(n_samples)
    var_cols  = 0d0
    do j=1, n_columns, 1
        tmp_mean = mean_cols(j)
        do i=1, n_samples, 1
            var_cols(j) = var_cols(j) + (matrix(i,j)-tmp_mean)**2d0
        end do
    end do
    var_cols = var_cols / dble(n_samples)
    call date_and_time(values=date_value2)
    print*, "    naive time[msec]:", time_diff(date_value1, date_value2)
    print*, "    variace:", var_cols

    print*, '============================================================='
    print*, "MATRIX Variance: My Function"
    call date_and_time(values=date_value1)
    mean_cols = mean(matrix, n_samples, n_columns)
    var_cols = variance(matrix, n_samples, n_columns, mean_cols)
    call date_and_time(values=date_value2)
    print*, "    My Function time[msec]:", time_diff(date_value1, date_value2)
    print*, "    variace:", var_cols


    print*, '============================================================='
    print*, "VECTOR COVariance: NAIVE"
    call date_and_time(values=date_value1)
    mean_vec_x = sum(vector_x) / dble(n_samples)
    mean_vec_y = sum(vector_y) / dble(n_samples)
    cocar_vec = 0d0
    do i=1, n_samples, 1
        cocar_vec = cocar_vec + (vector_x(i)-mean_vec_x)*(vector_y(i)-mean_vec_y)
    end do
    cocar_vec = cocar_vec / dble(n_samples)
    call date_and_time(values=date_value2)
    print*, "    naive time[msec]:", time_diff(date_value1, date_value2)
    print*, "    covariace:", cocar_vec

    print*, '============================================================='
    print*, "VECTOR COVariance: My Function"
    call date_and_time(values=date_value1)
    mean_vec_x = mean(vector_x, n_samples)
    mean_vec_y = mean(vector_y, n_samples)
    cocar_vec = covariance(vector_x, vector_y, n_samples, mean1=mean_vec_x, mean2=mean_vec_y)
    call date_and_time(values=date_value2)
    print*, "    My Function time[msec]:", time_diff(date_value1, date_value2)
    print*, "    covariace:", cocar_vec

    print*, '============================================================='
    print*, "MATRIX COVariance MATRIX: NAIVE"
    call date_and_time(values=date_value1)
    mean_cols = sum(matrix, dim=1) / dble(n_samples)
    covar_mat = 0d0
    do j=1, n_columns, 1
        tmp_mean_j = mean_cols(j)
        do i=j, n_columns, 1
            tmp_mean_i = mean_cols(i) 

            covar_ij = 0d0
            do k=1, n_samples
                covar_ij = covar_ij + (matrix(k,i)-tmp_mean_i) * (matrix(k,j)-tmp_mean_j)
            end do
            covar_ij = covar_ij / dble(n_samples)
            covar_mat(i,j) = covar_ij
            if (i .ne. j) covar_mat(j,i) = covar_ij
        end do
    end do
    call date_and_time(values=date_value2)
    print*, "    naive time[msec]:", time_diff(date_value1, date_value2)
    do i=1, n_columns, 1
        print*, covar_mat(i,:)
    end do

    print*, '============================================================='
    print*, "MATRIX COVariance MATRIX: My Function"
    call date_and_time(values=date_value1)
    mean_cols = mean(matrix, n_samples, n_columns)
    call covariance_matrix(covar_mat, matrix, n_samples, n_columns, mean_cols)
    call date_and_time(values=date_value2)
    print*, "    my function time[msec]:", time_diff(date_value1, date_value2)
    print*, "    covariace:"
    do i=1, n_columns, 1
        print*, covar_mat(i,:)
    end do

end program main_mean_variance_covariance
