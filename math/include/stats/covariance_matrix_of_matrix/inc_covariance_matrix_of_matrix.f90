subroutine covariance_matrix_of_matrix_r8(cov_mat, matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    real(kind=8), intent(inout) :: cov_mat(n_cols,n_cols)
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8), optional      :: means_of_matrix(n_cols)

    integer(kind=8) :: i, j, k, l, n_cols_unroll
    real(kind=8) :: means_of_matrix_opt(n_cols)
    real(kind=8) :: var_vals(n_cols)
    real(kind=8) :: tmp_v1xv2(7)

    if ( present(means_of_matrix) ) then
        means_of_matrix_opt = means_of_matrix
    else
        means_of_matrix_opt = mean(matrix, n_rows, n_cols)
    end if
    var_vals  = variance(matrix, n_rows, n_cols, means_of_matrix=means_of_matrix_opt)
    do i=1, n_cols, 1
        cov_mat(i,i) = var_vals(i)
    end do

    n_cols_unroll = n_cols - mod(n_cols, 7)
    do j=1, n_cols_unroll, 7
        do i=j+1, n_cols, 1
            tmp_v1xv2 = 0
            do l=1, n_rows, 1
                do k=0, 7-1
                    tmp_v1xv2(k+1) = tmp_v1xv2(k+1) & 
                                   + (matrix(l,i)   - means_of_matrix_opt(i)) & 
                                   * (matrix(l,j+k) - means_of_matrix_opt(j))
                end do
            end do
            do k=0, 7-1, 1
                cov_mat(i,j+k) = tmp_v1xv2(k+1)/dble(n_rows-1)
            end do
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        do i=j+1, n_cols, 1
            cov_mat(i,j) = covariance(matrix(:,i), matrix(:,j), n_rows, means_of_matrix_opt(i), means_of_matrix_opt(j))
        end do
    end do

    do j=1, n_cols, 1
        do i=j+1, n_cols
            cov_mat(j,i) = cov_mat(i,j)
        end do
    end do
end subroutine covariance_matrix_of_matrix_r8

subroutine covariance_matrix_of_matrix_i4(cov_mat, matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    real(kind=4), intent(inout) :: cov_mat(n_cols,n_cols)
    integer(kind=4), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=4), intent(in) :: n_rows, n_cols
    real(kind=4), optional      :: means_of_matrix(n_cols)

    integer(kind=4) :: i, j, k, l, n_cols_unroll
    real(kind=4) :: means_of_matrix_opt(n_cols)
    real(kind=4) :: var_vals(n_cols)
    real(kind=4) :: tmp_v1xv2(7)

    if ( present(means_of_matrix) ) then
        means_of_matrix_opt = means_of_matrix
    else
        means_of_matrix_opt = mean(matrix, n_rows, n_cols)
    end if
    var_vals  = variance(matrix, n_rows, n_cols, means_of_matrix=means_of_matrix_opt)
    do i=1, n_cols, 1
        cov_mat(i,i) = var_vals(i)
    end do

    n_cols_unroll = n_cols - mod(n_cols, 7)
    do j=1, n_cols_unroll, 7
        do i=j+1, n_cols, 1
            tmp_v1xv2 = 0
            do l=1, n_rows, 1
                do k=0, 7-1
                    tmp_v1xv2(k+1) = tmp_v1xv2(k+1) + float(matrix(l,i)*matrix(l,j+k))
                end do
            end do
            do k=0, 7-1, 1
                cov_mat(i,j+k) = tmp_v1xv2(k+1)/float(n_rows) - means_of_matrix_opt(i)*means_of_matrix_opt(j+k)
            end do
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        do i=j+1, n_cols, 1
            cov_mat(i,j) = covariance(matrix(:,i), matrix(:,j), n_rows, means_of_matrix_opt(i), means_of_matrix_opt(j))
        end do
    end do


    do j=1, n_cols, 1
        do i=j+1, n_cols
        cov_mat(j,i) = cov_mat(i,j)
        end do
    end do
end subroutine covariance_matrix_of_matrix_i4

subroutine covariance_matrix_of_matrix_i8(cov_mat, matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    real(kind=8), intent(inout) :: cov_mat(n_cols,n_cols)
    integer(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8), optional      :: means_of_matrix(n_cols)

    integer(kind=8) :: i, j, k, l, n_cols_unroll
    real(kind=8) :: means_of_matrix_opt(n_cols)
    real(kind=8) :: var_vals(n_cols)
    real(kind=8) :: tmp_v1xv2(7)

    if ( present(means_of_matrix) ) then
        means_of_matrix_opt = means_of_matrix
    else
        means_of_matrix_opt = mean(matrix, n_rows, n_cols)
    end if
    var_vals  = variance(matrix, n_rows, n_cols, means_of_matrix=means_of_matrix_opt)
    do i=1, n_cols, 1
        cov_mat(i,i) = var_vals(i)
    end do

    n_cols_unroll = n_cols - mod(n_cols, 7)
    do j=1, n_cols_unroll, 7
        do i=j+1, n_cols, 1
            tmp_v1xv2 = 0
            do l=1, n_rows, 1
                do k=0, 7-1
                    tmp_v1xv2(k+1) = tmp_v1xv2(k+1) + dble(matrix(l,i)*matrix(l,j+k))
                end do
            end do
            do k=0, 7-1, 1
                cov_mat(i,j+k) = tmp_v1xv2(k+1)/dble(n_rows-1) - means_of_matrix_opt(i)*means_of_matrix_opt(j+k)
            end do
        end do
    end do

    do j=n_cols_unroll+1, n_cols, 1
        do i=j+1, n_cols, 1
            cov_mat(i,j) = covariance(matrix(:,i), matrix(:,j), n_rows, means_of_matrix_opt(i), means_of_matrix_opt(j))
        end do
    end do

    do j=1, n_cols, 1
        do i=j+1, n_cols
        cov_mat(j,i) = cov_mat(i,j)
        end do
    end do
end subroutine covariance_matrix_of_matrix_i8
