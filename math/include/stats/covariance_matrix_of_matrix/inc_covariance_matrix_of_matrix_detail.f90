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
                                + (real(matrix(l,i), kind=kind(tmp))   - means_of_matrix_opt(i)) & 
                                * (real(matrix(l,j+k), kind=kind(tmp)) - means_of_matrix_opt(j))
            end do
        end do
        do k=0, 7-1, 1
            cov_mat(i,j+k) = tmp_v1xv2(k+1)/real(n_rows, kind=kind(tmp))
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
