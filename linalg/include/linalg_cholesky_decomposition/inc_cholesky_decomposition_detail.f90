matrix_lower = 0.0
matrix_lower(1,1) = sqrt(matrix(1,1) + 0.0d0)
do i=2, n_dim
    matrix_lower(i,1) = matrix(i,1) / matrix_lower(1,1)
end do

do i=2, n_dim
    do j=2, i-1, 1
        tmp_sum = matrix(i,j)
        do k=1, j-1, 1
            tmp_sum = tmp_sum - matrix_lower(i,k) * matrix_lower(j,k)
        end do
        matrix_lower(i,j) = tmp_sum / matrix_lower(j,j)
    end do
    matrix_lower(i,i) = sqrt( matrix(i,i) - sum(matrix_lower(i,:)**2.0) + 0.0d0)
end do
