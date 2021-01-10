matrix_inv = 0
do j=1, n_dim, 1
    matrix_inv(j,j) = 1
    do i = j+1, n_dim, 1
        matrix_inv(i,j) = - sum(matrix_unit_lower(i,j:i-1)*matrix_inv(j:i-1, j))
    end do
end do
