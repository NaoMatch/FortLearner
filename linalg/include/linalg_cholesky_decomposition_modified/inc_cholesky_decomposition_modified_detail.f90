diagonal_elements = 0.0
diagonal_elements(1) = matrix(1,1)
call identity(matrix_lower, n_dim)
do i=2, n_dim
    matrix_lower(i,1) = matrix(i,1) / diagonal_elements(1)
end do

do i=2, n_dim
    do j=2, i-1, 1
        tmp_sum = matrix(i,j)
        do k=1, j-1, 1
            tmp_sum = tmp_sum - matrix_lower(i,k) * matrix_lower(j,k) * diagonal_elements(k)
        end do
        matrix_lower(i,j) = tmp_sum / diagonal_elements(j)
    end do
    diagonal_elements(i) = matrix(i,i) - sum(matrix_lower(i,1:i-1)**2.0 * diagonal_elements(1:i-1))
end do
