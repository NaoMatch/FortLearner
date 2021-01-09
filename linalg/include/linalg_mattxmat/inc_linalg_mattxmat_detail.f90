if ( allocated(mat_out) ) deallocate(mat_out)

if ( with_intercept ) then
    allocate(mat_out(n_cols+1, n_cols+1))
    allocate(tmp_sum(n_cols))
    tmp_sum = sum(mat_in, dim=1)
    mat_out(1:n_cols, 1:n_cols) = matmul(transpose(mat_in), mat_in)
    mat_out(n_cols+1,1:n_cols) = tmp_sum
    mat_out(1:n_cols,n_cols+1) = tmp_sum
    mat_out(n_cols+1, n_cols+1) = n_rows
else
    allocate(mat_out(n_cols, n_cols))
    mat_out(:, :) = matmul(transpose(mat_in), mat_in)
end if
