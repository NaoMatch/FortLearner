if ( allocated(mat_out) ) deallocate(mat_out)
if ( with_intercept ) then
    allocate(mat_out(n_columns+1, n_columns+1))
else
    allocate(mat_out(n_columns, n_columns))
end if

do j=1, n_columns, 1
    j_unroll = j - mod(j, buffer_size)
    do i=1, j_unroll, buffer_size
        tmp_sums = zero
        do k=1, n_samples, 1
            tmp_val = mat_in(k,j)
            do l=0, buffer_size-1, 1
                tmp_sums(l+1) = tmp_sums(l+1) + mat_in(k,i+l) * tmp_val
            end do
        end do

        do l=0, buffer_size-1, 1
            mat_out(i+l,j) = tmp_sums(l+1)
            mat_out(j,i+l) = tmp_sums(l+1)
        end do
    end do

    do i=j_unroll+1, j
        tmp_sum = zero
        do k=1, n_samples, 1
            tmp_sum = tmp_sum + mat_in(k,i) * mat_in(k,j)
        end do

        mat_out(i,j) = tmp_sum
        mat_out(j,i) = tmp_sum
    end do

end do


if ( with_intercept ) then
    call sum_of_matrix(col_sums, mat_in, n_samples, n_columns, one)
    do j=1, n_columns, 1
        mat_out(j,n_columns+1) = col_sums(j)
        mat_out(n_columns+1,j) = col_sums(j)
    end do
    mat_out(n_columns+1, n_columns+1) = n_samples
end if
