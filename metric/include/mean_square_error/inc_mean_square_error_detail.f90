y_size_true = size(y_true)
y_size_pred = size(y_pred)
call this%check_same_sample_size(y_size_true, y_size_pred)
call this%check_non_empty_vector(y_size_true, y_size_pred)

tmp_sq_sum = 0
do i=1, y_size_true, 1
    tmp_sq_sum = tmp_sq_sum + (y_true(i)-y_pred(i)) ** 2.0
end do