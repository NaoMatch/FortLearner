y_size_true = size(y_true)
y_size_pred = size(y_pred)

call this%check_same_sample_size(y_size_true, y_size_pred)
call this%check_non_empty_vector(y_size_true, y_size_pred)

count_correct = 0
do i=1, y_size_true, 1
    factor = y_true(i) .eq. y_pred(i)
    count_correct = count_correct + factor
end do
