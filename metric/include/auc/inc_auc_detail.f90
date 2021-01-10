y_size_true = size(y_true)
y_size_pred = size(y_pred)

call this%check_same_sample_size(y_size_true, y_size_pred)
call this%check_non_empty_vector(y_size_true, y_size_pred)
call this%check_binary_classification(y_true)
call this%check_proba_range(y_pred)

allocate(indices(y_size_true))
count_false = 0
tmp = 0
do i=1, y_size_true
    indices(i) = i
end do

min_label = minval(y_true)
call quick_argsort(y_pred, indices, y_size_true)
do i=1, y_size_true
    idx      = indices(i)
    y_true_i = y_true(idx) - min_label
    count_false  = count_false + (1 - y_true_i)
    tmp      = tmp + y_true_i * count_false
end do
tmp = tmp / ( count_false * (y_size_true - count_false) )
call quick_argsort(indices, y_pred, y_size_true)
