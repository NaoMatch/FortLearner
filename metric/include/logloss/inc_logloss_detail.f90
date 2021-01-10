one = 1.0
eposilon = 0.0000001

y_size_true = size(y_true)
y_size_pred = size(y_pred)
call this%check_same_sample_size(y_size_true, y_size_pred)
call this%check_non_empty_vector(y_size_true, y_size_pred)
call this%check_binary_classification(y_true)
call this%check_proba_range(y_pred)

tmp = 0.0
do i=1, y_size_true, 1
    label = real(y_true(i), kind=kind(label))
    proba = y_pred(i)
    proba = minval((/one, maxval((/proba, eposilon/))/))
    tmp = tmp &
        - (        label  * log(      proba) & 
            + (1.0 - label) * log(1.0 - proba))
end do
