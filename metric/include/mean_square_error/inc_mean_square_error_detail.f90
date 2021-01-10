num_y_true = size(y_true)
num_y_pred = size(y_pred)
if (num_y_true .ne. num_y_pred) then
    stop "Size of y_true and y_pred missmatch."
end if

tmp_sq_sum = 0
do i=1, num_y_true, 1
    tmp_sq_sum = tmp_sq_sum + (y_true(i)-y_pred(i)) ** 2.0
end do