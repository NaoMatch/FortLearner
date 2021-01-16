! Fast Return
if (num_all .eq. num_bottom) then
    val_lower = maxval(vector)
    return
end if

! Fast-Retrurn 2
if ( num_all .le. 32_8 ) then
    call insertion_sort(vector, num_all)
    val_lower = vector(num_bottom)
    return
end if

pivot = vector((1+num_all)/2)
i = 1
j = num_all
do
    do while (vector(i) < pivot)
        i=i+1
    end do
    do while (pivot < vector(j))
        j=j-1
    end do
    if (i >= j) exit
    tmp = vector(i);  vector(i) = vector(j);  vector(j) = tmp
    i=i+1
    j=j-1
end do

num_lower = i-1
if ( num_lower .lt. num_bottom ) then
    num_bottom = num_bottom - num_lower
    num_upper = num_all - num_lower
    call quick_select_lower(val_lower, vector(num_lower+1:), num_upper, num_bottom)
elseif (num_lower .ge. num_bottom) then
    call quick_select_lower(val_lower, vector(1:num_lower),  num_lower, num_bottom)
end if
