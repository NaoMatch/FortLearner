if (n_samples .le. 32_8) then
    call insertion_argsort(vector1, vector2, n_samples)
    return
end if

idx = (1_8+n_samples) / 2_8
pivot = vector1(idx)
i = 1
j = n_samples

do
    do while (vector1(i) < pivot)
        i=i+1
    end do
    do while (pivot < vector1(j))
        j=j-1
    end do
    if (i >= j) exit
    tmp1 = vector1(i);  vector1(i) = vector1(j);  vector1(j) = tmp1
    tmp2 = vector2(i);  vector2(i) = vector2(j);  vector2(j) = tmp2
    i=i+1
    j=j-1
end do

if (n_th .le. j) then
    call quick_argselect_rec(vector1(1:j), vector2(1:j), j, n_th)
else
    call quick_argselect_rec(vector1(i:),  vector2(i:), n_samples-i+1, n_th-i+1)
end if
