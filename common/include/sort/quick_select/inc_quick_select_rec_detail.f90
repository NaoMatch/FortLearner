if (n_samples .le. 32_8) then
    call insertion_sort(vector, n_samples)
    return
end if

idx = (1_8+n_samples) / 2_8
pivot = vector(idx)
i = 1
j = n_samples

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

if (n_th .le. j) then
    call quick_select_rec(vector(1:j), j, n_th)
else
    call quick_select_rec(vector(i:), n_samples-i+1, n_th-i+1)
end if
