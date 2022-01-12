pivot = vector(1)
i = 1
j = n_samples
is_same(:) = vector(:) .eq. pivot

do
    do while (is_same(i))
        i=i+1
    end do
    do while (.not. is_same(j))
        j=j-1
    end do
    if (i >= j) exit
    tmp = vector(i);  vector(i) = vector(j);  vector(j) = tmp
    i=i+1
    j=j-1
end do

