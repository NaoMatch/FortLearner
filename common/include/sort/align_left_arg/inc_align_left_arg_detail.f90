pivot = vector1(1)
i = 1
j = n_samples
is_same(:) = vector1(:) .eq. pivot

do
    do while (is_same(i))
        i=i+1
    end do
    do while (.not. is_same(j))
        j=j-1
    end do
    if (i >= j) exit
    tmp1 = vector1(i);  vector1(i) = vector1(j);  vector1(j) = tmp1
    tmp2 = vector2(i);  vector2(i) = vector2(j);  vector2(j) = tmp2
    i=i+1
    j=j-1
end do
