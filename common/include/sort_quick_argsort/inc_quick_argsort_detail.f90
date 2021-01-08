if ( num .le. 4500 ) then
    if (num .le. 31) then
        call insertion_argsort(vector1(1:num), vector2(1:num), num)
        return
    end if

    pivot = vector1((1+num)/2)
    i = 1
    j = num
    do
        do while (vector1(i) < pivot)
            i=i+1
        end do
        do while (pivot < vector1(j))
            j=j-1
        end do
        if (i .ge. j) exit
        tmp1 = vector1(i);  vector1(i) = vector1(j);  vector1(j) = tmp1
        tmp2 = vector2(i);  vector2(i) = vector2(j);  vector2(j) = tmp2
        i=i+1
        j=j-1
    end do
else
    if (num .le. 63) then
        call insertion_argsort(vector1(1:num), vector2(1:num), num)
        return
    end if

    one = 1
    three = 3
    pivots(1) = vector1(1)
    pivots(2) = vector1((1+num)/2)
    pivots(3) = vector1(num)
    call insertion_sort(pivots, three)
    pivot = pivots(3)

    i = 1
    j = num
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
end if

