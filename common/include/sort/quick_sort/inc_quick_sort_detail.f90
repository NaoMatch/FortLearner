if ( num .le. 4500 ) then
    if (num .le. 31) then
        call insertion_sort(vector, num)
        return
    end if

    pivot = vector((1+num)/2)
    i = 1
    j = num
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
else
    if (num .le. 127) then
        call insertion_sort(vector, num)
        return
    end if

    one = 1
    three = 3
    pivots(1) = vector(1)
    pivots(2) = vector((1+num)/2)
    pivots(3) = vector(num)
    call insertion_sort(pivots, three)
    pivot = pivots(2)

    i = 1
    j = num
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
end if