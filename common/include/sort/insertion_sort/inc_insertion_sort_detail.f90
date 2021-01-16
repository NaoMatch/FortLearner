do i = 2, num
    tmp_i = vector(i)
    if (vector(i-1) .le. tmp_i) cycle
    j = i - 1
    do while (j .ge. 1)
        tmp_j = vector(j)
        if (tmp_j .le. tmp_i) exit
        vector(j + 1) = tmp_j
        j = j - 1
    end do
    vector(j + 1) = tmp_i
end do
