do i = 2, num
    tmp1_i = vector1(i)
    if (vector1(i-1) .le. tmp1_i) cycle

    tmp2_i = vector2(i)
    j = i - 1
    do while (j .ge. 1)
        tmp1_j = vector1(j)
        if (tmp1_j .le. tmp1_i) exit

        tmp2_j = vector2(j)
        vector1(j + 1) = tmp1_j
        vector2(j + 1) = tmp2_j
        j = j - 1
    end do
    vector1(j + 1) = tmp1_i
    vector2(j + 1) = tmp2_i
end do
