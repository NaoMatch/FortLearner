do i=2, num, 1
    factor = vector(i-1) .ne. vector(i)
    tmp_count = tmp_count + factor
end do