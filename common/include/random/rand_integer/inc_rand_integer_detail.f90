factor = hi - lo + 1
allocate(tmp_array(num))
call random_number(tmp_array)
unroll = num - mod(num, 15)
do i=1, unroll, 15
    do j=0, 15-1, 1
        buffer(j+1) = int(factor * tmp_array(i+j), kind=kind(factor)) + lo
    end do

    do j=0, 15-1, 1
        vector(i+j) = buffer(j+1)
    end do
end do
do i=unroll+1, num
    vector(i) = int(factor * tmp_array(i), kind=kind(factor)) + lo
end do
deallocate(tmp_array)
