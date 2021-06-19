r15 = 0
n_ids_unroll = n_ids - mod(n_ids, 2)
do i=1, n_ids_unroll, 2
    r00 = x(ids(i))
    r01 = x(ids(i+1))
    r00 = r00 + r01
    r15 = r15 + r00
end do

do i=n_ids_unroll+1, n_ids, 1
    r00 = x(ids(i))
    r15 = r15 + r00
end do
