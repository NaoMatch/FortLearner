r15 = 0
n_ids_unroll = n_ids - mod(n_ids, 4)
do i=1, n_ids_unroll, 4
    r00 = x(ids(i))
    r01 = x(ids(i+1))
    r02 = x(ids(i+2))
    r03 = x(ids(i+3))
    r00 = r00 + r01
    r02 = r02 + r03
    r00 = r00 + r02
    r15 = r15 + r00
end do

do i=n_ids_unroll+1, n_ids, 1
    r00 = x(ids(i))
    r15 = r15 + r00
end do
