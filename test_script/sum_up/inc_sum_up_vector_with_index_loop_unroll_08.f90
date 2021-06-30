r15 = 0
n_ids_unroll = n_ids - mod(n_ids, 8)
do i=1, n_ids_unroll, 8
    r00 = x(ids(i))
    r01 = x(ids(i+1))
    r02 = x(ids(i+2))
    r03 = x(ids(i+3))
    r04 = x(ids(i+4))
    r05 = x(ids(i+5))
    r06 = x(ids(i+6))
    r07 = x(ids(i+7))
    r00 = r00 + r01
    r02 = r02 + r03
    r04 = r04 + r05
    r06 = r06 + r07
    r00 = r00 + r02
    r04 = r04 + r06
    r00 = r00 + r04
    r15 = r15 + r00
end do

do i=n_ids_unroll+1, n_ids, 1
    r00 = x(ids(i))
    r15 = r15 + r00
end do
