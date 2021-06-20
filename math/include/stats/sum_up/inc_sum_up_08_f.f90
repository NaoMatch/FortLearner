r15 = 0d0

do i=1, num_unroll, 8
    r00 = x(i)
    r01 = x(i+1)
    r02 = x(i+2)
    r03 = x(i+3)
    r04 = x(i+4)
    r05 = x(i+5)
    r06 = x(i+6)
    r07 = x(i+7)

    r00 = r00 + r01
    r02 = r02 + r03
    r04 = r04 + r05
    r06 = r06 + r07

    r00 = r00 + r02
    r04 = r04 + r06
    
    r00 = r00 + r04
    
    r15 = r15 + r00
end do

do i=num_unroll+1, n, 1
    r15 = r15 + x(i)
end do
