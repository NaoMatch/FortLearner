unroll_size=8
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

r00=0
r01=0
r02=0
r03=0
r04=0
r05=0
r06=0
r07=0
do i=1, n_unroll, unroll_size
    if (y(i) .le. threshold_y) then
        r00 = r00 + x(i)
    end if

    if (y(i+1) .le. threshold_y) then
        r01 = r01 + x(i+1)
    end if

    if (y(i+2) .le. threshold_y) then
        r02 = r02 + x(i+2)
    end if

    if (y(i+3) .le. threshold_y) then
        r03 = r03 + x(i+3)
    end if

    if (y(i+4) .le. threshold_y) then
        r04 = r04 + x(i+4)
    end if

    if (y(i+5) .le. threshold_y) then
        r05 = r05 + x(i+5)
    end if

    if (y(i+6) .le. threshold_y) then
        r06 = r06 + x(i+6)
    end if

    if (y(i+7) .le. threshold_y) then
        r07 = r07 + x(i+7)
    end if
end do

tmp_sum=r00+r01+r02+r03+r04+r05+r06+r07
if (n_remain .gt. 0_8) then
    do i=n_unroll+1, n_samples, 1
        if (y(i) .le. threshold_y) then
            tmp_sum = tmp_sum + x(i)
        end if
    end do
end if