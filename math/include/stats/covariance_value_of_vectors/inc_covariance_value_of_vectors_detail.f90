tmp_sum = 0
if ( present(mean1) ) then
    mean1_opt = mean1
else
    mean1 = mean(vector1, num)
end if
if ( present(mean2) ) then
    mean2_opt = mean2
else
    mean2 = mean(vector2, num)
end if

do i=1, num, 1
    tmp_sum = tmp_sum & 
        + (real(vector1(i), kind=kind(mean1_opt)) - mean1_opt) &
        * (real(vector2(i), kind=kind(mean2_opt)) - mean2_opt)
end do
