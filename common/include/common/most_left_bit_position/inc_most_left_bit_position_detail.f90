do i=i_left, 0, -1
    if ( btest(val, i) ) then
        tmp = i
        exit
    end if
end do
