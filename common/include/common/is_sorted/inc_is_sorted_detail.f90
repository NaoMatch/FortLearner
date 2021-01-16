ascending_opt = t_
if ( present(ascending) ) ascending_opt = ascending
tmp_result = t_
if ( ascending_opt ) then
    do i=1, num-1, 1
        if ( vector(i) .gt. vector(i+1) ) then
            tmp_result = f_
            exit
        end if
    end do
else
    do i=1, num-1, 1
        if ( vector(i) .lt. vector(i+1) ) then
            tmp_result = f_
            exit
        end if
    end do
end if
