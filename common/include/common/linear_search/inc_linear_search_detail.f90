do i=1, n_samples
    if ( value .le. vector(i) ) then
        tmp = i
        exit
    end if
end do
