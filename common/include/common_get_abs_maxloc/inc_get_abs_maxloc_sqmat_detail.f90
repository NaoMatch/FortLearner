tmp_absmax = - huge(tmp_absmax)
do j=1, n_dim
    do i=j+1, n_dim
        tmp_absval = abs(matrix(i, j))
        if ( tmp_absval .gt. tmp_absmax ) then
            tmp_absmax = tmp_absval
            loc = (/i,j/)
        end if
    end do
end do
val = matrix(loc(1), loc(2))
