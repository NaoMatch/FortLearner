lo = 1
hi = n_samples
do while (lo .lt. hi)
    mid = (lo+hi)/2
    if ( vector(mid) .gt. value ) then
        hi = mid
    else
        lo = mid + 1
    end if
end do
