lo = 1
hi = n_samples
do while (lo .lt. hi)
    mid = (lo+hi)/2
    if ( vector(mid) .lt. value ) then
        lo = mid + 1
    else
        hi = mid
    end if
end do
