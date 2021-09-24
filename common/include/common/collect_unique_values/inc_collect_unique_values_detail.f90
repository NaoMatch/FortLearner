if (allocated(uniq_vals)) deallocate(uniq_vals)
idx = 2
n_unique = count_unique(vector, n_samples)
allocate(uniq_vals(n_unique))
uniq_vals(1) = vector(1)
do n=2, n_samples
    if ( vector(n-1) .ne. vector(n) ) then
        uniq_vals(idx) = vector(n)
        idx = idx + 1
    end if
end do
