if (allocated(uniq_v)) deallocate(uniq_v)
idx = 2
n_unique = count_unique(vector, n_samples)
allocate(uniq_v(n_unique))
uniq_v(1) = vector(1)
do n=2, n_samples
    if ( vector(n-1) .ne. vector(n) ) then
        uniq_v(idx) = vector(n)
        idx = idx + 1
    end if
end do
