    subroutine collect_unique_values_i8(uniq_v, vector, n_samples)
        implicit none
        integer(kind=8), allocatable   :: uniq_v(:)
        integer(kind=8), intent(inout) :: vector(n_samples)
        integer(kind=8), intent(in)    :: n_samples

        integer(kind=8) :: n_unique, n, idx

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
    end subroutine collect_unique_values_i8
