module mod_svm_cache
    use mod_const
    implicit none

    type kernel_cache
        integer(kind=8) :: idx_hash = 0_8
        real(kind=8), allocatable :: values(:)
    end type kernel_cache

    type svm_cache
        integer(kind=8)                 :: n_caches
        integer(kind=8)                 :: n_avail
        integer(kind=8)                 :: n_used
        logical(kind=4), allocatable    :: is_used(:)
        integer(kind=8), allocatable    :: recency(:) ! 直近利用のループカウント
        type(kernel_cache), allocatable :: kernels(:)
    contains
        procedure :: query => query_svm_cache
        procedure :: update => update_svm_cache
        procedure :: comput_cache_size => comput_cache_size_svm_cache
        procedure :: get_available_cache_count => get_available_cache_count_svm_cache

        procedure :: query_with_indices => query_svm_cache_with_indices
    end type svm_cache

    interface svm_cache
        procedure :: new_svm_cache
    end interface svm_cache
    
contains
    !> Cache space for svm only. 
    !> Performance of my implemented hash map is very low.
    !! \param n_caches number of cache space = number of training samples.
    function new_svm_cache(n_caches)
        implicit none
        type(svm_cache) :: new_svm_cache
        integer(kind=8) :: n_caches

        new_svm_cache%n_caches = n_caches
        new_svm_cache%n_used = 0

        allocate(new_svm_cache%is_used(n_caches))
        allocate(new_svm_cache%recency(n_caches))
        new_svm_cache%is_used(:) = f_
        new_svm_cache%recency(:) = huge(0_8)

        allocate(new_svm_cache%kernels(n_caches))
    end function new_svm_cache

    subroutine query_svm_cache_with_indices(this, key, idx_hash, values_, n_samples_, indices_, &
        loop_count, status, drop_existing_rows, compute_non_overlapping, idx_hash_old)
        implicit none
        class(svm_cache) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8), intent(in) :: idx_hash
        real(kind=8), intent(inout) :: values_(n_samples_)
        integer(kind=8), intent(in) :: n_samples_
        integer(kind=8), intent(in) :: indices_(n_samples_)
        integer(kind=8), intent(in) :: loop_count
        integer(kind=8), intent(inout) :: status
        logical(kind=4), intent(in) :: drop_existing_rows
        logical(kind=4), intent(in) :: compute_non_overlapping
        integer(kind=8), intent(in) :: idx_hash_old

        integer(kind=8) :: i, idx

        ! Is used or not.
        if ( this%is_used(key) ) then
            ! Kernel is already computed
            if (this%kernels(key)%idx_hash == idx_hash) then
                status = 0
                do i=1, n_samples_, 1
                    idx = indices_(i)
                    values_(i) = this%kernels(key)%values(idx)
                end do
                this%recency(key) = loop_count
            elseif (drop_existing_rows .and. this%kernels(key)%idx_hash == idx_hash_old) then
                status = 0
                do i=1, n_samples_, 1
                    idx = indices_(i)
                    values_(i) = this%kernels(key)%values(idx)
                end do
                this%recency(key) = loop_count
            elseif (compute_non_overlapping .and. this%kernels(key)%idx_hash == idx_hash_old) then
                status = 2
                do i=1, n_samples_, 1
                    values_(i) = this%kernels(key)%values(i)
                end do
                this%recency(key) = loop_count
            else
                status = 1
            end if
        else
            ! No results
            status = 1
        end if
    end subroutine query_svm_cache_with_indices

    !> Extract key-th row of Q matrix. 
    !! \param key row index of Q matrix to be extracted.
    !! \param idx_hash hash of shrunk row indices.
    !! \param values extracted key-th row of Q matrix.
    !! \param n_samples number of training samples.
    !! \param loop_count loop counter. This is used for Least Recently Used Stratedy.
    !! \param status query status. 0 indicates no error. 1 means that the result of the calculation for k-th row of Q matrix does not exist. 2 indicates that the result of the calculation of the kth row of the Q matrix is present but partially missing.
    !! \param drop_existing_rows If .true., extract shrunk indices. 
    !! \param compute_non_overlapping If .true., extract overlapping indices. 'overlapping' means duplicated indices of previous shrinkage stage.
    !! \param idx_hash_old hash of shrunk row indices of previous shrinkage stage.
    subroutine query_svm_cache(this, key, idx_hash, values, n_samples, loop_count, status, &
        drop_existing_rows, compute_non_overlapping, idx_hash_old)
        implicit none
        class(svm_cache), target       :: this
        integer(kind=8), intent(in)    :: key
        integer(kind=8), intent(in)    :: idx_hash
        real(kind=8), intent(inout)    :: values(n_samples)
        integer(kind=8), intent(in)    :: n_samples
        integer(kind=8), intent(in)    :: loop_count
        integer(kind=8), intent(inout) :: status
        logical(kind=4), intent(in)    :: drop_existing_rows
        logical(kind=4), intent(in)    :: compute_non_overlapping
        integer(kind=8), intent(in)    :: idx_hash_old
        integer(kind=8)    :: i

        ! Is used or not.
        if ( this%is_used(key) ) then
            ! Kernel is already computed
            if (this%kernels(key)%idx_hash == idx_hash) then
                status = 0
                do i=1, n_samples, 1
                    values(i) = this%kernels(key)%values(i)
                end do
                this%recency(key) = loop_count
            elseif (drop_existing_rows .and. this%kernels(key)%idx_hash == idx_hash_old) then
                status = 0
                do i=1, n_samples, 1
                    values(i) = this%kernels(key)%values(i)
                end do
                this%recency(key) = loop_count
            elseif (compute_non_overlapping .and. this%kernels(key)%idx_hash == idx_hash_old) then
                status = 2
                do i=1, n_samples, 1
                    values(i) = this%kernels(key)%values(i)
                end do
                this%recency(key) = loop_count
            else
                status = 1
            end if
        else
            ! No results
            status = 1
        end if
    end subroutine query_svm_cache


    !> Update key-th row of Q matrix. 
    !! \param key row index of Q matrix to be extracted.
    !! \param idx_hash hash of shrunk row indices.
    !! \param values extracted key-th row of Q matrix.
    !! \param n_samples number of training samples.
    !! \param loop_count loop counter. This is used for Least Recently Used Stratedy.
    subroutine update_svm_cache(this, key, idx_hash, values, n_values, loop_count)
        implicit none
        class(svm_cache) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8), intent(in) :: idx_hash
        real(kind=8), intent(in)    :: values(n_values)
        integer(kind=8), intent(in) :: n_values
        integer(kind=8), intent(in) :: loop_count

        integer(kind=8) :: oldest_used_idx

        this%is_used(key) = t_
        if (.not. allocated(this%kernels(key)%values)) allocate(this%kernels(key)%values(n_values))
        this%kernels(key)%values(:) = values(:)
        this%kernels(key)%idx_hash = idx_hash
        this%recency(key) = loop_count
        this%n_used = this%n_used + 1

        if (this%n_used >= this%n_avail) then
            oldest_used_idx = minloc(this%recency, dim=1)
            deallocate(this%kernels(oldest_used_idx)%values)
            this%recency(oldest_used_idx) = huge(0_8)
            this%is_used(oldest_used_idx) = f_
            this%n_used = count(this%is_used)
            this%kernels(oldest_used_idx)%idx_hash = 0_8
            this%n_used = this%n_used - 1
        end if
    end subroutine update_svm_cache


    !> Compute Cache size in MB.
    function comput_cache_size_svm_cache(this) result(cache_size)
        implicit none
        class(svm_cache) :: this

        integer(kind=8)  :: count_used
        real(kind=8)  :: cache_size ! cache size in MB

        count_used = count(this%is_used)

        cache_size = count_used * 64_8 * this%n_caches / 8d0 / 1024d0 / 1024d0
    end function comput_cache_size_svm_cache

    !> Compute available number of cache.
    !! \param max_cache_size maximum cache size.
    subroutine get_available_cache_count_svm_cache(this, max_cache_size)
        implicit none
        class(svm_cache) :: this
        integer(kind=8), intent(in) :: max_cache_size
        real(kind=8) :: unit_size

        unit_size = 64_8 * this%n_caches / 8_8 / 1024d0 / 1024d0
        this%n_avail = max_cache_size / unit_size
    end subroutine get_available_cache_count_svm_cache

end module mod_svm_cache