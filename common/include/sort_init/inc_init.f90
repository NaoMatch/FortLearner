subroutine init_r8(this, num, n_work_space)
    implicit none
    class(empty_bucket)   :: this
    integer(kind=8) :: num, n_work_space
    integer(kind=8) :: n_samples_per_buckets, i

    n_samples_per_buckets = maxval((/ num/n_work_space, this%min_size_i8/))

    if (allocated(this % work_space)) deallocate(this % work_space)
    allocate(this % work_space(n_work_space))

    do i=1, n_work_space
        allocate(this % work_space(i) % tmp_r8(n_samples_per_buckets))
        this % work_space(i) % idx_i8 = 1
        this % work_space(i) % current_size_i8 = n_samples_per_buckets
        this % work_space(i) % original_size_i8 = n_samples_per_buckets
    end do
end subroutine init_r8

subroutine init_i4(this, num, n_work_space)
    implicit none
    class(empty_bucket)   :: this
    integer(kind=4) :: num, n_work_space
    integer(kind=4) :: n_samples_per_buckets, i

    n_samples_per_buckets = maxval((/ num/n_work_space, this%min_size_i4/))

    if (allocated(this % work_space)) deallocate(this % work_space)
    allocate(this % work_space(n_work_space))

    do i=1, n_work_space
        allocate(this % work_space(i) % tmp_i4(n_samples_per_buckets))
        this % work_space(i) % idx_i4 = 1
        this % work_space(i) % current_size_i4 = n_samples_per_buckets
        this % work_space(i) % original_size_i4 = n_samples_per_buckets
    end do
end subroutine init_i4

subroutine init_i8(this, num, n_work_space)
    implicit none
    class(empty_bucket)   :: this
    integer(kind=8) :: num, n_work_space
    integer(kind=8) :: n_samples_per_buckets, i

    n_samples_per_buckets = maxval((/ num/n_work_space, this%min_size_i8/))

    if (allocated(this % work_space)) deallocate(this % work_space)
    allocate(this % work_space(n_work_space))

    do i=1, n_work_space
        allocate(this % work_space(i) % tmp_i8(n_samples_per_buckets))
        this % work_space(i) % idx_i8 = 1
        this % work_space(i) % current_size_i8 = n_samples_per_buckets
        this % work_space(i) % original_size_i8 = n_samples_per_buckets
    end do
end subroutine init_i8
