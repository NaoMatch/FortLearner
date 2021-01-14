!> A module for sort functions.
module mod_sort
    use mod_common
    use mod_random
    implicit none

    type work
        real(kind=4), allocatable    :: tmp_r4(:)
        real(kind=8), allocatable    :: tmp_r8(:)
        integer(kind=4), allocatable :: tmp_i4(:)
        integer(kind=8), allocatable :: tmp_i8(:)

        integer(kind=4), allocatable :: indices_i4(:)
        integer(kind=8), allocatable :: indices_i8(:)
        integer(kind=4) :: idx_i4, counter_i4, current_size_i4, original_size_i4
        integer(kind=8) :: idx_i8, counter_i8, current_size_i8, original_size_i8
    contains
        procedure :: append_r4
        procedure :: append_r8
        procedure :: append_i4
        procedure :: append_i8

        procedure :: size_up_r4
        procedure :: size_up_r8
        procedure :: size_up_i4
        procedure :: size_up_i8

        procedure :: sort_r4
        procedure :: sort_r8
        ! procedure :: sort_i4
        ! procedure :: sort_i8

        procedure :: append_arg_r4
        procedure :: append_arg_r8
        procedure :: argsort_r4
        procedure :: argsort_r8

    end type work

    type empty_bucket
        integer(kind=4) :: min_size_i4 = 100
        integer(kind=8) :: min_size_i8 = 100_8
        type(work), allocatable :: work_space(:)
    contains
        ! procedure, pass :: init_i4
        ! procedure, pass :: init_i8
        procedure, pass :: init_r4
        procedure, pass :: init_r8
        ! generic         :: init => init_i4, init_i8, init_r4, init_r8
        ! generic         :: init => init_r4

        ! procedure :: init_arg_i4
        ! procedure :: init_arg_i8
        procedure :: init_arg_r4
        procedure :: init_arg_r8
    end type empty_bucket


    interface pbucket_sort
        module procedure pbucket_sort_r4
        module procedure pbucket_sort_r8
        ! module procedure pbucket_sort_i4
        ! module procedure pbucket_sort_i8
    end interface pbucket_sort


    interface pbucket_argsort
        module procedure pbucket_argsort_r4
        module procedure pbucket_argsort_r8
        ! module procedure pbucket_argsort_i4
        ! module procedure pbucket_argsort_i8
    end interface pbucket_argsort


    !> An interface to call insertion_sort_r4, insertion_sort_r8, insertion_sort_i4, and insertion_sort_i8
    interface insertion_sort
        module procedure :: insertion_sort_r4
        module procedure :: insertion_sort_r8
        module procedure :: insertion_sort_i4
        module procedure :: insertion_sort_i8
    end interface insertion_sort

    !> An interface to call insertion_argsort_r4_i4, insertion_argsort_r8_i8, insertion_argsort_i4_i4, and insertion_argsort_i8
    interface insertion_argsort
        module procedure :: insertion_argsort_r4_i4
        module procedure :: insertion_argsort_r8_i8
        module procedure :: insertion_argsort_r4_r4
        module procedure :: insertion_argsort_r8_r8
        module procedure :: insertion_argsort_i4_i4
        module procedure :: insertion_argsort_i8_i8
        module procedure :: insertion_argsort_i4_r4
        module procedure :: insertion_argsort_i8_r8
    end interface insertion_argsort

    !> An interface to call quick_sort_r4, quick_sort_r8, quick_sort_i4, and quick_sort_i8
    interface quick_sort
        module procedure :: quick_sort_r4
        module procedure :: quick_sort_r8
        module procedure :: quick_sort_i4
        module procedure :: quick_sort_i8
    end interface quick_sort

    !> An interface to call quick_argsort_r4_i4, quick_argsort_r8_i8, quick_argsort_i4_i4, and quick_argsort_i8_i8
    interface quick_argsort
        module procedure :: quick_argsort_r4_i4
        module procedure :: quick_argsort_r8_i8
        module procedure :: quick_argsort_r4_r4
        module procedure :: quick_argsort_r8_r8
        module procedure :: quick_argsort_i4_i4
        module procedure :: quick_argsort_i8_i8
        module procedure :: quick_argsort_i4_r4
        module procedure :: quick_argsort_i8_r8
    end interface quick_argsort

    !> An interface to block_quick_sort
    interface block_quick_sort
        module procedure :: block_quick_sort_r4
        module procedure :: block_quick_sort_r8
        module procedure :: block_quick_sort_i4
        module procedure :: block_quick_sort_i8
    end interface block_quick_sort

    !> An interface to block_quick_argsort
    interface block_quick_argsort
        module procedure :: block_quick_argsort_r4_i4
        module procedure :: block_quick_argsort_r8_r8
    end interface block_quick_argsort

    interface quick_select_lower
        module procedure :: quick_select_lower_r8
    end interface quick_select_lower

    interface quick_select_upper
        module procedure :: quick_select_upper_r8
    end interface quick_select_upper

contains

    recursive subroutine pbucket_argsort_r4(vector, indices, n_samples, rec_depth)
        real(kind=4), intent(inout)           :: vector(n_samples)
        integer(kind=4), intent(inout)        :: indices(n_samples)
        integer(kind=4), intent(in)           :: n_samples
        integer(kind=4), optional, intent(in) :: rec_depth

        integer(kind=4) :: rec_depth_opt
        real(kind=4) :: v_min, v_max
        integer(kind=4) :: n_samples_sqrt
        integer(kind=4), allocatable :: indices_sampling(:)
        real(kind=4), allocatable    :: values_sampling(:)
        integer(kind=4) :: n_uniq
        integer(kind=4) :: left_posi, n_buckets
        real(kind=4)    :: diff, max_val
        integer(kind=4) :: i, bucket_idx
        real(kind=4) :: tmp_v, factor
        integer(kind=4) :: tmp_i
        type(empty_bucket) :: bucket
        integer(kind=4) :: ini, fin, one

        if (n_samples .eq. 0) return
        rec_depth_opt = 0
        if (present(rec_depth)) rec_depth_opt = rec_depth

        ! call get_minmax(vector, n_samples, v_min, v_max)
        v_min = minval(vector)
        v_max = maxval(vector)
        if (v_min .eq. v_max) return ! Fast Return

        one = 1
        n_samples_sqrt=sqrt(n_samples+0.0)
        allocate(indices_sampling(n_samples_sqrt))
        allocate(values_sampling(n_samples_sqrt))
        call rand_integer(one, n_samples, indices_sampling, n_samples_sqrt) ! generate sample indices_sampling with duplication
        values_sampling(:) = vector(indices_sampling)
        call quick_sort(values_sampling, n_samples_sqrt)
        n_uniq = count_unique(values_sampling, n_samples_sqrt)
        if (n_uniq .le. 10) then
            call quick_argsort(vector, indices, n_samples)
            return
        elseif (n_uniq .gt. int(n_samples_sqrt * 0.98)) then
            left_posi = most_left_bit_position(n_uniq)
            n_buckets = ibset(0_4, left_posi+1)
        else
            n_buckets = n_uniq
        end if

        ! Append Bucket
        call bucket%init_arg_r4(n_samples, n_buckets)
        diff = 1.0 / (v_max-v_min)
        max_val = n_buckets-1.0
        diff = diff * max_val
        factor =  - v_min * diff + 1

        do i=1, n_samples
            tmp_v = vector(i)
            tmp_i = indices(i)
            bucket_idx = int( tmp_v * diff + factor, kind=4 )
            call bucket % work_space(bucket_idx) % append_arg_r4(tmp_v, tmp_i)
        end do

        if (rec_depth_opt .le. max_rec_depth) then ! 0(Only One Time Recursion) is Best?
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i4 - 2
                vector(ini:fin)  = bucket % work_space(i) % tmp_r4(1:bucket % work_space(i) % idx_i4-1)
                indices(ini:fin) = bucket % work_space(i) % tmp_i4(1:bucket % work_space(i) % idx_i4-1)
                call pbucket_argsort_r4(vector(ini:fin), indices(ini:fin), fin-ini+1, rec_depth_opt+1)
                ini = fin + 1
            end do
        else
            ! Sort Arrays
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i4 - 2
                call bucket % work_space(i) % argsort_r4()
                vector(ini:fin)  = bucket % work_space(i) % tmp_r4(1:bucket % work_space(i) % idx_i4-1)
                indices(ini:fin) = bucket % work_space(i) % tmp_i4(1:bucket % work_space(i) % idx_i4-1)
                ini = fin + 1
            end do
        end if
    end subroutine pbucket_argsort_r4


    recursive subroutine pbucket_argsort_r8(vector, indices, n_samples, rec_depth)
        real(kind=8), intent(inout)           :: vector(n_samples)
        integer(kind=8), intent(inout)        :: indices(n_samples)
        integer(kind=8), intent(in)           :: n_samples
        integer(kind=8), optional, intent(in) :: rec_depth

        integer(kind=8) :: rec_depth_opt
        real(kind=8) :: v_min, v_max
        integer(kind=8) :: n_samples_sqrt
        integer(kind=8), allocatable :: indices_sampling(:)
        real(kind=8), allocatable    :: values_sampling(:)
        integer(kind=8) :: n_uniq
        integer(kind=8) :: left_posi, n_buckets
        real(kind=8)    :: diff, max_val
        integer(kind=8) :: i, bucket_idx
        real(kind=8) :: tmp_v, factor
        integer(kind=8) :: tmp_i
        type(empty_bucket) :: bucket
        integer(kind=8) :: ini, fin, one

        if (n_samples .eq. 0) return
        rec_depth_opt = 0
        if (present(rec_depth)) rec_depth_opt = rec_depth

        ! call get_minmax(vector, n_samples, v_min, v_max)
        v_min = minval(vector)
        v_max = maxval(vector)
        if (v_min .eq. v_max) return ! Fast Return

        one = 1_8
        n_samples_sqrt=sqrt(n_samples+0.0)
        allocate(indices_sampling(n_samples_sqrt))
        allocate(values_sampling(n_samples_sqrt))
        call rand_integer(one, n_samples, indices_sampling, n_samples_sqrt) ! generate sample indices_sampling with duplication
        values_sampling(:) = vector(indices_sampling)
        call quick_sort(values_sampling, n_samples_sqrt)
        n_uniq = count_unique(values_sampling, n_samples_sqrt)
        if (n_uniq .le. 10) then
            call quick_argsort(vector, indices, n_samples)
            return
        elseif (n_uniq .gt. int(n_samples_sqrt * 0.98)) then
            left_posi = most_left_bit_position(n_uniq)
            n_buckets = ibset(0_8, left_posi+1)
        else
            n_buckets = n_uniq
        end if

        ! Append Bucket
        call bucket%init_arg_r8(n_samples, n_buckets)
        diff = 1d0 / (v_max-v_min)
        max_val = n_buckets-1.0
        diff = diff * max_val
        factor =  - v_min * diff + 1

        do i=1, n_samples
            tmp_v = vector(i)
            tmp_i = indices(i)
            bucket_idx = int( tmp_v * diff + factor, kind=8 )
            call bucket % work_space(bucket_idx) % append_arg_r8(tmp_v, tmp_i)
        end do

        if (rec_depth_opt .le. max_rec_depth) then ! 0(Only One Time Recursion) is Best?
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i8 - 2
                vector(ini:fin)  = bucket % work_space(i) % tmp_r8(1:bucket % work_space(i) % idx_i8-1)
                indices(ini:fin) = bucket % work_space(i) % tmp_i8(1:bucket % work_space(i) % idx_i8-1)
                call pbucket_argsort_r8(vector(ini:fin), indices(ini:fin), fin-ini+1, rec_depth_opt+1)
                ini = fin + 1
            end do
        else
            ! Sort Arrays
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i8 - 2
                call bucket % work_space(i) % argsort_r8()
                vector(ini:fin)  = bucket % work_space(i) % tmp_r8(1:bucket % work_space(i) % idx_i8-1)
                indices(ini:fin) = bucket % work_space(i) % tmp_i8(1:bucket % work_space(i) % idx_i8-1)
                ini = fin + 1
            end do
        end if
    end subroutine pbucket_argsort_r8


    recursive subroutine pbucket_argsort_new_r8(vector, indices, n_samples, rec_depth)
        real(kind=8), intent(inout)           :: vector(n_samples)
        integer(kind=8), intent(inout)        :: indices(n_samples)
        integer(kind=8), intent(in)           :: n_samples
        integer(kind=8), optional, intent(in) :: rec_depth

        integer(kind=8) :: rec_depth_opt
        real(kind=8) :: v_min, v_max
        integer(kind=8) :: n_samples_sqrt
        integer(kind=8), allocatable :: indices_sampling(:)
        real(kind=8), allocatable    :: values_sampling(:)
        integer(kind=8) :: n_uniq
        integer(kind=8) :: left_posi, n_buckets
        real(kind=8)    :: diff, max_val
        integer(kind=8) :: i, bucket_idx
        real(kind=8) :: tmp_v, factor
        integer(kind=8) :: tmp_i
        type(empty_bucket) :: bucket
        integer(kind=8) :: ini, fin, one

        real(kind=8)    :: buffer_v(31)
        integer(kind=8) :: buffer_i(31), buffer_b(31), n_samples_unroll, u, uid, tmp_b

        if (n_samples .eq. 0) return
        rec_depth_opt = 0
        if (present(rec_depth)) rec_depth_opt = rec_depth

        ! call get_minmax(vector, n_samples, v_min, v_max)
        v_min = minval(vector)
        v_max = maxval(vector)
        if (v_min .eq. v_max) return ! Fast Return

        one = 1_8
        n_samples_sqrt=sqrt(n_samples+0.0)
        allocate(indices_sampling(n_samples_sqrt))
        allocate(values_sampling(n_samples_sqrt))
        call rand_integer(one, n_samples, indices_sampling, n_samples_sqrt) ! generate sample indices_sampling with duplication
        values_sampling(:) = vector(indices_sampling)
        call quick_sort(values_sampling, n_samples_sqrt)
        n_uniq = count_unique(values_sampling, n_samples_sqrt)
        if (n_uniq .le. 10) then
            call quick_argsort(vector, indices, n_samples)
            return
        elseif (n_uniq .gt. int(n_samples_sqrt * 0.98)) then
            left_posi = most_left_bit_position(n_uniq)
            n_buckets = ibset(0_8, left_posi+1)
        else
            n_buckets = n_uniq
        end if

        ! Append Bucket
        call bucket%init_arg_r8(n_samples, n_buckets)
        diff = 1.0 / (v_max-v_min)
        max_val = n_buckets-1.0
        factor = diff * max_val
        diff = - v_min * factor + 1
        n_samples_unroll = n_samples - mod(n_samples, 31)

        do i=1, n_samples_unroll, 31
            do u=0, 31-1, 1
                tmp_v = vector(u+i)
                tmp_i = indices(u+i)
                tmp_b = maxval((/int(tmp_v * factor + diff , kind=8), 1_8/))
                call bucket % work_space(tmp_b) % append_arg_r8(tmp_v, tmp_i)
            end do

            ! do u=0, 31-1, 1
            !     tmp_v = vector(u+i)
            !     buffer_v(u+1) = tmp_v
            !     buffer_i(u+1) = indices(u+i)
            !     buffer_b(u+1) = maxval((/int(tmp_v * factor + diff , kind=8), 1_8/))
            ! end do

            ! do u=0, 31-1, 1
            !     tmp_v = buffer_v(u+1)
            !     tmp_i = buffer_i(u+1)
            !     bucket_idx = buffer_b(u+1)
            !     call bucket % work_space(bucket_idx) % append_arg_r8(tmp_v, tmp_i)
            ! end do
        end do

        do i=n_samples_unroll+1, n_samples
            tmp_v = vector(i)
            tmp_i = indices(i)
            bucket_idx = maxval((/int(tmp_v * factor + diff , kind=8), 1_8/))
            call bucket % work_space(bucket_idx) % append_arg_r8(tmp_v, tmp_i)
        end do

        if (rec_depth_opt .le. max_rec_depth) then ! 0(Only One Time Recursion) is Best?
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i8 - 2
                vector(ini:fin)  = bucket % work_space(i) % tmp_r8(1:bucket % work_space(i) % idx_i8-1)
                indices(ini:fin) = bucket % work_space(i) % tmp_i8(1:bucket % work_space(i) % idx_i8-1)
                call pbucket_argsort_new_r8(vector(ini:fin), indices(ini:fin), fin-ini+1, rec_depth_opt+1)
                ini = fin + 1
            end do
        else
            ! Sort Arrays
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i8 - 2
                call bucket % work_space(i) % argsort_r8()
                vector(ini:fin)  = bucket % work_space(i) % tmp_r8(1:bucket % work_space(i) % idx_i8-1)
                indices(ini:fin) = bucket % work_space(i) % tmp_i8(1:bucket % work_space(i) % idx_i8-1)
                ini = fin + 1
            end do
        end if
    end subroutine pbucket_argsort_new_r8


    recursive subroutine pbucket_sort_r4(vector, n_samples, rec_depth)
        real(kind=4), intent(inout)           :: vector(n_samples)
        integer(kind=4), intent(in)           :: n_samples
        integer(kind=4), optional, intent(in) :: rec_depth

        integer(kind=4)              :: rec_depth_opt
        real(kind=4)                 :: v_min, v_max
        integer(kind=4)              :: n_samples_sqrt
        integer(kind=4), allocatable :: indices(:)
        real(kind=4), allocatable    :: values(:)
        integer(kind=4)              :: n_unique, idx
        integer(kind=4)              :: left_posi, n_buckets
        real(kind=8)                 :: diff, max_val, factor
        integer(kind=4)              :: i, bucket_idx
        real(kind=4)                 :: tmp
        type(empty_bucket)           :: bucket
        ! -------------------------------------------------------------
        integer(kind=4)              :: ini, fin, n_samples_unroll, u, one=1
        real(kind=4)                 :: tmp_vals(63)
        integer(kind=4)              :: tmp_idxs(63)

        if (n_samples .eq. 0) return ! Fast Return

        rec_depth_opt = 0
        if (present(rec_depth)) rec_depth_opt = rec_depth

        ! call get_minmax(v_min, v_max, vector, n_samples)
        v_min = minval(vector)
        v_max = maxval(vector)
        if (v_min .eq. v_max) return ! Fast Return

        n_samples_sqrt=sqrt(n_samples+0.0)
        allocate(indices(n_samples_sqrt))
        allocate(values(n_samples_sqrt))
        call rand_integer(one, n_samples, indices, n_samples_sqrt) ! generate sample indices with duplication
        do i=1, n_samples_sqrt, 1
            idx = indices(i)
            values(i) = vector(idx)
        end do
        
        call quick_sort(values, n_samples_sqrt)
        n_unique = count_unique(values, n_samples_sqrt)
        if (n_unique .le. 10) then
            call quick_sort(vector, n_samples)
            return
        elseif (n_unique .gt. int(n_samples * 0.98)) then
            left_posi = most_left_bit_position(n_unique)
            n_buckets = ibset(0_4, left_posi+1)
        else
            n_buckets = n_unique
        end if

        ! Append Bucket
        call bucket%init_r4(n_samples, n_buckets)
        diff = 1.0 / (v_max-v_min)
        max_val = n_buckets-1.0
        factor = diff * max_val
        diff = - v_min * factor + 1
        n_samples_unroll = n_samples - mod(n_samples, 63)
        do i=1, n_samples_unroll, 63
            do u=0, 63-1, 1
                tmp = vector(i)
                tmp_vals(u+1) = tmp
                tmp_idxs(u+1) = int( tmp * factor + diff, kind=4)
            end do
            do u=0, 63-1, 1
                tmp = tmp_vals(u+1)
                idx = tmp_idxs(u+1)
                call bucket % work_space(idx) % append_r4(tmp)
            end do
        end do
        do i=n_samples_unroll+1, n_samples, 1
            tmp = vector(i)
            bucket_idx = int( tmp * factor + diff, kind=4)
            call bucket % work_space(bucket_idx) % append_r4(tmp)
        end do

        ! Go to Next
        if (rec_depth_opt .le. max_rec_depth) then ! 0(Only One Time Recursion) is Best?
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i4 - 2
                vector(ini:fin) = bucket % work_space(i) % tmp_r4(1:bucket % work_space(i) % idx_i4-1)
                call pbucket_sort(vector(ini:fin), fin-ini+1, rec_depth_opt+1)
                ini = fin + 1
            end do

        else
            ! Sort Arrays
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i4 - 2
                call bucket % work_space(i) % sort_r4()
                vector(ini:fin) = bucket % work_space(i) % tmp_r4(1:bucket % work_space(i) % idx_i4-1)
                ini = fin + 1
            end do
        end if
    end subroutine pbucket_sort_r4


    recursive subroutine pbucket_sort_r8(vector, n_samples, rec_depth)
        real(kind=8), intent(inout)           :: vector(n_samples)
        integer(kind=8), intent(in)           :: n_samples
        integer(kind=8), optional, intent(in) :: rec_depth

        integer(kind=8)              :: rec_depth_opt
        real(kind=8)                 :: v_min, v_max
        integer(kind=8)              :: n_samples_sqrt
        integer(kind=8), allocatable :: indices(:)
        real(kind=8), allocatable    :: values(:)
        integer(kind=8)              :: n_unique, idx
        integer(kind=8)              :: left_posi, n_buckets
        real(kind=8)                 :: diff, max_val, factor
        integer(kind=8)              :: i, bucket_idx
        real(kind=8)                 :: tmp
        type(empty_bucket)           :: bucket
        ! -------------------------------------------------------------
        integer(kind=8)              :: ini, fin, n_samples_unroll, u, one=1
        real(kind=8)                 :: tmp_vals(63)
        integer(kind=8)              :: tmp_idxs(63)

        if (n_samples .eq. 0) return ! Fast Return

        rec_depth_opt = 0
        if (present(rec_depth)) rec_depth_opt = rec_depth

        ! call get_minmax(v_min, v_max, vector, n_samples)
        v_min = minval(vector)
        v_max = maxval(vector)
        if (v_min .eq. v_max) return ! Fast Return

        n_samples_sqrt=sqrt(n_samples+0.0)
        allocate(indices(n_samples_sqrt))
        allocate(values(n_samples_sqrt))
        call rand_integer(one, n_samples, indices, n_samples_sqrt) ! generate sample indices with duplication
        do i=1, n_samples_sqrt, 1
            idx = indices(i)
            values(i) = vector(idx)
        end do
        
        call quick_sort(values, n_samples_sqrt)
        n_unique = count_unique(values, n_samples_sqrt)
        if (n_unique .le. 10) then
            call quick_sort(vector, n_samples)
            return
        elseif (n_unique .gt. int(n_samples * 0.98)) then
            left_posi = most_left_bit_position(n_unique)
            n_buckets = ibset(0_4, left_posi+1)
        else
            n_buckets = n_unique
        end if

        ! Append Bucket
        call bucket%init_r8(n_samples, n_buckets)
        diff = 1d0 / (v_max-v_min)
        max_val = n_buckets-1.0
        factor = diff * max_val
        diff = - v_min * factor + 1
        n_samples_unroll = n_samples - mod(n_samples, 63)
        do i=1, n_samples_unroll, 63
            do u=0, 63-1, 1
                tmp = vector(i)
                tmp_vals(u+1) = tmp
                tmp_idxs(u+1) = int( tmp * factor + diff, kind=4)
            end do
            do u=0, 63-1, 1
                tmp = tmp_vals(u+1)
                idx = tmp_idxs(u+1)
                call bucket % work_space(idx) % append_r8(tmp)
            end do
        end do
        do i=n_samples_unroll+1, n_samples, 1
            tmp = vector(i)
            bucket_idx = int( tmp * factor + diff, kind=8)
            call bucket % work_space(bucket_idx) % append_r8(tmp)
        end do

        ! Go to Next
        if (rec_depth_opt .le. max_rec_depth) then ! 0(Only One Time Recursion) is Best?
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i8 - 2
                vector(ini:fin) = bucket % work_space(i) % tmp_r8(1:bucket % work_space(i) % idx_i8-1)
                call pbucket_sort(vector(ini:fin), fin-ini+1, rec_depth_opt+1)
                ini = fin + 1
            end do

        else
            ! Sort Arrays
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i8 - 2
                call bucket % work_space(i) % sort_r8()
                vector(ini:fin) = bucket % work_space(i) % tmp_r8(1:bucket % work_space(i) % idx_i8-1)
                ini = fin + 1
            end do
        end if
    end subroutine pbucket_sort_r8


    recursive subroutine pbucket_sort_old_r8(vector, n_samples, rec_depth)
        real(kind=8), intent(inout)           :: vector(n_samples)
        integer(kind=8), intent(in)           :: n_samples
        integer(kind=8), optional, intent(in) :: rec_depth

        integer(kind=8)              :: rec_depth_opt
        real(kind=8)                 :: v_min, v_max
        integer(kind=8)              :: n_samples_sqrt
        integer(kind=8), allocatable :: indices(:)
        real(kind=8), allocatable    :: values(:)
        integer(kind=8)              :: n_unique, idx
        integer(kind=8)              :: left_posi, n_buckets
        real(kind=8)                 :: diff, max_val, factor
        integer(kind=8)              :: i, bucket_idx
        real(kind=8)                 :: tmp
        type(empty_bucket)           :: bucket
        ! -------------------------------------------------------------
        integer(kind=8)              :: ini, fin, n_samples_unroll, u, one=1
        real(kind=8)                 :: tmp_vals(63)
        integer(kind=8)              :: tmp_idxs(63)

        if (n_samples .eq. 0) return ! Fast Return

        rec_depth_opt = 0
        if (present(rec_depth)) rec_depth_opt = rec_depth

        ! call get_minmax(v_min, v_max, vector, n_samples)
        v_min = minval(vector)
        v_max = maxval(vector)
        if (v_min .eq. v_max) return ! Fast Return

        n_samples_sqrt=sqrt(n_samples+0.0)
        allocate(indices(n_samples_sqrt))
        allocate(values(n_samples_sqrt))
        call rand_integer(one, n_samples, indices, n_samples_sqrt) ! generate sample indices with duplication
        do i=1, n_samples_sqrt, 1
            idx = indices(i)
            values(i) = vector(idx)
        end do
        
        call quick_sort(values, n_samples_sqrt)
        n_unique = count_unique(values, n_samples_sqrt)
        if (n_unique .le. 10) then
            call quick_sort(vector, n_samples)
            return
        elseif (n_unique .gt. int(n_samples * 0.98)) then
            left_posi = most_left_bit_position(n_unique)
            n_buckets = ibset(0_4, left_posi+1)
        else
            n_buckets = n_unique
        end if

        ! Append Bucket
        call bucket%init_r8(n_samples, n_buckets)
        diff = 1d0 / (v_max-v_min)
        max_val = n_buckets-1.0
        factor = diff * max_val
        diff = - v_min * factor + 1
        n_samples_unroll = n_samples - mod(n_samples, 63)
        do i=1, n_samples, 1
            tmp = vector(i)
            bucket_idx = int( tmp * factor + diff, kind=8)
            call bucket % work_space(bucket_idx) % append_r8(tmp)
        end do

        ! Go to Next
        if (rec_depth_opt .le. max_rec_depth) then ! 0(Only One Time Recursion) is Best?
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i8 - 2
                vector(ini:fin) = bucket % work_space(i) % tmp_r8(1:bucket % work_space(i) % idx_i8-1)
                call pbucket_sort(vector(ini:fin), fin-ini+1, rec_depth_opt+1)
                ini = fin + 1
            end do

        else
            ! Sort Arrays
            ini = 1
            do i=1, n_buckets
                fin = ini + bucket % work_space(i) % idx_i8 - 2
                call bucket % work_space(i) % sort_r8()
                vector(ini:fin) = bucket % work_space(i) % tmp_r8(1:bucket % work_space(i) % idx_i8-1)
                ini = fin + 1
            end do
        end if
    end subroutine pbucket_sort_old_r8

    !> A subroutine to size up work space for pseudo backet sort
    subroutine size_up_r4(this)
        implicit none
        class(work)           :: this
        real(kind=4), allocatable      :: tmp_array(:)
        integer(kind=4), allocatable :: tmp_idx(:)

        allocate(tmp_array(this % current_size_i4))
        tmp_array(:) = this % tmp_r4(:)

        deallocate(this % tmp_r4)
        allocate(this % tmp_r4(this % current_size_i4 * 2))
        this % tmp_r4(1:this % current_size_i4) = tmp_array

        deallocate(tmp_array)

        if ( allocated(this%indices_i4) ) then
            allocate(tmp_idx(this % current_size_i4))
            tmp_idx(:) = this % indices_i4(:)

            deallocate(this % indices_i4)
            allocate(this % indices_i4(this % current_size_i4 * 2))
            this % indices_i4(1:this % current_size_i4) = tmp_idx

            deallocate(tmp_idx)
        end if

        this % current_size_i4 = size(this % tmp_r4)
    end subroutine size_up_r4
    include "./include/sort_size_up/inc_size_up.f90"


    !> A subroutine to sort values and indices stored in 'work' for pseudo bucket sort
    subroutine sort_r4(this)
        implicit none
        class(work) :: this
        call quick_sort(this % tmp_r4(1:this % idx_i4-1), this % idx_i4-1)
    end subroutine sort_r4
    include "./include/sort_sort/inc_sort.f90"


    !> A subroutine to sort values and indices stored in 'work' for pseudo bucket sort
    subroutine argsort_r8(this)
        implicit none
        class(work) :: this
        call quick_argsort(this % tmp_r8(1:this % idx_i8-1), this % tmp_i8(1:this % idx_i8-1), this % idx_i8-1)
    end subroutine argsort_r8


    subroutine argsort_r4(this)
        implicit none
        class(work) :: this
        call quick_argsort(this % tmp_r4(1:this % idx_i4-1), this % tmp_i4(1:this % idx_i4-1), this % idx_i4-1)
    end subroutine argsort_r4


    !> A subroutine to initialize work space for pseudo backet sort
    !! \param num number of samples
    !! \param n_work_space numnber of work spaces
    subroutine init_r4(this, num, n_work_space)
        implicit none
        class(empty_bucket)   :: this
        integer(kind=4) :: num, n_work_space
        integer(kind=4) :: n_samples_per_buckets, i

        n_samples_per_buckets = maxval((/ num/n_work_space, this%min_size_i4/))

        if (allocated(this % work_space)) deallocate(this % work_space)
        allocate(this % work_space(n_work_space))

        do i=1, n_work_space
            allocate(this % work_space(i) % tmp_r4(n_samples_per_buckets))
            this % work_space(i) % idx_i4 = 1
            this % work_space(i) % current_size_i4 = n_samples_per_buckets
            this % work_space(i) % original_size_i4 = n_samples_per_buckets
        end do
    end subroutine init_r4
    include "./include/sort_init/inc_init.f90"


    !> A subroutine to initialize work space for pseudo backet argsort
    !! \param num number of samples
    !! \param n_work_space numnber of work spaces
    subroutine init_arg_r8(this, num, n_work_space)
        implicit none
        class(empty_bucket)  :: this
        integer(kind=8)      :: num, n_work_space
        integer(kind=8)      :: n_samples_per_buckets, i
        n_samples_per_buckets = maxval((/ num/n_work_space, this%min_size_i8/))

        if (allocated(this % work_space)) deallocate(this % work_space)
        allocate(this % work_space(n_work_space))

        do i=1, n_work_space
            allocate(this % work_space(i) % tmp_r8(n_samples_per_buckets))
            allocate(this % work_space(i) % tmp_i8(n_samples_per_buckets))
            this % work_space(i) % idx_i8           = 1
            this % work_space(i) % current_size_i8  = n_samples_per_buckets
            this % work_space(i) % original_size_i8 = n_samples_per_buckets
        end do
    end subroutine init_arg_r8


    subroutine init_arg_r4(this, num, n_work_space)
        implicit none
        class(empty_bucket)  :: this
        integer(kind=4)      :: num, n_work_space
        integer(kind=4)      :: n_samples_per_buckets, i
        n_samples_per_buckets = maxval((/ num/n_work_space, this%min_size_i4/))

        if (allocated(this % work_space)) deallocate(this % work_space)
        allocate(this % work_space(n_work_space))

        do i=1, n_work_space
            allocate(this % work_space(i) % tmp_r4(n_samples_per_buckets))
            allocate(this % work_space(i) % tmp_i4(n_samples_per_buckets))
            this % work_space(i) % idx_i4           = 1
            this % work_space(i) % current_size_i4  = n_samples_per_buckets
            this % work_space(i) % original_size_i4 = n_samples_per_buckets
        end do
    end subroutine init_arg_r4


    !> A subroutine to append value to work space for pseudo backet sort
    !! \param tmp_val a value to be appended to work space
    subroutine append_r4(this, tmp_val)
        implicit none
        class(work) :: this
        real(kind=4)      :: tmp_val
        if ( this % idx_i4 .gt. this % current_size_i4 ) call this % size_up_r4()
        this % tmp_r4(this % idx_i4) = tmp_val
        this % idx_i4 = this % idx_i4 + 1
    end subroutine append_r4
    include "./include/sort_append/inc_append.f90"


    !> A subroutine to append value and its index to work space for pseudo backet argsort
    !! \param tmp_val a value to be appended to work space
    !! \param tmp_idx an index to be appended to work space
    subroutine append_arg_r8(this, tmp_val, tmp_idx)
        implicit none
        class(work) :: this
        real(kind=8)   :: tmp_val
        integer(kind=8)   :: tmp_idx
        if ( this % idx_i8 .gt. this % current_size_i8 ) call this % size_up_r8()
        this % tmp_r8(this % idx_i8) = tmp_val
        this % tmp_i8(this % idx_i8) = tmp_idx
        this % idx_i8 = this % idx_i8 + 1
    end subroutine append_arg_r8


    subroutine append_arg_r4(this, tmp_val, tmp_idx)
        implicit none
        class(work) :: this
        real(kind=4)   :: tmp_val
        integer(kind=4)   :: tmp_idx
        if ( this % idx_i4 .gt. this % current_size_i4 ) call this % size_up_r4()
        this % tmp_r4(this % idx_i4) = tmp_val
        this % tmp_i4(this % idx_i4) = tmp_idx
        this % idx_i4 = this % idx_i4 + 1
    end subroutine append_arg_r4


    !> A subroutine to sort the input vectors using insert sort
    !! \param vector a vector to be sorted
    !! \param num number of samples in vector
    subroutine insertion_sort_r4(vector, num)
        implicit none
        real(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in) :: num
        real(kind=4)                :: tmp_i, tmp_j
        integer(kind=4)             :: i, j
        include "./include/sort_insertion_sort/inc_insertion_sort_detail.f90"
    end subroutine insertion_sort_r4
    include "./include/sort_insertion_sort/inc_insertion_sort.f90"


    !> A subroutine to sort 'vector2' according to 'vector1' using insert sort
    !! \param vector a vector to be sorted
    !! \param indices a indices to be sorted according to vector
    !! \param num number of samples in vector
    subroutine insertion_argsort_r4_i4(vector1, vector2, num)
        implicit none
        real(kind=4), intent(inout)    :: vector1(num)
        integer(kind=4), intent(inout) :: vector2(num)
        integer(kind=4), intent(in)    :: num
        real(kind=4)                   :: tmp1_i, tmp1_j
        integer(kind=4)                :: tmp2_i, tmp2_j
        integer(kind=4)                :: i, j
        include "./include/sort_insertion_argsort/inc_insertion_argsort_detail.f90"
    end subroutine insertion_argsort_r4_i4
    include "./include/sort_insertion_argsort/inc_insertion_argsort.f90"


    !> A subroutine to sort the input vector using quick sort (Insertion sort is also used for small vectors)
    !! \param vector a vector to be sorted
    !! \param num number of samples in vector
    recursive subroutine quick_sort_r4(vector, num)
        implicit none
        real(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in) :: num
        real(kind=4)                :: pivot, tmp
        integer(kind=4)             :: i, j, one, three, dev
        real(kind=4)                :: pivots(3)
        include "./include/sort_quick_sort/inc_quick_sort_detail.f90"        
        if (1 < i-1)    call quick_sort_r4(vector(1:i-1),  i-1)
        if (j+1 < num)  call quick_sort_r4(vector(j+1:num),num-j)
    end subroutine quick_sort_r4
    include "./include/sort_quick_sort/inc_quick_sort.f90"


    !> A subroutine to sort 'vector2' according to 'vector1' using quick sort (Insertion sort is also used for small vectors)
    !! \param vector a vector to be sorted
    !! \param indices a indices to be sorted accoring to 'vector'
    !! \param num number of samples in vector
    recursive subroutine quick_argsort_r4_i4(vector1, vector2, num)
        implicit none
        real(kind=4), intent(inout)    :: vector1(num)
        integer(kind=4), intent(inout) :: vector2(num)
        integer(kind=4), intent(in)    :: num
        real(kind=4)                   :: pivot, tmp1
        integer(kind=4)                :: tmp2
        integer(kind=4)                :: i, j, one, three, dev
        real(kind=4)                   :: pivots(3)
        include "./include/sort_quick_argsort/inc_quick_argsort_detail.f90"        
        if (1 < i-1)    call quick_argsort_r4_i4(vector1(1:i-1),  vector2(1:i-1), i-1)
        if (j+1 < num)  call quick_argsort_r4_i4(vector1(j+1:num),vector2(j+1:num), num-j)
    end subroutine quick_argsort_r4_i4
    include "./include/sort_quick_argsort/inc_quick_argsort.f90"


    !> A subroutine to sort the input vector using block quick sort (Insertion sort is also used for small vectors)
    !! \param vector a vector to be sorted
    !! \param indices a indices to be sorted accoring to 'vector'
    !! \param num number of samples in vector
    recursive subroutine block_quick_sort_r4(vector, num)
        real(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in)    :: num
        integer(kind=4) :: offset_l(31), offset_r(31)
        integer(kind=4) :: start_l, start_r, num_l, num_r, r, l, i, j, num_m
        integer(kind=4) :: idx_l, idx_r, max_idx, next_l, next_r, one
        real(kind=4) :: pivot, tmp
        integer(kind=4)             :: three, dev
        real(kind=4)                :: dev_rand
        real(kind=4)                  :: pivots(3)
        include "./include/sort_block_quick_sort/inc_block_quick_sort_detail.f90"
        if (next_l .gt. 1)     call block_quick_sort_r4(vector(1:next_l),     next_l)
        if (num-next_l .gt. 1) call block_quick_sort_r4(vector(next_l+1:num), num-next_l)
    end subroutine block_quick_sort_r4
    include "./include/sort_block_quick_sort/inc_block_quick_sort.f90"


    !> A subroutine to sort 'vector2' according to 'vector1' using block quick sort (Insertion sort is also used for small vectors)
    !! \param vector a vector to be sorted
    !! \param indices a indices to be sorted accoring to 'vector'
    !! \param num number of samples in vector
    recursive subroutine block_quick_argsort_r4_i4(vector1, vector2, num)
        real(kind=4), intent(inout) :: vector1(num)
        integer(kind=4), intent(inout)   :: vector2(num)
        integer(kind=4), intent(in)    :: num
        integer(kind=4) :: offset_l(15), offset_r(15)
        integer(kind=4) :: start_l, start_r, num_l, num_r, r, l, i, j, num_m
        integer(kind=4) :: idx_l, idx_r, max_idx, next_l, next_r, one
        real(kind=4)    :: pivot, tmp1
        integer(kind=4) :: tmp2
        integer(kind=4) :: three, dev
        real(kind=4)    :: pivots(3)
        include "./include/sort_block_quick_argsort/inc_block_quick_argsort_detail.f90"
        if (next_l .gt. 1)     call block_quick_argsort_r4_i4(vector1(1:next_l),vector2(1:next_l),      next_l)
        if (num-next_l .gt. 1) call block_quick_argsort_r4_i4(vector1(next_l+1:num), vector2(next_l+1:num), num-next_l)
    end subroutine block_quick_argsort_r4_i4
    include "./include/sort_block_quick_argsort/inc_block_quick_argsort.f90"


    !> Get the N-th smallest value
    !! \return return the N-th smallest value
    !! \param the N-th smallest value
    !! \param vector input vector 
    !! \param num_all size of vector
    !! \param num_bottom N from the smallest
    recursive subroutine quick_select_lower_r8(val_lower, vector, num_all, num_bottom)
        implicit none
        real(kind=8), intent(inout)    :: val_lower
        real(kind=8), intent(inout)    :: vector(num_all)
        integer(kind=8), intent(in)    :: num_all
        integer(kind=8), intent(inout) :: num_bottom
        
        integer(kind=8) :: num_lower, num_upper, i, j
        real(kind=8)    :: pivot, tmp

        ! Fast Return
        if (num_all .eq. num_bottom) then
            val_lower = maxval(vector)
            return
        end if

        ! Fast-Retrurn 2
        if ( num_all .le. 32_8 ) then
            call insertion_sort(vector, num_all)
            val_lower = vector(num_bottom)
            return
        end if

        pivot = vector((1+num_all)/2)
        i = 1
        j = num_all
        do
            do while (vector(i) < pivot)
                i=i+1
            end do
            do while (pivot < vector(j))
                j=j-1
            end do
            if (i >= j) exit
            tmp = vector(i);  vector(i) = vector(j);  vector(j) = tmp
            i=i+1
            j=j-1
        end do
        
        num_lower = i-1
        if ( num_lower .lt. num_bottom ) then
            num_bottom = num_bottom - num_lower
            num_upper = num_all - num_lower
            call quick_select_lower_r8(val_lower, vector(num_lower+1:), num_upper, num_bottom)
        elseif (num_lower .ge. num_bottom) then
            call quick_select_lower_r8(val_lower, vector(1:num_lower),  num_lower, num_bottom)
        end if
    end subroutine quick_select_lower_r8


    !> Get the N-th bigget value
    !! \return return the N-th bigget value
    !! \param the N-th bigget value
    !! \param vector input vector 
    !! \param num_all size of vector
    !! \param num_bottom N from the bigget
    recursive subroutine quick_select_upper_r8(val_upper, vector, num_all, num_top)
        implicit none
        real(kind=8), intent(inout)    :: val_upper
        real(kind=8), intent(inout)    :: vector(num_all)
        integer(kind=8), intent(in)    :: num_all
        integer(kind=8), intent(inout) :: num_top
        integer(kind=8) :: num_bottom
        num_bottom = num_all - num_top
        call quick_select_lower_r8(val_upper, vector, num_all, num_bottom)
    end subroutine quick_select_upper_r8



end module mod_sort