!> A module for sort functions.
module mod_sort
    use mod_common
    use mod_random
    implicit none

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
        module procedure :: insertion_argsort_r4_r4
        module procedure :: insertion_argsort_r8_i8
        module procedure :: insertion_argsort_r8_r8
        module procedure :: insertion_argsort_i4_i4
        module procedure :: insertion_argsort_i4_r4
        module procedure :: insertion_argsort_i8_i8
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
        module procedure :: quick_argsort_r4_r4
        module procedure :: quick_argsort_r8_i8
        module procedure :: quick_argsort_r8_r8
        module procedure :: quick_argsort_i4_i4
        module procedure :: quick_argsort_i4_r4
        module procedure :: quick_argsort_i8_i8
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
        module procedure :: block_quick_argsort_r4_r4
        module procedure :: block_quick_argsort_r8_i8
        module procedure :: block_quick_argsort_r8_r8
        module procedure :: block_quick_argsort_i4_i4
        module procedure :: block_quick_argsort_i4_r4
        module procedure :: block_quick_argsort_i8_i8
        module procedure :: block_quick_argsort_i8_r8
    end interface block_quick_argsort

    interface quick_select
        module procedure :: quick_select_r8
    end interface quick_select

    interface quick_argselect
        module procedure :: quick_argselect_r8_r8
        module procedure :: quick_argselect_r8_i8
        module procedure :: quick_argselect_i8_r8
        module procedure :: quick_argselect_i8_i8
    end interface quick_argselect

    interface quick_select_lower
        module procedure :: quick_select_lower_r4
        module procedure :: quick_select_lower_r8
        module procedure :: quick_select_lower_i4
        module procedure :: quick_select_lower_i8
    end interface quick_select_lower

    interface quick_select_upper
        module procedure :: quick_select_upper_r4
        module procedure :: quick_select_upper_r8
        module procedure :: quick_select_upper_i4
        module procedure :: quick_select_upper_i8
    end interface quick_select_upper


    ! =============================================================
    type tmp_work_r4
        real(kind=4), allocatable :: x(:)
    end type tmp_work_r4

    type tmp_work_r8
        real(kind=8), allocatable :: x(:)
    end type tmp_work_r8

    type tmp_work_i4
        integer(kind=4), allocatable :: x(:)
    end type tmp_work_i4

    type tmp_work_i8
        integer(kind=8), allocatable :: x(:)
    end type tmp_work_i8


    ! =============================================================
    type tmp_work_r4_i4
        real(kind=4), allocatable    :: x(:)
        integer(kind=4), allocatable :: y(:)
    end type tmp_work_r4_i4

    type tmp_work_r4_r4
        real(kind=4), allocatable :: x(:)
        real(kind=4), allocatable :: y(:)
    end type tmp_work_r4_r4

    type tmp_work_i4_i4
        integer(kind=4), allocatable :: x(:)
        integer(kind=4), allocatable :: y(:)
    end type tmp_work_i4_i4

    type tmp_work_i4_r4
        integer(kind=4), allocatable :: x(:)
        real(kind=4), allocatable    :: y(:)
    end type tmp_work_i4_r4


    ! =============================================================
    type tmp_work_r8_i8
        real(kind=8), allocatable    :: x(:)
        integer(kind=8), allocatable :: y(:)
    end type tmp_work_r8_i8

    type tmp_work_r8_r8
        real(kind=8), allocatable :: x(:)
        real(kind=8), allocatable :: y(:)
    end type tmp_work_r8_r8

    type tmp_work_i8_i8
        integer(kind=8), allocatable :: x(:)
        integer(kind=8), allocatable :: y(:)
    end type tmp_work_i8_i8

    type tmp_work_i8_r8
        integer(kind=8), allocatable :: x(:)
        real(kind=8), allocatable    :: y(:)
    end type tmp_work_i8_r8


    interface pbucket_sort
        module procedure pbucket_sort_r4
        module procedure pbucket_sort_r8
        module procedure pbucket_sort_i4
        module procedure pbucket_sort_i8
    end interface pbucket_sort

    interface pbucket_argsort
        module procedure pbucket_argsort_r4_i4
        module procedure pbucket_argsort_r4_r4
        module procedure pbucket_argsort_r8_i8
        module procedure pbucket_argsort_r8_r8
        
        module procedure pbucket_argsort_i4_i4
        module procedure pbucket_argsort_i4_r4
        module procedure pbucket_argsort_i8_i8
        module procedure pbucket_argsort_i8_r8
    end interface pbucket_argsort

contains


    !> A subroutine to sort the input vectors using insert sort
    !! \param vector a vector to be sorted
    !! \param num number of samples in vector
    subroutine insertion_sort_r4(vector, num)
        implicit none
        real(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in) :: num
        real(kind=4)                :: tmp_i, tmp_j
        integer(kind=4)             :: i, j
        include "./include/sort/insertion_sort/inc_insertion_sort_detail.f90"
    end subroutine insertion_sort_r4
    include "./include/sort/insertion_sort/inc_insertion_sort.f90"


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
        include "./include/sort/insertion_argsort/inc_insertion_argsort_detail.f90"
    end subroutine insertion_argsort_r4_i4
    include "./include/sort/insertion_argsort/inc_insertion_argsort.f90"


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
        include "./include/sort/quick_sort/inc_quick_sort_detail.f90"        
        if (1 < i-1)    call quick_sort_r4(vector(1:i-1),  i-1)
        if (j+1 < num)  call quick_sort_r4(vector(j+1:num),num-j)
    end subroutine quick_sort_r4
    include "./include/sort/quick_sort/inc_quick_sort.f90"


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
        include "./include/sort/quick_argsort/inc_quick_argsort_detail.f90"        
        if (1 < i-1)    call quick_argsort_r4_i4(vector1(1:i-1),  vector2(1:i-1), i-1)
        if (j+1 < num)  call quick_argsort_r4_i4(vector1(j+1:num),vector2(j+1:num), num-j)
    end subroutine quick_argsort_r4_i4
    include "./include/sort/quick_argsort/inc_quick_argsort.f90"


    !> A subroutine to sort the input vector using block quick sort (Insertion sort is also used for small vectors)
    !! \param vector a vector to be sorted
    !! \param indices a indices to be sorted accoring to 'vector'
    !! \param num number of samples in vector
    recursive subroutine block_quick_sort_r4(vector, num)
        real(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in) :: num
        integer(kind=4)             :: offset_l(31), offset_r(31)
        integer(kind=4)             :: start_l, start_r, num_l, num_r, r, l, i, j, num_m
        integer(kind=4)             :: idx_l, idx_r, max_idx, next_l, next_r, one
        real(kind=4)                :: pivot, tmp
        integer(kind=4)             :: three, dev
        real(kind=4)                :: dev_rand
        real(kind=4)                :: pivots(3)
        include "./include/sort/block_quick_sort/inc_block_quick_sort_detail.f90"
        if (next_l .gt. 1)     call block_quick_sort_r4(vector(1:next_l),     next_l)
        if (num-next_l .gt. 1) call block_quick_sort_r4(vector(next_l+1:num), num-next_l)
    end subroutine block_quick_sort_r4
    include "./include/sort/block_quick_sort/inc_block_quick_sort.f90"


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
        real(kind=4)    :: pivots(3), pivot, tmp1
        integer(kind=4) :: tmp2
        integer(kind=4) :: three
        include "./include/sort/block_quick_argsort/inc_block_quick_argsort_detail.f90"
        if (next_l .gt. 1)     call block_quick_argsort_r4_i4(vector1(1:next_l),vector2(1:next_l),      next_l)
        if (num-next_l .gt. 1) call block_quick_argsort_r4_i4(vector1(next_l+1:num), vector2(next_l+1:num), num-next_l)
    end subroutine block_quick_argsort_r4_i4
    include "./include/sort/block_quick_argsort/inc_block_quick_argsort.f90"


    !> A subroutine to sort the input vector using pseudo-bucket sort (quick sort is also used for small vectors)
    !! \param vector a vector to be sorted
    !! \param num number of samples in vector
    !! \param rec_depth *do not use* recursive depth
    recursive subroutine pbucket_sort_r4(vector, n_samples, rec_depth)
        implicit none
        real(kind=4), intent(inout)           :: vector(n_samples)
        integer(kind=4), intent(in)           :: n_samples
        integer(kind=4), optional, intent(in) :: rec_depth

        integer(kind=4)                :: rec_depth_opt
        real(kind=4)                   :: vector_min, vector_max
        integer(kind=4)                :: n_sampling
        integer(kind=4), allocatable   :: indices(:)
        real(kind=4), allocatable      :: values(:)
        integer(kind=4)                :: two=2, one=1, zero=0
        integer(kind=4)                :: i, idx
        integer(kind=4)                :: n_unique, n_buckets
        integer(kind=4)                :: left_posi
        ! =============================================================
        type(tmp_work_r4), allocatable :: works(:)
        integer(kind=4), allocatable :: counters(:), sizes(:)
        integer(kind=4) :: b, min_bucket_size
        integer(kind=4) :: thousand=1000
        real(kind=8)    :: diff, max_val, factor
        real(kind=4)    :: tmp_v, cnt_b, size_b
        integer(kind=4) :: bucket_idx
        real(kind=4), allocatable :: tmp_x(:)
        integer(kind=4) :: k, j, ini, fin
        integer(kind=4) :: n_samples_unroll
        real(kind=4) :: buffer_v(63)
        integer(kind=4) :: buffer_b(63)
        integer(kind=4) :: buffuer_size=63
        include "./include/sort/pbucket_sort/inc_pbucket_sort_detail.f90"
    end subroutine pbucket_sort_r4
    include "./include/sort/pbucket_sort/inc_pbucket_sort.f90"


    !> A subroutine to sort 'indices' according to 'vector' using pseudo-bucket sort (quick sort is also used for small vectors)
    !! \param vector a vector to be sorted
    !! \param indices a indices to be sorted
    !! \param num number of samples in vector
    !! \param rec_depth *do not use* recursive depth
    recursive subroutine pbucket_argsort_r4_i4(vector, indices, n_samples, rec_depth)
        implicit none
        real(kind=4), intent(inout)           :: vector(n_samples)
        integer(kind=4), intent(inout)        :: indices(n_samples)
        integer(kind=4), intent(in)           :: n_samples
        integer(kind=4), optional, intent(in) :: rec_depth

        integer(kind=4)                :: rec_depth_opt
        real(kind=4)                   :: vector_min, vector_max
        integer(kind=4)                :: n_sampling
        integer(kind=4), allocatable   :: sampling_indices(:)
        real(kind=4), allocatable      :: values(:)
        integer(kind=4)                :: two=2, one=1, zero=0
        integer(kind=4)                :: i, idx
        integer(kind=4)                :: n_unique, n_buckets
        integer(kind=4)                :: left_posi
        ! =============================================================
        type(tmp_work_r4_i4), allocatable :: works(:)
        integer(kind=4), allocatable :: counters(:), sizes(:)
        integer(kind=4) :: b, min_bucket_size
        integer(kind=4) :: thousand=1000
        real(kind=8)    :: diff, max_val, factor
        real(kind=4)    :: tmp_v
        integer(kind=4) :: cnt_b, size_b
        integer(kind=4) :: bucket_idx
        real(kind=4), allocatable :: tmp_x(:)
        integer(kind=4), allocatable :: tmp_y(:)
        integer(kind=4) :: k, j, ini, fin
        integer(kind=4) :: n_samples_unroll
        integer(kind=4) :: tmp_i
        include "./include/sort/pbucket_argsort/inc_pbucket_argsort_detail.f90"
    end subroutine pbucket_argsort_r4_i4
    include "./include/sort/pbucket_argsort/inc_pbucket_argsort.f90"

    !> Get the N-th Value
    !! \param vector input vector to extract n_th element.
    !! \param n_samples size of vector
    !! \param n_th n-th index
    recursive subroutine quick_select_r8(vector, n_samples, n_th)
        implicit none
        real(kind=8), intent(inout) :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8), intent(in) :: n_th

        real(kind=8) :: pivot, tmp
        integer(kind=8) :: i, j, idx, n_th_new

        if (n_samples .le. 32_8) then
            call insertion_sort(vector, n_samples)
            return
        end if

        idx = (1_8+n_samples) / 2_8
        pivot = vector(idx)
        i = 1
        j = n_samples

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

        if (n_th .le. j) then
            call quick_select_r8(vector(1:j), j, n_th)
        else
            call quick_select_r8(vector(i:), n_samples-i+1, n_th-i+1)
        end if
    end subroutine quick_select_r8

    !> Get the N-th Value
    !! \param vector input vector to extract n_th element.
    !! \param n_samples size of vector
    !! \param n_th n-th index
    recursive subroutine quick_argselect_r8_i8(vector, indices, n_samples, n_th)
        implicit none
        real(kind=8), intent(inout) :: vector(n_samples)
        integer(kind=8), intent(inout) :: indices(n_samples)
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8), intent(in) :: n_th

        real(kind=8) :: pivot, tmp_r, pivots(3)
        integer(kind=8) :: i, j, idx, tmp_i 

        if (n_samples .le. 32_8) then
            call insertion_argsort(vector, indices, n_samples)
            return
        end if


        pivots(1) = vector(1)
        pivots(2) = vector((1_8+n_samples)/2_8)
        pivots(3) = vector(n_samples)
        call insertion_sort(pivots, 3_8)
        pivot = pivots(2)
        i = 1
        j = n_samples

        do
            do while (vector(i) < pivot)
                i=i+1
            end do
            do while (pivot < vector(j))
                j=j-1
            end do
            if (i >= j) exit
            tmp_r = vector(i);  vector(i)  = vector(j);  vector(j)  = tmp_r
            tmp_i = indices(i); indices(i) = indices(j); indices(j) = tmp_i
            i=i+1
            j=j-1
        end do

        if (n_th .le. j) then
            call quick_argselect_r8_i8(vector(1:j), indices(1:j), j, n_th)
        else
            call quick_argselect_r8_i8(vector(i:), indices(i:), n_samples-i+1, n_th-i+1)
        end if
    end subroutine quick_argselect_r8_i8

    !> Get the N-th Value
    !! \param vector input vector to extract n_th element.
    !! \param n_samples size of vector
    !! \param n_th n-th index
    recursive subroutine quick_argselect_r8_r8(vector, indices, n_samples, n_th)
        implicit none
        real(kind=8), intent(inout) :: vector(n_samples)
        real(kind=8), intent(inout) :: indices(n_samples)
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8), intent(in) :: n_th

        real(kind=8) :: pivot, tmp_r, pivots(3), tmp_i
        integer(kind=8) :: i, j, idx 

        if (n_samples .le. 32_8) then
            call insertion_argsort(vector, indices, n_samples)
            return
        end if


        pivots(1) = vector(1)
        pivots(2) = vector((1_8+n_samples)/2_8)
        pivots(3) = vector(n_samples)
        call insertion_sort(pivots, 3_8)
        pivot = pivots(2)
        i = 1
        j = n_samples

        do
            do while (vector(i) < pivot)
                i=i+1
            end do
            do while (pivot < vector(j))
                j=j-1
            end do
            if (i >= j) exit
            tmp_r = vector(i);  vector(i)  = vector(j);  vector(j)  = tmp_r
            tmp_i = indices(i); indices(i) = indices(j); indices(j) = tmp_i
            i=i+1
            j=j-1
        end do

        if (n_th .le. j) then
            call quick_argselect_r8_r8(vector(1:j), indices(1:j), j, n_th)
        else
            call quick_argselect_r8_r8(vector(i:), indices(i:), n_samples-i+1, n_th-i+1)
        end if
    end subroutine quick_argselect_r8_r8

    !> Get the N-th Value
    !! \param vector input vector to extract n_th element.
    !! \param n_samples size of vector
    !! \param n_th n-th index
    recursive subroutine quick_argselect_i8_i8(vector, indices, n_samples, n_th)
        implicit none
        integer(kind=8), intent(inout) :: vector(n_samples)
        integer(kind=8), intent(inout) :: indices(n_samples)
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8), intent(in) :: n_th

        real(kind=8) :: pivot, tmp_r, pivots(3)
        integer(kind=8) :: i, j, idx, tmp_i 

        if (n_samples .le. 32_8) then
            call insertion_argsort(vector, indices, n_samples)
            return
        end if


        pivots(1) = vector(1)
        pivots(2) = vector((1_8+n_samples)/2_8)
        pivots(3) = vector(n_samples)
        call insertion_sort(pivots, 3_8)
        pivot = pivots(2)
        i = 1
        j = n_samples

        do
            do while (vector(i) < pivot)
                i=i+1
            end do
            do while (pivot < vector(j))
                j=j-1
            end do
            if (i >= j) exit
            tmp_r = vector(i);  vector(i)  = vector(j);  vector(j)  = tmp_r
            tmp_i = indices(i); indices(i) = indices(j); indices(j) = tmp_i
            i=i+1
            j=j-1
        end do

        if (n_th .le. j) then
            call quick_argselect_i8_i8(vector(1:j), indices(1:j), j, n_th)
        else
            call quick_argselect_i8_i8(vector(i:), indices(i:), n_samples-i+1, n_th-i+1)
        end if
    end subroutine quick_argselect_i8_i8

    !> Get the N-th Value
    !! \param vector input vector to extract n_th element.
    !! \param n_samples size of vector
    !! \param n_th n-th index
    recursive subroutine quick_argselect_i8_r8(vector, indices, n_samples, n_th)
        implicit none
        integer(kind=8), intent(inout) :: vector(n_samples)
        real(kind=8), intent(inout) :: indices(n_samples)
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8), intent(in) :: n_th

        real(kind=8) :: pivot, tmp_r, pivots(3), tmp_i 
        integer(kind=8) :: i, j, idx

        if (n_samples .le. 32_8) then
            call insertion_argsort(vector, indices, n_samples)
            return
        end if


        pivots(1) = vector(1)
        pivots(2) = vector((1_8+n_samples)/2_8)
        pivots(3) = vector(n_samples)
        call insertion_sort(pivots, 3_8)
        pivot = pivots(2)
        i = 1
        j = n_samples

        do
            do while (vector(i) < pivot)
                i=i+1
            end do
            do while (pivot < vector(j))
                j=j-1
            end do
            if (i >= j) exit
            tmp_r = vector(i);  vector(i)  = vector(j);  vector(j)  = tmp_r
            tmp_i = indices(i); indices(i) = indices(j); indices(j) = tmp_i
            i=i+1
            j=j-1
        end do

        if (n_th .le. j) then
            call quick_argselect_i8_r8(vector(1:j), indices(1:j), j, n_th)
        else
            call quick_argselect_i8_r8(vector(i:), indices(i:), n_samples-i+1, n_th-i+1)
        end if
    end subroutine quick_argselect_i8_r8




    !> Get the N-th smallest value
    !! \return return the N-th smallest value
    !! \param the N-th smallest value
    !! \param vector input vector 
    !! \param num_all size of vector
    !! \param num_bottom N from the smallest
    recursive subroutine quick_select_lower_r4(val_lower, vector, num_all, num_bottom)
        implicit none
        real(kind=4), intent(inout)    :: val_lower
        real(kind=4), intent(inout)    :: vector(num_all)
        integer(kind=4), intent(in)    :: num_all
        integer(kind=4), intent(inout) :: num_bottom
        
        integer(kind=4) :: num_lower, num_upper, i, j
        real(kind=4)    :: pivot, tmp
        include "./include/sort/quick_select/inc_quick_select_lower_detail.f90"
    end subroutine quick_select_lower_r4
    include "./include/sort/quick_select/inc_quick_select_lower.f90"


    !> Get the N-th bigget value
    !! \return return the N-th bigget value
    !! \param the N-th bigget value
    !! \param vector input vector 
    !! \param num_all size of vector
    !! \param num_bottom N from the bigget
    recursive subroutine quick_select_upper_r4(val_upper, vector, num_all, num_top)
        implicit none
        real(kind=4), intent(inout)    :: val_upper
        real(kind=4), intent(inout)    :: vector(num_all)
        integer(kind=4), intent(in)    :: num_all
        integer(kind=4), intent(inout) :: num_top
        integer(kind=4) :: num_bottom
        num_bottom = num_all - num_top
        call quick_select_lower(val_upper, vector, num_all, num_bottom)
    end subroutine quick_select_upper_r4
    include "./include/sort/quick_select/inc_quick_select_upper.f90"



end module mod_sort