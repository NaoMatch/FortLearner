! gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90 main_sort.f90 -o main_sort.out
program main_sort
    use mod_common, only: is_sorted
    use mod_timer
    use mod_sort
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8), tot_time, iter, max_iter, set_bucket_size, b_size, b_step_size

    integer(kind=4) :: n_samples_i4, n_unique_i4
    integer(kind=8) :: n_samples_i8, n_unique_i8
    real(kind=4), allocatable :: vector_r4(:), vector_copy_r4(:)
    real(kind=8), allocatable :: vector_r8(:), vector_copy_r8(:)
    integer(kind=4), allocatable :: indices_i4(:)
    integer(kind=8), allocatable :: indices_i8(:)

    integer(kind=8) :: i

    max_iter = 10
    n_samples_i4 = 10000000
    n_samples_i8 = 10000000_8
    n_unique_i4 = int(sqrt(n_unique_i4+0.0), kind=kind(n_unique_i4))
    n_unique_i8 = int(sqrt(n_unique_i8+0.0), kind=kind(n_unique_i8))
    n_unique_i4 = 100
    n_unique_i8 = 100
    allocate(vector_r4(n_samples_i4), vector_copy_r4(n_samples_i4))
    allocate(vector_r8(n_samples_i8), vector_copy_r8(n_samples_i8))
    allocate(indices_i4(n_samples_i4))
    allocate(indices_i8(n_samples_i8))
    print*, '============================================================='
    print*, "Randomize vector"
    call random_number(vector_r4)
    call random_number(vector_r8)

    vector_r4 = int(vector_r4 * n_unique_i4, kind=kind(vector_r4))
    vector_r8 = int(vector_r8 * n_unique_i8, kind=kind(vector_r8))


    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Quick Sort, Random Uniform KIND=4"
    print*, "    number of samples: ", n_samples_i4
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r4)
        do i=1, n_samples_i4, 1
            vector_copy_r4(i) = vector_r4(i)
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value1)
        call quick_sort(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r4, n_samples_i4)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"


    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Pseudo Bucket Sort, Random Uniform KIND=4"
    print*, "    number of samples: ", n_samples_i4
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r4)
        do i=1, n_samples_i4, 1
            vector_copy_r4(i) = vector_r4(i)
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value1)
        call pbucket_sort(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r4, n_samples_i4)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"




    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Quick ArgSort, Random Uniform KIND=4"
    print*, "    number of samples: ", n_samples_i4
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r4)
        do i=1, n_samples_i4, 1
            vector_copy_r4(i) = vector_r4(i)
            indices_i4(i) = i
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value1)
        call quick_argsort(vector_copy_r4, indices_i4, n_samples_i4)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r4, n_samples_i4)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"


    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Pseudo Bucket ArgSort, Random Uniform KIND=4"
    print*, "    number of samples: ", n_samples_i4
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r4)
        do i=1, n_samples_i4, 1
            vector_copy_r4(i) = vector_r4(i)
            indices_i4(i) = i
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value1)
        call pbucket_argsort(vector_copy_r4, indices_i4, n_samples_i4)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r4, n_samples_i4)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"



    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Quick ArgSort, Random Uniform KIND=8"
    print*, "    number of samples: ", n_samples_i8
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r8)
        do i=1, n_samples_i8, 1
            vector_copy_r8(i) = vector_r8(i)
            indices_i8(i) = i
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r8, n_samples_i8)
        call date_and_time(values=date_value1)
        call quick_argsort(vector_copy_r8, indices_i8, n_samples_i8)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r8, n_samples_i8)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"


    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Pseudo Bucket ArgSort, Random Uniform KIND=8"
    print*, "    number of samples: ", n_samples_i8
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r8)
        do i=1, n_samples_i8, 1
            vector_copy_r8(i) = vector_r8(i)
            indices_i8(i) = i
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r8, n_samples_i8)
        call date_and_time(values=date_value1)
        call pbucket_argsort(vector_copy_r8, indices_i8, n_samples_i8)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r8, n_samples_i8)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"
    ! stop

    ! max_iter = 1
    ! n_samples_i4 = 1000000
    ! n_samples_i8 = 1000000_8
    ! n_unique_i4 = int(sqrt(n_unique_i4+0.0), kind=kind(n_unique_i4))
    ! n_unique_i8 = int(sqrt(n_unique_i8+0.0), kind=kind(n_unique_i8))
    ! allocate(vector_r4(n_samples_i4), vector_copy_r4(n_samples_i4))
    ! allocate(vector_r8(n_samples_i8), vector_copy_r8(n_samples_i8))
    ! allocate(indices_i4(n_samples_i4))
    ! allocate(indices_i8(n_samples_i8))
    ! print*, '============================================================='
    ! print*, "Randomize vector"
    ! call random_number(vector_r4)
    ! call random_number(vector_r8)

    ! vector_r4(n_samples_i4/2) = 10000000
    ! vector_r8(n_samples_i8/2) = 10000000

    ! print*, '============================================================='
    ! print*, '============================================================='
    ! print*, '============================================================='
    ! print*, "Pseudo Bucket ArgSort, Random Uniform KIND=8"
    ! print*, "    number of samples: ", n_samples_i8

    ! print*, "100000", sqrt(100000+0.0), 100, 100/100000.0
    ! print*, "1000000", sqrt(1000000+0.0), 1000, 1000/1000000.0
    ! print*, "10000000", sqrt(10000000+0.0), 10000, 10000/10000000.0

    ! b_step_size = 500
    ! set_bucket_size = b_step_size
    ! do b_size=1, 100
    !     tot_time = 0
    !     do iter=1, max_iter
    !         call random_number(vector_r8)
    !         do i=1, n_samples_i8, 1
    !             vector_copy_r8(i) = vector_r8(i)
    !             ! indices_i8(i) = i
    !         end do
    !         ! print*, "    is sorted(before): ", is_sorted(vector_copy_r8, n_samples_i8)
    !         call date_and_time(values=date_value1)
    !         ! call pbucket_argsort_change_n_buckets_r8_i8(vector_copy_r8, indices_i8, n_samples_i8, set_bucket_size=set_bucket_size)
    !         call pbucket_sort_change_n_buckets_r8(vector_copy_r8, n_samples_i8, set_bucket_size=set_bucket_size)
    !         call date_and_time(values=date_value2)
    !         tot_time = tot_time + time_diff(date_value1, date_value2)
    !     end do
    !     print*, "    Time: ", set_bucket_size/dble(n_samples_i8), set_bucket_size, tot_time/dble(max_iter), "[msec]"
    !     ! print*, "    Time: ", set_bucket_size, int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"
    !     set_bucket_size = set_bucket_size + b_step_size
    ! end do

contains

    recursive subroutine pbucket_argsort_change_n_buckets_r8_i8(vector, indices, n_samples, set_bucket_size, rec_depth)
        implicit none
        real(kind=8), intent(inout)           :: vector(n_samples)
        integer(kind=8), intent(inout)        :: indices(n_samples)
        integer(kind=8), intent(in)           :: n_samples
        integer(kind=8), optional, intent(in) :: rec_depth
        integer(kind=8), intent(in)           :: set_bucket_size

        integer(kind=8)                :: rec_depth_opt
        real(kind=8)                   :: vector_min, vector_max
        integer(kind=8)                :: n_sampling
        integer(kind=8), allocatable   :: sampling_indices(:)
        real(kind=8), allocatable      :: values(:)
        integer(kind=8)                :: two=2, one=1, zero=0
        integer(kind=8)                :: i, idx
        integer(kind=8)                :: n_unique, n_buckets
        integer(kind=8)                :: left_posi
        ! =============================================================
        type(tmp_work_r8_i8), allocatable :: works(:)
        integer(kind=8), allocatable :: counters(:), sizes(:)
        integer(kind=8) :: b, min_bucket_size
        integer(kind=8) :: thousand=1000
        real(kind=8)    :: diff, max_val, factor
        real(kind=8)    :: tmp_v
        integer(kind=8) :: cnt_b, size_b
        integer(kind=8) :: bucket_idx
        real(kind=8), allocatable :: tmp_x(:)
        integer(kind=8), allocatable :: tmp_y(:)
        integer(kind=8) :: k, j, ini, fin
        integer(kind=8) :: n_samples_unroll
        integer(kind=8) :: tmp_i

        if (n_samples .eq. 0) return ! Fast Return

        vector_min = minval(vector)
        vector_max = maxval(vector)
        if (vector_min .eq. vector_max) return ! Fast Return

        rec_depth_opt = 0
        if (present(rec_depth)) rec_depth_opt = rec_depth

        ! -----------------------------------------------------------------------
        n_sampling = sqrt(real(n_samples, kind=kind(n_sampling)))
        allocate(sampling_indices(n_sampling))
        allocate(values(n_sampling))
        call rand_integer(one, n_samples, sampling_indices, n_sampling)
        call quick_sort(sampling_indices, n_sampling)
        do i=1, n_sampling, 1
            idx = sampling_indices(i)
            values(i) = vector(idx)
        end do
        call quick_sort(values, n_sampling)
        n_unique = count_unique(values, n_sampling)
        if (n_unique .le. 10) then
            call quick_argsort(vector, indices, n_samples)
            return
        ! elseif (n_unique .gt. int(n_samples * 0.98, kind=kind(n_unique))) then
        !     left_posi = most_left_bit_position(n_unique)
        !     n_buckets = ibset(zero, left_posi+one)
        else
            n_buckets = n_unique
        end if

        n_buckets = set_bucket_size

        ! -----------------------------------------------------------------------
        allocate(works(n_buckets))
        allocate(counters(n_buckets))
        allocate(sizes(n_buckets))
        min_bucket_size = maxval((/ thousand, n_samples/n_buckets /))
        counters = 1
        sizes = min_bucket_size
        do b=1, n_buckets, 1
            allocate( works(b)%x(min_bucket_size) )
            allocate( works(b)%y(min_bucket_size) )
        end do
        diff = 1.0 / (vector_max-vector_min)
        max_val = n_buckets-1.0
        factor = diff * max_val
        diff = - vector_min * factor + 1

        do i=1, n_samples, 1
            tmp_v = vector(i)
            tmp_i = indices(i)
            bucket_idx = int( tmp_v * factor + diff, kind=kind(bucket_idx))
            cnt_b = counters(bucket_idx)
            size_b = sizes(bucket_idx)

            if ( cnt_b .gt. size_b ) then
                allocate(tmp_x(size_b), tmp_y(size_b))
                do j=1, size_b, 1
                    tmp_x(j) = works(bucket_idx) % x(j)
                    tmp_y(j) = works(bucket_idx) % y(j)
                end do
                deallocate(works(bucket_idx) % x, works(bucket_idx) % y)
                allocate(works(bucket_idx) % x(size_b*two), works(bucket_idx) % y(size_b*two))
                do j=1, size_b, 1
                    works(bucket_idx) % x(j) = tmp_x(j)
                    works(bucket_idx) % y(j) = tmp_y(j)
                end do
                deallocate(tmp_x, tmp_y)
                sizes(bucket_idx) = size_b*two
            end if
            works(bucket_idx) % x(cnt_b) = tmp_v
            works(bucket_idx) % y(cnt_b) = tmp_i

            counters(bucket_idx) = cnt_b + 1
        end do

        ! Sort Arrays
        ini = 1
        do b=1, n_buckets
            fin = ini + counters(b) - 2
            call quick_argsort(works(b) % x(1:counters(b)-1), works(b) % y(1:counters(b)-1), counters(b)-1)
            vector(ini:fin) = works(b) % x(1:counters(b)-1)
            indices(ini:fin) = works(b) % y(1:counters(b)-1)
            ini = fin + 1
        end do
    end subroutine pbucket_argsort_change_n_buckets_r8_i8


    recursive subroutine pbucket_sort_change_n_buckets_r8(vector, n_samples, set_bucket_size, rec_depth)
        implicit none
        real(kind=8), intent(inout)           :: vector(n_samples)
        integer(kind=8), intent(in)           :: n_samples
        integer(kind=8), intent(in)           :: set_bucket_size
        integer(kind=8), optional, intent(in) :: rec_depth

        integer(kind=8)                :: rec_depth_opt
        real(kind=8)                   :: vector_min, vector_max
        integer(kind=8)                :: n_sampling
        integer(kind=8), allocatable   :: indices(:)
        real(kind=8), allocatable      :: values(:)
        integer(kind=8)                :: two=2, one=1, zero=0
        integer(kind=8)                :: i, idx
        integer(kind=8)                :: n_unique, n_buckets
        integer(kind=8)                :: left_posi
        ! =============================================================
        type(tmp_work_r8), allocatable :: works(:)
        integer(kind=8), allocatable :: counters(:), sizes(:)
        integer(kind=8) :: b, min_bucket_size
        integer(kind=8) :: thousand=1000
        real(kind=8)    :: diff, max_val, factor
        real(kind=8)    :: tmp_v, cnt_b, size_b
        integer(kind=8) :: bucket_idx
        real(kind=8), allocatable :: tmp_x(:)
        integer(kind=8) :: k, j, ini, fin
        integer(kind=8) :: n_samples_unroll
        real(kind=8) :: buffer_v(63)
        integer(kind=8) :: buffer_b(63)
        integer(kind=8) :: buffuer_size=63
        if (n_samples .eq. 0) return ! Fast Return

        vector_min = minval(vector)
        vector_max = maxval(vector)
        if (vector_min .eq. vector_max) return ! Fast Return

        rec_depth_opt = 0
        if (present(rec_depth)) rec_depth_opt = rec_depth

        ! -----------------------------------------------------------------------
        n_sampling = sqrt(real(n_samples, kind=kind(n_sampling)))
        allocate(indices(n_sampling))
        allocate(values(n_sampling))
        call rand_integer(one, n_samples, indices, n_sampling)
        call quick_sort(indices, n_sampling)
        do i=1, n_sampling, 1
            idx = indices(i)
            values(i) = vector(idx)
        end do
        call quick_sort(values, n_sampling)
        n_unique = count_unique(values, n_sampling)
        if (n_unique .le. 10) then
            call quick_sort(vector, n_samples)
            return
        elseif (n_unique .gt. int(n_samples * 0.98, kind=kind(n_unique))) then
            left_posi = most_left_bit_position(n_unique)
            n_buckets = ibset(zero, left_posi+one)
        else
            n_buckets = n_unique
        end if
        n_buckets = set_bucket_size

        ! -----------------------------------------------------------------------
        allocate(works(n_buckets))
        allocate(counters(n_buckets))
        allocate(sizes(n_buckets))
        min_bucket_size = maxval((/ thousand, n_samples/n_buckets /))
        counters = 1
        sizes = min_bucket_size
        do b=1, n_buckets, 1
            allocate( works(b)%x(min_bucket_size) )
        end do
        diff = 1.0 / (vector_max-vector_min)
        max_val = n_buckets-1.0
        factor = diff * max_val
        diff = - vector_min * factor + 1
        n_samples_unroll = n_samples - mod(n_samples, buffuer_size)

        do i=1, n_samples_unroll, buffuer_size
            do k=0, buffuer_size-1, 1
                tmp_v = vector(i+k)
                buffer_v(k+1) = tmp_v
                buffer_b(k+1) = int( tmp_v * factor + diff, kind=kind(bucket_idx))
            end do

            do k=0, buffuer_size-1, 1
                tmp_v = buffer_v(k+1)
                bucket_idx = buffer_b(k+1)
                cnt_b = counters(bucket_idx)
                size_b = sizes(bucket_idx)
                if ( cnt_b .gt. size_b ) then
                    allocate(tmp_x(size_b))
                    do j=1, size_b, 1
                        tmp_x(j) = works(bucket_idx) % x(j)
                    end do
                    deallocate(works(bucket_idx) % x)
                    allocate(works(bucket_idx) % x(size_b*two))
                    do j=1, size_b, 1
                        works(bucket_idx) % x(j) = tmp_x(j)
                    end do
                    deallocate(tmp_x)
                    sizes(bucket_idx) = size_b*two
                end if
                works(bucket_idx) % x(cnt_b) = tmp_v

                counters(bucket_idx) = cnt_b + 1
            end do
        end do

        do i=n_samples_unroll+1, n_samples, 1
            tmp_v = vector(i)
            bucket_idx = int( tmp_v * factor + diff, kind=kind(bucket_idx))
            cnt_b = counters(bucket_idx)
            size_b = sizes(bucket_idx)

            if ( cnt_b .gt. size_b ) then
                allocate(tmp_x(size_b))
                do j=1, size_b, 1
                    tmp_x(j) = works(bucket_idx) % x(j)
                end do
                deallocate(works(bucket_idx) % x)
                allocate(works(bucket_idx) % x(size_b*two))
                do j=1, size_b, 1
                    works(bucket_idx) % x(j) = tmp_x(j)
                end do
                deallocate(tmp_x)
                sizes(bucket_idx) = size_b*two
            end if
            works(bucket_idx) % x(cnt_b) = tmp_v

            counters(bucket_idx) = cnt_b + 1
        end do

        if (rec_depth_opt .le. max_rec_depth) then ! 0(Only One Time Recursion) is Best?
            ini = 1
            do b=1, n_buckets
                fin = ini + counters(b) - 2
                vector(ini:fin) = works(b) % x(1:counters(b)-1)
                call pbucket_sort(vector(ini:fin), fin-ini+1, rec_depth_opt+1)
                ini = fin + 1
            end do
        else
            ! Sort Arrays
            ini = 1
            do b=1, n_buckets
                fin = ini + counters(b) - 2
                call quick_sort(works(b) % x(1:counters(b)-1), counters(b)-1)
                vector(ini:fin) = works(b) % x(1:counters(b)-1)
                ini = fin + 1
            end do
        end if
    end subroutine pbucket_sort_change_n_buckets_r8
end program main_sort
