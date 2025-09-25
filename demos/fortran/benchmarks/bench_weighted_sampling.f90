! Benchmark to compare weighted sampling implementations:
!  - prefix-sum + binary search (logarithmic per draw)
!  - weighted reservoir sampling (linear per draw)
! 常設ベンチとして残し、make bench-weighted から実行できるようにする。
program bench_weighted_sampling
    use :: mod_kinds
    use :: mod_prefix_sum
    use :: mod_prefix_sum_selection, only : prefix_sum_selection_r64
    use :: mod_weighted_reservoir,  only : weighted_reservoir_single
    implicit none

    integer, parameter :: n_sizes = 4
    integer, parameter :: sizes(n_sizes)        = [1000, 10000, 100000, 1000000]
    integer, parameter :: query_counts(n_sizes) = [200000, 50000, 5000, 500]

    integer :: k, i
    integer :: count_start, count_end, count_rate, count_max, ticks
    integer(i64) :: size_i64, queries_i64
    integer(i64) :: q, idx64
    integer(i64) :: acc_prefix_sum, acc_reservoir
    real(r64) :: elapsed_prefix_sum, elapsed_reservoir
    real(r64), allocatable :: weights(:)
    real(r64), allocatable :: prefix(:)

    call init_seed()
    call system_clock(count_rate=count_rate, count_max=count_max)
    if (count_rate <= 0) then
        write(*, '(a)') 'system_clock rate is not positive; aborting benchmark.'
        stop 1
    end if

    write(*, '(a)') '--- Weighted Sampling Benchmark ---'
    write(*, '(a/)') 'Comparing prefix-sum selection and weighted reservoir sampling.'

    do k = 1, n_sizes
        size_i64    = int(sizes(k), kind=i64)
        queries_i64 = int(query_counts(k), kind=i64)

        allocate(weights(sizes(k)))
        allocate(prefix(sizes(k)))
        do i = 1, sizes(k)
            weights(i) = 1.0_r64 + real(i - 1, kind=r64)
        end do
        acc_prefix_sum = 0_i64
        call system_clock(count_start)
        do q = 1_i64, queries_i64
            ! Recompute prefix sums every draw to model scenarios without cached prefix arrays.
            call prefix_sum_r64(weights, prefix, size_i64)
            idx64 = prefix_sum_selection_r64(prefix, size_i64)
            if (idx64 < 1_i64) idx64 = 1_i64
            if (idx64 > size_i64) idx64 = size_i64
            acc_prefix_sum = acc_prefix_sum + idx64
        end do
        call system_clock(count_end)
        ticks = count_end - count_start
        if (count_max > 0 .and. ticks < 0) ticks = ticks + count_max
        elapsed_prefix_sum = real(ticks, kind=r64) / real(count_rate, kind=r64)

        acc_reservoir = 0_i64
        call system_clock(count_start)
        do q = 1_i64, queries_i64
            idx64 = weighted_reservoir_single(weights, size_i64)
            if (idx64 < 1_i64) idx64 = 1_i64
            if (idx64 > size_i64) idx64 = size_i64
            acc_reservoir = acc_reservoir + idx64
        end do
        call system_clock(count_end)
        ticks = count_end - count_start
        if (count_max > 0 .and. ticks < 0) ticks = ticks + count_max
        elapsed_reservoir = real(ticks, kind=r64) / real(count_rate, kind=r64)

        write(*, '(a,i0,a,i0,a)') 'size=', sizes(k), ', queries=', query_counts(k), ' -- timings (s):'
        write(*, '(a,f10.6,a,i0)') '  prefix-sum : ', elapsed_prefix_sum, '  checksum=', acc_prefix_sum
        write(*, '(a,f10.6,a,i0)') '  reservoir  : ', elapsed_reservoir,  '  checksum=', acc_reservoir
        write(*, '(a/)') ' '

        deallocate(weights)
        deallocate(prefix)
    end do

contains

    subroutine init_seed()
        integer :: sz, i
        integer, allocatable :: seed(:)

        call random_seed(size=sz)
        allocate(seed(sz))
        do i = 1, sz
            seed(i) = 13579 + 73 * (i - 1)
        end do
        call random_seed(put=seed)
        deallocate(seed)
    end subroutine init_seed

end program bench_weighted_sampling
