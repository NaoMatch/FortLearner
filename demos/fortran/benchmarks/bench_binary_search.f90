! Benchmark to compare branchless binary search with classic binary/linear searches.
! 常設ベンチとして残し、make bench-search から実行できるようにする。
program bench_binary_search
    use :: mod_kinds
    use :: mod_binary_search, only : binary_search_r64
    implicit none

    integer, parameter :: n_sizes = 4
    integer, parameter :: sizes(n_sizes)        = [1000, 10000, 100000, 1000000]
    integer, parameter :: query_counts(n_sizes) = [1000000, 200000, 20000, 2000]

    integer :: k, i
    integer :: count_start, count_end, count_rate, count_max, ticks
    integer(i64) :: size_i64, queries_i64
    integer(i64) :: q
    integer(i64) :: acc_branchless, acc_branchy, acc_linear
    real(r64) :: elapsed_branchless, elapsed_branchy, elapsed_linear
    real(r64) :: val
    real(r64), allocatable :: vec(:)

    call system_clock(count_rate=count_rate, count_max=count_max)
    if (count_rate <= 0) then
        write(*, '(a)') 'system_clock rate is not positive; aborting benchmark.'
        stop 1
    end if

    write(*, '(a)') '--- Binary Search Benchmark ---'
    write(*, '(a)') 'Comparing branchless, classic binary, and linear search implementations.'
    write(*, '(a/)') 'Values are lower-bound insertion indices. Checksums prevent optimization.'

    do k = 1, n_sizes
        size_i64    = int(sizes(k), kind=i64)
        queries_i64 = int(query_counts(k), kind=i64)

        allocate(vec(sizes(k)))
        do i = 1, sizes(k)
            vec(i) = real(i, kind=r64)
        end do

        ! Branchless binary search (production implementation)
        acc_branchless = 0_i64
        call system_clock(count_start)
        do q = 1_i64, queries_i64
            val = make_query(q, size_i64)
            acc_branchless = acc_branchless + binary_search_r64(vec, val, size_i64)
        end do
        call system_clock(count_end)
        ticks = count_end - count_start
        if (count_max > 0 .and. ticks < 0) ticks = ticks + count_max
        elapsed_branchless = real(ticks, kind=r64) / real(count_rate, kind=r64)

        ! Classic branchy binary search
        acc_branchy = 0_i64
        call system_clock(count_start)
        do q = 1_i64, queries_i64
            val = make_query(q, size_i64)
            acc_branchy = acc_branchy + binary_search_branchy(vec, val, size_i64)
        end do
        call system_clock(count_end)
        ticks = count_end - count_start
        if (count_max > 0 .and. ticks < 0) ticks = ticks + count_max
        elapsed_branchy = real(ticks, kind=r64) / real(count_rate, kind=r64)

        ! Linear search baseline
        acc_linear = 0_i64
        call system_clock(count_start)
        do q = 1_i64, queries_i64
            val = make_query(q, size_i64)
            acc_linear = acc_linear + linear_search_lb(vec, val, size_i64)
        end do
        call system_clock(count_end)
        ticks = count_end - count_start
        if (count_max > 0 .and. ticks < 0) ticks = ticks + count_max
        elapsed_linear = real(ticks, kind=r64) / real(count_rate, kind=r64)

        write(*, '(a,i0,a,i0,a)') 'size=', sizes(k), ', queries=', query_counts(k), ' -- timings (s):'
        write(*, '(a,f10.6,a,i0)') '  branchless: ', elapsed_branchless, '  checksum=', acc_branchless
        write(*, '(a,f10.6,a,i0)') '  branchy   : ', elapsed_branchy,   '  checksum=', acc_branchy
        write(*, '(a,f10.6,a,i0)') '  linear    : ', elapsed_linear,    '  checksum=', acc_linear
        write(*, '(a/)') ' '

        deallocate(vec)
    end do

contains

    pure function binary_search_branchy(vec, val, n) result(idx)
        real(r64), intent(in)    :: vec(n)
        real(r64), intent(in)    :: val
        integer(i64), intent(in) :: n
        integer(i64)             :: idx
        integer(i64) :: left, right, mid

        if (n <= 0_i64) then
            idx = 1_i64
            return
        end if

        left  = 1_i64
        right = n
        idx   = n + 1_i64
        do while (left <= right)
            mid = (left + right) / 2_i64
            if (vec(mid) < val) then
                left = mid + 1_i64
            else
                idx  = mid
                right = mid - 1_i64
            end if
        end do
    end function binary_search_branchy


    pure function linear_search_lb(vec, val, n) result(idx)
        real(r64), intent(in)    :: vec(n)
        real(r64), intent(in)    :: val
        integer(i64), intent(in) :: n
        integer(i64)             :: idx
        integer(i64) :: j

        if (n <= 0_i64) then
            idx = 1_i64
            return
        end if

        do j = 1_i64, n
            if (vec(j) >= val) then
                idx = j
                return
            end if
        end do
        idx = n + 1_i64
    end function linear_search_lb


    pure function make_query(iter, size) result(val)
        integer(i64), intent(in) :: iter
        integer(i64), intent(in) :: size
        real(r64) :: val
        integer(i64) :: pattern, pos

        if (size <= 0_i64) then
            val = 0.0_r64
            return
        end if

        pattern = modulo(iter, 4_i64)
        select case (pattern)
        case (0_i64)
            val = -1.0_r64
        case (1_i64)
            pos = modulo(iter * 37_i64 + 11_i64, size) + 1_i64
            val = real(pos, kind=r64) - 0.25_r64
        case (2_i64)
            pos = modulo(iter * 53_i64 + 7_i64, size) + 1_i64
            val = real(pos, kind=r64) + 0.10_r64
        case default
            val = real(size, kind=r64) + 1.5_r64
        end select
    end function make_query

end program bench_binary_search
