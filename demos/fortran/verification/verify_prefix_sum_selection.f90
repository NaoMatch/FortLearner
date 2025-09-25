! Verify prefix-sum based weighted sampling by comparing empirical frequencies
! with expected probabilities across representative weight patterns.
program verify_prefix_sum_selection
    use :: mod_kinds
    use :: mod_prefix_sum
    use :: mod_prefix_sum_selection, only : prefix_sum_selection_r64
    implicit none

    integer, parameter :: NUM_SAMPLES = 200000
    real(r64), parameter :: TOLERANCE = 0.02_r64

    call init_seed()

    call run_case('uniform weights',            [1.0_r64, 1.0_r64, 1.0_r64, 1.0_r64, 1.0_r64])
    call run_case('single heavy weight',        [1.0_r64, 1.0_r64, 6.0_r64, 1.0_r64, 1.0_r64])
    call run_case('increasing linear weights',  [1.0_r64, 2.0_r64, 3.0_r64, 4.0_r64, 5.0_r64])

contains

    subroutine init_seed()
        integer :: sz, i
        integer, allocatable :: seed(:)

        call random_seed(size=sz)
        allocate(seed(sz))
        do i = 1, sz
            seed(i) = 12345 + 37 * (i - 1)
        end do
        call random_seed(put=seed)
        deallocate(seed)
    end subroutine init_seed


    subroutine run_case(case_name, weights)
        character(*), intent(in) :: case_name
        real(r64), intent(in)    :: weights(:)

        integer :: n, i
        integer(i64) :: draws
        integer(i64) :: idx64
        integer :: idx
        real(r64) :: total_weight
        real(r64) :: expected, observed, diff
        real(r64) :: max_diff
        real(r64), allocatable :: prefix(:)
        real(r64), allocatable :: probs(:)
        integer(i64), allocatable :: counts(:)

        n = size(weights)
        if (n <= 0) then
            write(*,'(a)') 'skipping case: zero-length weights'
            return
        end if

        allocate(prefix(n), probs(n), counts(n))
        call prefix_sum_r64(weights, prefix, int(n, kind=i64))
        total_weight = prefix(n)
        if (total_weight <= 0.0_r64) then
            write(*,'(a)') 'skipping case: non-positive total weight'
            deallocate(prefix, probs, counts)
            return
        end if

        do i = 1, n
            probs(i) = weights(i) / total_weight
        end do
        counts = 0_i64

        do draws = 1_i64, int(NUM_SAMPLES, kind=i64)
            idx64 = prefix_sum_selection_r64(prefix, int(n, kind=i64))
            idx = max(1, min(int(idx64), n))
            counts(idx) = counts(idx) + 1_i64
        end do

        write(*,'(/a)') repeat('=', 60)
        write(*,'(a)') 'Case: ' // trim(case_name)
        write(*,'(a)') '  samples: ' // trim(adjustl(itoa(NUM_SAMPLES))) // '  tolerance: ' // trim(ftoa(TOLERANCE))
        write(*,'(a)') '    idx    expected     observed     abs diff'

        max_diff = 0.0_r64
        do i = 1, n
            observed = real(counts(i), kind=r64) / real(NUM_SAMPLES, kind=r64)
            diff = abs(observed - probs(i))
            max_diff = max(max_diff, diff)
            write(*,'(i7,3f12.6)') i, probs(i), observed, diff
        end do

        if (max_diff <= TOLERANCE) then
            write(*,'(a,f8.5,a)') '  -> PASS (max diff=', max_diff, ')'
        else
            write(*,'(a,f8.5,a)') '  -> WARN (max diff=', max_diff, ')'
        end if

        deallocate(prefix, probs, counts)
    end subroutine run_case


    pure function itoa(val) result(out)
        integer, intent(in) :: val
        character(:), allocatable :: out
        character(len=32) :: buf
        write(buf,'(i0)') val
        out = trim(buf)
    end function itoa

    pure function ftoa(val) result(out)
        real(r64), intent(in) :: val
        character(:), allocatable :: out
        character(len=32) :: buf
        write(buf,'(f8.5)') val
        out = adjustl(trim(buf))
    end function ftoa

end program verify_prefix_sum_selection
