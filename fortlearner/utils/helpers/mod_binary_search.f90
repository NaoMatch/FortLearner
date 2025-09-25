module mod_binary_search
    use :: mod_kinds
    use :: mod_bit_helpers, only : prev_pow2_i64
    implicit none

contains

    pure function binary_search_r64(vec, val, n) result(idx)
        implicit none
        real(r64), intent(in)    :: vec(n)
        real(r64), intent(in)    :: val
        integer(i64), intent(in) :: n
        integer(i64)             :: idx

        integer(i64) :: step
        integer(i64) :: candidate
        logical      :: take_candidate

        if (n <= 0_i64) then
            idx = 1_i64
            return
        end if

        idx  = 0_i64
        step = prev_pow2_i64(n)
        if (step == 0_i64) step = 1_i64

        do while (step > 0_i64)
            candidate      = min(idx + step, n)
            take_candidate = vec(candidate) < val
            idx            = merge(candidate, idx, take_candidate)
            step           = ishft(step, -1)
        end do

        idx = idx + 1_i64
    end function binary_search_r64

end module mod_binary_search
