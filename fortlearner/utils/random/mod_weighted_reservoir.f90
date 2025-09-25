module mod_weighted_reservoir
    use :: mod_kinds
    use :: mod_clip, only: clip_low
    use :: mod_predicates, only: is_zero
    implicit none
    

contains

    function weighted_reservoir_single(fitness, n_rows) result(idx)
        implicit none
        real(r64), intent(in) :: fitness(n_rows)
        integer(i64), intent(in) :: n_rows

        real(r64) :: key, best_key, u
        integer(i64) :: idx, i

        best_key = -huge(0.0_r64)
        idx = -1
        do i = 1, n_rows
            if (is_zero(fitness(i))) cycle
            call random_number(u)
            u = clip_low(u, min_val=epsilon(1.0_r64))
            key = log(u) / fitness(i)           ! w(i) > 0
            if (key > best_key) then
                best_key = key
                idx      = i
            end if
        end do
    end function weighted_reservoir_single

end module mod_weighted_reservoir
