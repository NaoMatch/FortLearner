module mod_clip
    use :: mod_kinds
    implicit none

contains
    pure elemental function clip_both(x, min_val, max_val) result(y)
        real(r64), intent(in) :: x, min_val, max_val
        real(r64)             :: y
        y = max(min_val, min(max_val, x))
    end function clip_both

    pure elemental function clip_low(x, min_val) result(y)
        real(r64), intent(in) :: x, min_val
        real(r64)             :: y
        y = max(min_val, x)
    end function clip_low

    pure elemental function clip_high(x, max_val) result(y)
        real(r64), intent(in) :: x, max_val
        real(r64)             :: y
        y = min(max_val, x)
    end function clip_high
end module mod_clip