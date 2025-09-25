module mod_is_in_range
    use :: mod_kinds
    implicit none

    interface is_in_range
        module procedure is_in_range_i64
        module procedure is_in_range_r64
    end interface is_in_range
    
contains

    pure elemental logical function is_in_range_r64(x, lo, hi) result(ok)
        real(r64), intent(in) :: x, lo, hi
        ok = (x >= lo) .and. (x <= hi)
    end function  

    pure elemental logical function is_in_range_i64(x, lo, hi) result(ok)
        integer(i64), intent(in) :: x, lo, hi
        ok = (x >= lo) .and. (x <= hi)
    end function
    
end module mod_is_in_range
