module mod_random_int
    use :: mod_kinds
    implicit none
    
contains

    function random_int_single(lo, hi) result(rnd_int)
        implicit none
        integer(i64), intent(in) :: lo, hi
        real(r64) :: tmp
        integer(i64) :: rnd_int
        integer(i64) :: factor

        call random_number(tmp)
        factor = hi - lo + 1
        rnd_int = int(factor * tmp, kind=i64) + lo
    end function random_int_single

end module mod_random_int
