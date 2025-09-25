module mod_bit_helpers
    use :: mod_kinds
    implicit none

contains

    pure function prev_pow2_i64(x) result(val)
        implicit none
        integer(i64), intent(in) :: x
        integer(i64) :: val
        integer(i64) :: tmp

        if (x <= 1_i64) then
            val = 0_i64
            return
        end if

        tmp = x - 1_i64

        tmp = ior(tmp, ishft(tmp, -1))
        tmp = ior(tmp, ishft(tmp, -2))
        tmp = ior(tmp, ishft(tmp, -4))
        tmp = ior(tmp, ishft(tmp, -8))
        tmp = ior(tmp, ishft(tmp, -16))
        tmp = ior(tmp, ishft(tmp, -32))

        val = ishft(tmp + 1_i64, -1)
    end function prev_pow2_i64

end module mod_bit_helpers
