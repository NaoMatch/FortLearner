module mod_is_close
    use :: mod_kinds
    use :: mod_program_limits, only : ATOL_R64, RTOL_R64
    implicit none

contains

    !------------------------------------------------------------------
    pure elemental function is_close(x, y, rtol, atol) result(ok)
        real(r64), intent(in) :: x, y
        real(r64), intent(in), optional :: rtol, atol
        real(r64) :: rtol_, atol_
        logical   :: ok

        ! ---- 安全なデフォルト設定 -------------------------------
        rtol_ = RTOL_R64
        if (present(rtol)) rtol_ = rtol

        atol_ = ATOL_R64
        if (present(atol)) atol_ = atol

        ! ---- 判定式 ---------------------------------------------
        ok = abs(x - y) <= (atol_ + rtol_ * max(abs(x), abs(y)))
    end function is_close

    !------------------------------------------------------------------
    pure elemental function is_zero(x, atol) result(ok)
        real(r64), intent(in) :: x
        real(r64), intent(in), optional :: atol
        logical :: ok
        real(r64) :: atol_
        ! rtol = 0 で十分。atol はゼロ判定専用のやや厳しい既定。
        atol_ = 1.0e-12_r64
        if (present(atol)) atol_ = atol
        ok = is_close(x, 0.0_r64, rtol = 0.0_r64, atol = atol_)
    end function is_zero
    
    !------------------------------------------------------------------
    pure elemental function is_one(x, rtol, atol) result(ok)
        real(r64), intent(in) :: x
        real(r64), intent(in), optional :: rtol, atol
        logical :: ok
        ok = is_close(x, 1.0_r64, rtol, atol)
    end function is_one

end module mod_is_close
