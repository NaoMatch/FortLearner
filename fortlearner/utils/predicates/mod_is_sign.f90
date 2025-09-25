!>= 符号判定 predicate 群（REAL(r64) / INTEGER(i64)）
module mod_is_sign
    use :: mod_kinds
    use, intrinsic :: ieee_arithmetic                ! ieee_is_finite
    use :: mod_program_limits, only : ATOL_R64       ! = 1e-12〜1e-10 推奨
    implicit none

    interface is_positive
        module procedure is_positive_i64
        module procedure is_positive_r64
    end interface is_positive

    interface is_negative
        module procedure is_negative_i64
        module procedure is_negative_r64        
    end interface is_negative

    interface is_non_positive
        module procedure is_non_positive_i64
        module procedure is_non_positive_r64
    end interface is_non_positive

    interface is_non_negative
        module procedure is_non_negative_i64
        module procedure is_non_negative_r64        
    end interface is_non_negative

contains
    pure elemental function is_positive_r64(x) result(ok)
        real(r64), intent(in) :: x
        logical               :: ok
        ok = ieee_is_finite(x) .and. (x >  ATOL_R64)
    end function is_positive_r64

    pure elemental function is_positive_i64(x) result(ok)
        integer(i64), intent(in) :: x
        logical                  :: ok
        ok = (x > 0_i64)
    end function is_positive_i64




    pure elemental function is_non_positive_r64(x) result(ok)
        real(r64), intent(in) :: x
        logical               :: ok
        ok = .not. is_positive_r64(x)
    end function is_non_positive_r64

    pure elemental function is_non_positive_i64(x) result(ok)
        integer(i64), intent(in) :: x
        logical                  :: ok
        ok = .not. is_positive_i64(x)
    end function is_non_positive_i64



    pure elemental function is_negative_r64(x) result(ok)
        real(r64), intent(in) :: x
        logical               :: ok
        ok = ieee_is_finite(x) .and. (x < -ATOL_R64)
    end function is_negative_r64

    pure elemental function is_negative_i64(x) result(ok)
        integer(i64), intent(in) :: x
        logical                  :: ok
        ok = (x < 0_i64)
    end function is_negative_i64




    pure elemental function is_non_negative_r64(x) result(ok)
        real(r64), intent(in) :: x
        logical               :: ok
        ok = .not. is_negative_r64(x)
    end function is_non_negative_r64

    pure elemental function is_non_negative_i64(x) result(ok)
        integer(i64), intent(in) :: x
        logical                  :: ok
        ok = .not. is_negative_i64(x)
    end function is_non_negative_i64

end module mod_is_sign
