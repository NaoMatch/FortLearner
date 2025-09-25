module mod_is_finite
    use :: mod_kinds
    use, intrinsic :: ieee_arithmetic
    implicit none

    interface is_all_finite
        module procedure :: is_all_finite_vec
        module procedure :: is_all_finite_mat
    end interface ! is_all_finite

contains

    pure elemental function is_finite(x) result(ok)
        real(r64), intent(in) :: x
        logical               :: ok
        ok = ieee_is_finite(x)
    end function is_finite

    function is_all_finite_vec(vec) result(ok)
        real(r64), intent(in) :: vec(:)
        logical               :: ok
        ok = all(is_finite(vec))
    end function is_all_finite_vec
    
    function is_all_finite_mat(mat) result(ok)
        real(r64), intent(in) :: mat(:,:)
        logical               :: ok
        ok = all(is_finite(mat))
    end function is_all_finite_mat
    
end module mod_is_finite