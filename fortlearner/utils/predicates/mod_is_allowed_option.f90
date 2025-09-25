module mod_is_allowed_option
    use :: mod_kinds
    implicit none

    !-------------------- generic インターフェース ----------------------------
    interface is_allowed_option
        module procedure is_allowed_option_i64
        module procedure is_allowed_option_character
    end interface is_allowed_option

contains

    !======================================================================
    pure function is_allowed_option_i64(target_value, allowed_values) result(is_allowed)
        integer(i64), intent(in) :: target_value
        integer(i64), intent(in) :: allowed_values(:)
        logical                  :: is_allowed

        is_allowed = any(allowed_values == target_value)
    end function is_allowed_option_i64

    !======================================================================
    pure function is_allowed_option_character(target_value, allowed_values) result(is_allowed)
        character(*), intent(in) :: target_value
        character(*), intent(in) :: allowed_values(:)
        character(len=:), allocatable :: tv_trim
        logical :: is_allowed
        integer :: i

        tv_trim = trim(target_value)

        is_allowed = .false.
        do i = 1, size(allowed_values)
            if (trim(allowed_values(i)) == tv_trim) then
                is_allowed = .true.
                exit
            end if
        end do
    end function is_allowed_option_character

end module mod_is_allowed_option