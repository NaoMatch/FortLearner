module mod_warn_in_range
    use :: mod_kinds,             only: i64
    use :: iso_fortran_env,       only: error_unit
    use :: mod_character_helpers, only: bound_str, make_prefix
    use :: mod_config,            only: ENABLE_WARNING
    implicit none


contains

    subroutine warn_in_range_i64(a, lo, hi, file, class_name, value_name)
        implicit none
        integer(i64), intent(in)            :: a
        integer(i64), intent(in), optional  :: lo, hi
        character(*), intent(in)            :: file
        character(*), intent(in)            :: class_name
        character(*), intent(in)            :: value_name

        integer(i64) :: lo_, hi_
        character(:), allocatable :: prefix, lo_str, hi_str

        if (.not. ENABLE_WARNING) return

        ! ---- 既定範囲 ----
        lo_ = -huge(0_i64); if (present(lo)) lo_ = lo
        hi_ =  huge(0_i64); if (present(hi)) hi_ = hi


        if (a < lo_ .or. a > hi_) then
            lo_str   = bound_str(lo_, .true.)
            hi_str   = bound_str(hi_, .false.)
            prefix = make_prefix(file, class_name, value_name)
            write(error_unit,'(a,i0," is outside range [",a,", ",a,"].")')  &
                prefix, a, lo_str, hi_str
        end if
    end subroutine warn_in_range_i64

    subroutine warn_ge_i64(a, lo, file, class_name, value_name)
        implicit none
        integer(i64), intent(in) :: a
        integer(i64), intent(in) :: lo
        character(*), intent(in) :: file
        character(*), intent(in) :: class_name
        character(*), intent(in) :: value_name
        call warn_in_range_i64(a, lo=lo, file=file, class_name=class_name, value_name=value_name)
    end subroutine

    subroutine warn_le_i64(a, hi, file, class_name, value_name)
        implicit none
        integer(i64), intent(in) :: a
        integer(i64), intent(in) :: hi
        character(*), intent(in) :: file
        character(*), intent(in) :: class_name
        character(*), intent(in) :: value_name
        call warn_in_range_i64(a, hi=hi, file=file, class_name=class_name, value_name=value_name)
    end subroutine

end module mod_warn_in_range
