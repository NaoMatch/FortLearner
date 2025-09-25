module mod_check_finite
    use :: iso_fortran_env,       only: error_unit
    use :: mod_kinds,             only: r64, i64
    use :: mod_error_codes,       only: ERR_NON_FINITE
    use :: mod_error_manager,     only: raise_error
    use :: mod_character_helpers, only: make_prefix
    use :: mod_predicates,        only: is_finite, is_all_finite
    use, intrinsic :: ieee_arithmetic
    use :: mod_program_limits,    only: MAX_MSG_LEN
    implicit none

    interface check_finite
        module procedure check_finite_r64_scalar
        module procedure check_finite_r64_vec
        module procedure check_finite_r64_mat
    end interface check_finite

contains    

    subroutine check_finite_r64_scalar(a, file, class_name, value_name, fatal)
        implicit none
        real(r64),    intent(in) :: a
        character(*), intent(in) :: file
        character(*), intent(in) :: class_name
        character(*), intent(in) :: value_name

        logical :: is_nan, is_inf
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal


        if (.not. is_finite(a)) then
	        is_nan = ieee_is_nan(a)
	        is_inf = (.not.ieee_is_finite(a)) .and. (.not.ieee_is_nan(a))

            prefix = make_prefix(file, class_name, value_name)
            if (is_nan .and. is_inf) then
                write(tmp_msg,'(a," must be finite, but contains NaN and ±Inf.")') prefix
            elseif ( is_nan ) then
                write(tmp_msg,'(a," must be finite, but contains NaN.")')       prefix
            elseif ( is_inf ) then
                write(tmp_msg,'(a," must be finite, but contains ±Inf.")')      prefix
            end if
            msg = trim(tmp_msg)
            call raise_error(ERR_NON_FINITE, msg, do_fatal)
        end if
    end subroutine check_finite_r64_scalar

    subroutine check_finite_r64_vec(vec, file, class_name, value_name, fatal)
        implicit none
        real(r64),    intent(in) :: vec(:)
        character(*), intent(in) :: file
        character(*), intent(in) :: class_name
        character(*), intent(in) :: value_name

        logical :: has_nan, has_inf
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal


        if (.not. is_all_finite(vec)) then
	        has_nan = any(ieee_is_nan(vec))
	        has_inf = any((.not.ieee_is_finite(vec)) .and. (.not.ieee_is_nan(vec)))

            prefix = make_prefix(file, class_name, value_name)
            if (has_nan .and. has_inf) then
                write(tmp_msg,'(a," must be finite, but contains NaN and ±Inf.")') prefix
            elseif ( has_nan ) then
                write(tmp_msg,'(a," must be finite, but contains NaN.")')       prefix
            elseif ( has_inf ) then
                write(tmp_msg,'(a," must be finite, but contains ±Inf.")')      prefix
            end if
            msg = trim(tmp_msg)
            call raise_error(ERR_NON_FINITE, msg, do_fatal)
        end if
    end subroutine check_finite_r64_vec

    subroutine check_finite_r64_mat(mat, file, class_name, value_name, fatal)
        implicit none
        real(r64),    intent(in) :: mat(:,:)
        character(*), intent(in) :: file
        character(*), intent(in) :: class_name
        character(*), intent(in) :: value_name

        logical :: has_nan, has_inf
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal


        if (.not. is_all_finite(mat)) then
	        has_nan = any(ieee_is_nan(mat))
	        has_inf = any((.not.ieee_is_finite(mat)) .and. (.not.ieee_is_nan(mat)))

            prefix = make_prefix(file, class_name, value_name)
            if (has_nan .and. has_inf) then
                write(tmp_msg,'(a," must be finite, but contains NaN and ±Inf.")') prefix
            elseif ( has_nan ) then
                write(tmp_msg,'(a," must be finite, but contains NaN.")')       prefix
            elseif ( has_inf ) then
                write(tmp_msg,'(a," must be finite, but contains ±Inf.")')      prefix
            end if
            msg = trim(tmp_msg)
            call raise_error(ERR_NON_FINITE, msg, do_fatal)
        end if
    end subroutine check_finite_r64_mat

end module mod_check_finite
