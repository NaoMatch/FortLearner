module mod_check_array_shape
    use :: mod_kinds,             only: r64, i64
    use :: iso_fortran_env,       only: error_unit
    use :: mod_error_codes
    use :: mod_error_manager,     only: raise_error
    use :: mod_program_limits,    only: ATOL_R64, MAX_NAME_LEN, MAX_MSG_LEN
    use :: mod_character_helpers, only: make_prefix
    implicit none

    interface check_min_rows
        module procedure check_min_rows_r64
        module procedure check_min_rows_i64
    end interface ! check_min_rows

    interface check_min_cols
        module procedure check_min_cols_r64
        module procedure check_min_cols_i64
    end interface ! check_min_cols

    interface check_exact_shape
        module procedure check_exact_shape_r64
        module procedure check_exact_shape_i64
    end interface ! check_exact_shape

contains    

    subroutine check_min_rows_r64(a, min_rows, file, class_name, value_name, fatal)
        implicit none
        real(r64),    intent(in) :: a(:,:)
        integer(i64), intent(in) :: min_rows
        character(*), intent(in) :: file, class_name, value_name

        integer(i64) :: n_rows
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        n_rows = size(a, dim=1)

        if (n_rows < min_rows) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must have ≥ ",i0," rows, but got ",i0,".")') &
                trim(prefix), min_rows, n_rows
            msg = trim(tmp_msg)
            call raise_error(ERR_TOO_FEW_ROWS, msg, do_fatal)
        end if
    end subroutine check_min_rows_r64

    subroutine check_min_rows_i64(a, min_rows, file, class_name, value_name, fatal)
        implicit none
        integer(i64), intent(in) :: a(:,:)
        integer(i64), intent(in) :: min_rows
        character(*), intent(in) :: file, class_name, value_name

        integer(i64) :: n_rows
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        n_rows = size(a, dim=1)

        if (n_rows < min_rows) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must have ≥ ",i0," rows, but got ",i0,".")') &
                trim(prefix), min_rows, n_rows
            msg = trim(tmp_msg)
            call raise_error(ERR_TOO_FEW_ROWS, msg, do_fatal)
        end if
    end subroutine check_min_rows_i64


    !> 2-D 配列 a の列数が min_cols 未満なら停止  
    subroutine check_min_cols_r64(a, min_cols, file, class_name, value_name, fatal)
        implicit none
        real(r64),    intent(in) :: a(:,:)
        integer(i64), intent(in) :: min_cols
        character(*), intent(in) :: file, class_name, value_name

        integer(i64) :: n_cols
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        n_cols = size(a, dim=2)

        if (n_cols < min_cols) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must have ≥ ",i0," columns, but got ",i0,".")') &
                trim(prefix), min_cols, n_cols
            msg = trim(tmp_msg)
            call raise_error(ERR_TOO_FEW_COLS, msg, do_fatal)
        end if
    end subroutine check_min_cols_r64

    !> 2-D 配列 a の列数が min_cols 未満なら停止  
    subroutine check_min_cols_i64(a, min_cols, file, class_name, value_name, fatal)
        implicit none
        integer(i64), intent(in) :: a(:,:)
        integer(i64), intent(in) :: min_cols
        character(*), intent(in) :: file, class_name, value_name

        integer(i64) :: n_cols
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        n_cols = size(a, dim=2)

        if (n_cols < min_cols) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must have ≥ ",i0," columns, but got ",i0,".")') &
                trim(prefix), min_cols, n_cols
            msg = trim(tmp_msg)
            call raise_error(ERR_TOO_FEW_COLS, msg, do_fatal)
        end if
    end subroutine check_min_cols_i64

    subroutine check_exact_shape_r64(a, exp_n_rows, exp_n_cols, file, class_name, value_name, fatal)
        implicit none
        real(r64),    intent(in) :: a(:,:)
        integer(i64), intent(in) :: exp_n_rows, exp_n_cols
        character(*), intent(in) :: file, class_name, value_name

        integer(i64) :: n_rows, n_cols
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        n_rows = size(a, dim=1)
        n_cols = size(a, dim=2)

        if (n_rows /= exp_n_rows .or. n_cols /= exp_n_cols) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be (",i0,",",i0,") but got (",i0,",",i0,").")') &
                trim(prefix), exp_n_rows, exp_n_cols, n_rows, n_cols
            msg = trim(tmp_msg)
            call raise_error(ERR_SHAPE_MISMATCH, msg, do_fatal)
        end if
    end subroutine check_exact_shape_r64

    subroutine check_exact_shape_i64(a, exp_n_rows, exp_n_cols, file, class_name, value_name, fatal)
        implicit none
        integer(i64), intent(in) :: a(:,:)
        integer(i64), intent(in) :: exp_n_rows, exp_n_cols
        character(*), intent(in) :: file, class_name, value_name

        integer(i64) :: n_rows, n_cols
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        n_rows = size(a, dim=1)
        n_cols = size(a, dim=2)

        if (n_rows /= exp_n_rows .or. n_cols /= exp_n_cols) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be (",i0,",",i0,") but got (",i0,",",i0,").")') &
                trim(prefix), exp_n_rows, exp_n_cols, n_rows, n_cols
            msg = trim(tmp_msg)
            call raise_error(ERR_SHAPE_MISMATCH, msg, do_fatal)
        end if
    end subroutine check_exact_shape_i64
    
end module mod_check_array_shape
