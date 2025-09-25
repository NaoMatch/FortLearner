module mod_error_manager
    use iso_c_binding, only: c_long, c_char
    use mod_kinds,     only: i64
    use iso_fortran_env, only: error_unit
    implicit none

    integer(i64), save :: last_error_code = 0

    ! Fortranログ用
    character(len=512), save :: last_error_msg_internal = ''

    ! Cバインディング用（null終端文字列）
    character(kind=c_char), dimension(512), save :: last_error_msg_c

contains

    !=====================
    subroutine raise_error(code, msg, fatal)
        integer(i64),    intent(in) :: code
        character(*),    intent(in) :: msg
        logical,         intent(in) :: fatal

        last_error_code = code
        call set_last_error_msg(msg)

        if (fatal) then
            write(error_unit, '(a)') msg
            flush(error_unit)
            error stop code
        end if
    end subroutine raise_error

    !=====================
    subroutine set_last_error_msg(msg)
        character(*), intent(in) :: msg
        integer :: i, len_msg

        last_error_msg_internal = msg(1:min(len(msg), len(last_error_msg_internal)))

        len_msg = len_trim(msg)

        ! 初期化（achar(0) → c_char に強制変換）
        last_error_msg_c(:) = transfer(achar(0), last_error_msg_c(1))

        ! コピー（最大長 -1 まで）
        do i = 1, min(len_msg, size(last_error_msg_c) - 1)
            last_error_msg_c(i) = transfer(msg(i:i), last_error_msg_c(1))
        end do

        ! 明示的 null 終端（安全性のため）
        last_error_msg_c(min(len_msg + 1, size(last_error_msg_c))) = transfer(achar(0), last_error_msg_c(1))
    end subroutine set_last_error_msg

end module mod_error_manager
