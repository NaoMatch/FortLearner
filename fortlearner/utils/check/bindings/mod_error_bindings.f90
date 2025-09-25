module mod_error_bindings   !! ← 小さな補助モジュール
    use iso_c_binding
    use mod_error_manager, only : last_error_code, last_error_msg_c, last_error_msg_internal
    implicit none
contains

    !=====================
    function c_strlen(str) result(length)
        character(kind=c_char), intent(in) :: str(:)
        integer :: length, i

        length = 0
        do i = 1, size(str)
            if (str(i) == transfer(achar(0), str(1))) exit
            length = length + 1
        end do
    end function c_strlen

    subroutine get_last_error(code, msg) bind(C, name="get_last_error")
        integer(c_long), intent(out) :: code
        character(kind=c_char), intent(out) :: msg(*)

        integer :: i, len_c

        code = int(last_error_code, kind=c_long)

        len_c = c_strlen(last_error_msg_c)

        do i = 1, len_c
            msg(i) = last_error_msg_c(i)
        end do

        ! null terminate
        msg(len_c + 1) = transfer(achar(0), msg(1))
    end subroutine get_last_error

    subroutine clear_last_error() bind(C, name="clear_last_error")
        use iso_c_binding, only: c_char
        implicit none
        integer :: i

        last_error_code = 0
        last_error_msg_internal = ''

        ! C文字列バッファを null クリア
        do i = 1, size(last_error_msg_c)
            last_error_msg_c(i) = transfer(achar(0), last_error_msg_c(1))
        end do
    end subroutine clear_last_error

end module