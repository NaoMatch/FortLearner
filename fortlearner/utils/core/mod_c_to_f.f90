module mod_c_to_f
    use :: iso_c_binding
    implicit none
    

contains

    pure function c_str_to_f(cstr) result(f)
        use iso_c_binding, only: c_char, c_null_char
        implicit none
        character(kind=c_char), intent(in) :: cstr(*)   ! Cのchar*（assumed-size）
        character(:), allocatable          :: f
        integer :: n, i

        ! 1) 長さ（null 終端まで）を数える
        n = 0
        i = 1
        do
            if (cstr(i) == c_null_char) exit
            n = n + 1
            i = i + 1
        end do

        ! 2) Fortran文字列にコピー
        allocate(character(len=n) :: f)
        do i = 1, n
            f(i:i) = transfer(cstr(i), 'a')  !! c_char → default character
        end do
    end function c_str_to_f

end module mod_c_to_f
