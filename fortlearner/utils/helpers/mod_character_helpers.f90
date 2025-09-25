module mod_character_helpers
    use :: mod_kinds
    implicit none
        
    public :: bound_str, make_prefix

    interface bound_str
        module procedure bound_str_i64
        module procedure bound_str_r64
    end interface

contains

    pure function bound_str_i64(bnd, is_low) result(s)
        integer(i64), intent(in) :: bnd
        logical,       intent(in) :: is_low
        character(:), allocatable :: s
        character(len=32) :: buf
        if ((is_low .and. bnd == -huge(0_i64)) .or.  &
            (.not.is_low .and. bnd == huge(0_i64))) then
            s = merge('-inf', '+inf', is_low)
        else
            write(buf,'(i0)') bnd
            s = trim(buf)
        end if
    end function

    pure function bound_str_r64(bnd, is_low) result(s)
        real(r64),  intent(in) :: bnd
        logical,       intent(in) :: is_low
        character(:), allocatable :: s
        character(len=32) :: buf
        if ((is_low .and. bnd == -huge(0.0_r64)) .or. &
            (.not.is_low .and. bnd == huge(0.0_r64))) then
            s = merge('-inf', '+inf', is_low)
        else
            write(buf,'(es14.7)') bnd
            s = adjustl(trim(buf))
        end if
    end function

    function make_prefix(file, class, name, sep, quote) result(pref)
        implicit none
        character(*), intent(in), optional :: file, class, name
        character(*), intent(in), optional :: sep
        logical,       intent(in), optional :: quote
        character(:), allocatable           :: pref
        character(:), allocatable           :: sep_

        logical :: quote_
        sep_   = '';      if (present(sep))   sep_   = sep
        quote_ = .false.;    if (present(quote)) quote_ = quote

        pref = ''
        if (present(file))  pref = trim(file)//': '
        if (present(class)) pref = pref//'['//trim(class)//'] '

        if (present(name)) then
            if (quote_) then
                pref = pref//"' "//trim(name)//"'"   ! ← ここを分岐で処理
            else
                pref = pref//trim(name)
            end if
            pref = pref//sep_                      ! sep_ に前後スペースを含める
        end if
    end function make_prefix

    function join_char_array(list, sep) result(out)
        implicit none
        character(*), intent(in) :: list(:)
        character(*), intent(in) :: sep
        character(:), allocatable :: out
        integer :: i

        out = trim(list(1))
        do i = 2, size(list)
            out = out // sep // trim(list(i))
        end do
    end function join_char_array

end module mod_character_helpers
