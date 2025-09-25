module mod_dump_load
    use :: mod_kinds
    use :: mod_program_limits
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    

contains
    !------------------ r64 ----------------------------------
    subroutine dump_r64(unit, x)
        integer(i64),           intent(in) :: unit
        real(r64),      intent(in) :: x
        write(unit) x
    end subroutine

    subroutine load_r64(unit, x)
        integer(i64),           intent(in) :: unit
        real(r64),      intent(out):: x
        read(unit) x
    end subroutine

    !------------------ i64 -----------------------------------
    subroutine dump_i64(unit, x)
        integer(i64),           intent(in) :: unit
        integer(i64),    intent(in) :: x
        write(unit) x
    end subroutine

    subroutine load_i64(unit, x)
        integer(i64),           intent(in) :: unit
        integer(i64),    intent(out):: x
        read(unit) x
    end subroutine

    !------------------ logical -----------------------------------
    subroutine dump_logical(unit, x)
        integer(i64),           intent(in) :: unit
        logical,    intent(in) :: x
        write(unit) x
    end subroutine

    subroutine load_logical(unit, x)
        integer(i64),           intent(in) :: unit
        logical,    intent(out):: x
        read(unit) x
    end subroutine

    !============================================================
    ! 1. 固定長文字列（単一）
    !============================================================
    subroutine dump_char_fixed_scalar(unit, s)
        integer(i64),                intent(in) :: unit
        character(len=MAX_NAME_LEN),   intent(in) :: s
        write(unit) s                  ! メタなし・そのまま出力
    end subroutine
    !---------------------------------------------------------
    subroutine load_char_fixed_scalar(unit, s)
        integer(i64),                intent(in)  :: unit
        character(len=MAX_NAME_LEN),   intent(out) :: s
        read(unit) s                   ! そのまま読み込み
        s = trim(s)                    ! 右側スペース除去（必要なら）
    end subroutine
    
    !============================================================
    ! 2. 固定長文字列 × 可変長配列
    !============================================================
    subroutine dump_char_fixed_vec(unit, arr)
        integer(i64),                intent(in) :: unit
        character(len=MAX_NAME_LEN),   intent(in) :: arr(:)
        integer(i32) :: n
        n = size(arr)
        write(unit) n                  ! 要素数メタ
        write(unit) arr                ! 固定長 × n
    end subroutine
    !---------------------------------------------------------
    subroutine load_char_fixed_vec(unit, arr)
        integer(i64),                intent(in)  :: unit
        character(len=MAX_NAME_LEN),   intent(out) :: arr(:)
        integer(i32) :: n
        read(unit) n
        if (n /= size(arr)) error stop "Size mismatch in load_char_fixed_vec"
        read(unit) arr
    end subroutine

    !------------------------- 1D 配列 -------------------------
    subroutine dump_vec(unit, v)
        integer(i64), intent(in)           :: unit
        real(r64), intent(in)      :: v(:)
        integer(i64) :: n
        n = size(v)
        write(unit) n                 ! 要素数
        write(unit) v                 ! データ
    end subroutine

    subroutine load_vec(unit, v)
        integer(i64), intent(in)                 :: unit
        real(r64), allocatable, intent(out):: v(:)
        integer(i64) :: n
        read(unit) n
        allocate(v(n))
        read(unit) v
    end subroutine

    !------------------------- 2D 配列 -------------------------
    subroutine dump_mat(unit, a)
        integer(i64), intent(in)           :: unit
        real(r64), intent(in)      :: a(:,:)
        integer(i64) :: nrow, ncol
        nrow = size(a,1);  ncol = size(a,2)
        write(unit) nrow, ncol       ! 行数・列数
        write(unit) a                ! データ (列優先)
    end subroutine

    subroutine load_mat(unit, a)
        integer(i64), intent(in)                 :: unit
        real(r64), allocatable, intent(out):: a(:,:)
        integer(i64) :: nrow, ncol
        read(unit) nrow, ncol
        allocate(a(nrow,ncol))
        read(unit) a
    end subroutine
end module mod_dump_load
