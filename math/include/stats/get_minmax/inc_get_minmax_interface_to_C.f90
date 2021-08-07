!> An interface to call extern minmax functions in c and inline assembler
Interface
    subroutine get_minmax_c_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_c_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_c_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_c_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_02_c_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_02_c_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_02_c_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_02_c_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_04_c_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_04_c_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_04_c_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_04_c_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_08_c_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_08_c_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_08_c_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_08_c_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_16_c_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_16_c_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_16_c_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_16_c_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_a_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_a_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_a_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_a_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_02_a_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_02_a_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_02_a_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_02_a_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_04_a_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_04_a_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_04_a_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_04_a_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_08_a_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_08_a_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_08_a_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_08_a_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_08z_a_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_08z_a_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_08z_a_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_08z_a_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_16_a_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_16_a_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_16_a_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_16_a_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_16z_a_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_16z_a_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_16z_a_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_16z_a_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_32_a_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_32_a_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_32_a_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_32_a_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_64_a_r8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_64_a_r8')
        Import
        Real(c_double)            :: x_min, x_max
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n)
    end subroutine    

    subroutine get_minmax_unroll_64_a_i8(x_min, x_max, x, n) Bind(C,Name='get_minmax_unroll_64_a_i8')
        Import
        Integer(c_int64_t)            :: x_min, x_max
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end subroutine    
end interface
