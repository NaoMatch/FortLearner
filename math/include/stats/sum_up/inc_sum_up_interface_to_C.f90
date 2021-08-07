!> An interface to call extern sum_up_vector functions in c and inline assembler
Interface
    function sum_up_C_r8(x,n) Bind(C,Name='sum_up_C_r8')
        Import
        Real(c_double) :: sum_up_C_r8
        Integer(c_int64_t),Value :: n
        Real(c_double),Intent(In) :: x(n)
    end function    

    function sum_up_C_i8(x,n) Bind(C,Name='sum_up_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_C_i8
        Integer(c_int64_t),Value   :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end function        

    function sum_up_02_C_r8(x,n) Bind(C,Name='sum_up_02_C_r8')
        Import
        Real(c_double) :: sum_up_02_C_r8
        Integer(c_int64_t),Value :: n
        Real(c_double),Intent(In) :: x(n)
    end function    

    function sum_up_02_C_i8(x,n) Bind(C,Name='sum_up_02_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_02_C_i8
        Integer(c_int64_t),Value   :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end function        

    function sum_up_04_C_r8(x,n) Bind(C,Name='sum_up_04_C_r8')
        Import
        Real(c_double) :: sum_up_04_C_r8
        Integer(c_int64_t),Value :: n
        Real(c_double),Intent(In) :: x(n)
    end function    

    function sum_up_04_C_i8(x,n) Bind(C,Name='sum_up_04_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_04_C_i8
        Integer(c_int64_t),Value   :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end function        

    function sum_up_08_C_r8(x,n) Bind(C,Name='sum_up_08_C_r8')
        Import
        Real(c_double) :: sum_up_08_C_r8
        Integer(c_int64_t),Value :: n
        Real(c_double),Intent(In) :: x(n)
    end function    

    function sum_up_08_C_i8(x,n) Bind(C,Name='sum_up_08_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_08_C_i8
        Integer(c_int64_t),Value   :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end function        

    function sum_up_16_C_r8(x,n) Bind(C,Name='sum_up_16_C_r8')
        Import
        Real(c_double) :: sum_up_16_C_r8
        Integer(c_int64_t),Value :: n
        Real(c_double),Intent(In) :: x(n)
    end function    

    function sum_up_16_C_i8(x,n) Bind(C,Name='sum_up_16_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_16_C_i8
        Integer(c_int64_t),Value   :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end function        

    function sum_up_32_C_r8(x,n) Bind(C,Name='sum_up_32_C_r8')
        Import
        Real(c_double) :: sum_up_32_C_r8
        Integer(c_int64_t),Value :: n
        Real(c_double),Intent(In) :: x(n)
    end function    

    function sum_up_32_C_i8(x,n) Bind(C,Name='sum_up_32_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_32_C_i8
        Integer(c_int64_t),Value   :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end function        

    function sum_up_04_ASM_r8(x,n) Bind(C,Name='sum_up_04_ASM_r8')
        Import
        Real(c_double) :: sum_up_04_ASM_r8
        Integer(c_int64_t),Value :: n
        Real(c_double),Intent(In) :: x(n)
    end function    

    function sum_up_04_ASM_i8(x,n) Bind(C,Name='sum_up_04_ASM_i8')
        Import
        Integer(c_int64_t)            :: sum_up_04_ASM_i8
        Integer(c_int64_t),Value   :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end function        

    function sum_up_08_ASM_r8(x,n) Bind(C,Name='sum_up_08_ASM_r8')
        Import
        Real(c_double) :: sum_up_08_ASM_r8
        Integer(c_int64_t),Value :: n
        Real(c_double),Intent(In) :: x(n)
    end function    

    function sum_up_08_ASM_i8(x,n) Bind(C,Name='sum_up_08_ASM_i8')
        Import
        Integer(c_int64_t)            :: sum_up_08_ASM_i8
        Integer(c_int64_t),Value   :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end function        

    function sum_up_16_ASM_r8(x,n) Bind(C,Name='sum_up_16_ASM_r8')
        Import
        Real(c_double) :: sum_up_16_ASM_r8
        Integer(c_int64_t),Value :: n
        Real(c_double),Intent(In) :: x(n)
    end function    

    function sum_up_16_ASM_i8(x,n) Bind(C,Name='sum_up_16_ASM_i8')
        Import
        Integer(c_int64_t)            :: sum_up_16_ASM_i8
        Integer(c_int64_t),Value   :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end function        

    function sum_up_32_ASM_r8(x,n) Bind(C,Name='sum_up_32_ASM_r8')
        Import
        Real(c_double) :: sum_up_32_ASM_r8
        Integer(c_int64_t),Value :: n
        Real(c_double),Intent(In) :: x(n)
    end function    

    function sum_up_32_ASM_i8(x,n) Bind(C,Name='sum_up_32_ASM_i8')
        Import
        Integer(c_int64_t)            :: sum_up_32_ASM_i8
        Integer(c_int64_t),Value   :: n
        Integer(c_int64_t),Intent(In) :: x(n)
    end function        
end interface
