Interface
function variance_C_r8(x, n) Bind(C,Name='variance_C_r8')
    Import
    Real(c_double) :: variance_C_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_C_i8(x, n) Bind(C,Name='variance_C_i8')
    Import
    Integer(c_int64_t)            :: variance_C_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_02_C_r8(x, n) Bind(C,Name='variance_02_C_r8')
    Import
    Real(c_double) :: variance_02_C_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_02_C_i8(x, n) Bind(C,Name='variance_02_C_i8')
    Import
    Integer(c_int64_t)            :: variance_02_C_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_04_C_r8(x, n) Bind(C,Name='variance_04_C_r8')
    Import
    Real(c_double) :: variance_04_C_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_04_C_i8(x, n) Bind(C,Name='variance_04_C_i8')
    Import
    Integer(c_int64_t)            :: variance_04_C_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_08_C_r8(x, n) Bind(C,Name='variance_08_C_r8')
    Import
    Real(c_double) :: variance_08_C_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_08_C_i8(x, n) Bind(C,Name='variance_08_C_i8')
    Import
    Integer(c_int64_t)            :: variance_08_C_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_16_C_r8(x, n) Bind(C,Name='variance_16_C_r8')
    Import
    Real(c_double) :: variance_16_C_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_16_C_i8(x, n) Bind(C,Name='variance_16_C_i8')
    Import
    Integer(c_int64_t)            :: variance_16_C_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_32_C_r8(x, n) Bind(C,Name='variance_32_C_r8')
    Import
    Real(c_double) :: variance_32_C_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_32_C_i8(x, n) Bind(C,Name='variance_32_C_i8')
    Import
    Integer(c_int64_t)            :: variance_32_C_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_02_A_r8(x, n) Bind(C,Name='variance_02_A_r8')
    Import
    Real(c_double) :: variance_02_A_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_02_A_i8(x, n) Bind(C,Name='variance_02_A_i8')
    Import
    Integer(c_int64_t)            :: variance_02_A_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_04_A_r8(x, n) Bind(C,Name='variance_04_A_r8')
    Import
    Real(c_double) :: variance_04_A_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_04_A_i8(x, n) Bind(C,Name='variance_04_A_i8')
    Import
    Integer(c_int64_t)            :: variance_04_A_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_08_A_r8(x, n) Bind(C,Name='variance_08_A_r8')
    Import
    Real(c_double) :: variance_08_A_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_08_A_i8(x, n) Bind(C,Name='variance_08_A_i8')
    Import
    Integer(c_int64_t)            :: variance_08_A_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_08z_A_r8(x, n) Bind(C,Name='variance_08z_A_r8')
    Import
    Real(c_double) :: variance_08z_A_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_08z_A_i8(x, n) Bind(C,Name='variance_08z_A_i8')
    Import
    Integer(c_int64_t)            :: variance_08z_A_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_16_A_r8(x, n) Bind(C,Name='variance_16_A_r8')
    Import
    Real(c_double) :: variance_16_A_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_16_A_i8(x, n) Bind(C,Name='variance_16_A_i8')
    Import
    Integer(c_int64_t)            :: variance_16_A_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_16z_A_r8(x, n) Bind(C,Name='variance_16z_A_r8')
    Import
    Real(c_double) :: variance_16z_A_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_16z_A_i8(x, n) Bind(C,Name='variance_16z_A_i8')
    Import
    Integer(c_int64_t)            :: variance_16z_A_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        

function variance_32_A_r8(x, n) Bind(C,Name='variance_32_A_r8')
    Import
    Real(c_double) :: variance_32_A_r8
    Integer(c_int64_t),Value :: n
    Real(c_double),Intent(In) :: x(n)
end function    

function variance_32_A_i8(x, n) Bind(C,Name='variance_32_A_i8')
    Import
    Integer(c_int64_t)            :: sum_up_32_A_i8
    Integer(c_int64_t),Value   :: n
    Integer(c_int64_t),Intent(In) :: x(n)
end function        
end interface
