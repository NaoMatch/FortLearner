!> An interface to call extern sum_up_gt functions in c and inline assembler
Interface
    function sum_up_gt_loop_C_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_C_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_C_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_C_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_C_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_02_C_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_02_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_02_C_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_02_C_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_02_C_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_02_C_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_04_C_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_04_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_04_C_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_04_C_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_04_C_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_04_C_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_08_C_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_08_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_08_C_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_08_C_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_08_C_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_08_C_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_16_C_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_16_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_16_C_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_16_C_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_16_C_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_16_C_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_branchless_C_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_branchless_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_branchless_C_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_branchless_C_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_branchless_C_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_branchless_C_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_branchless_02_C_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_branchless_02_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_branchless_02_C_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_branchless_02_C_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_branchless_02_C_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_branchless_02_C_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_branchless_04_C_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_branchless_04_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_branchless_04_C_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_branchless_04_C_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_branchless_04_C_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_branchless_04_C_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_branchless_08_C_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_branchless_08_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_branchless_08_C_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_branchless_08_C_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_branchless_08_C_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_branchless_08_C_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_branchless_16_C_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_branchless_16_C_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_branchless_16_C_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_branchless_16_C_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_branchless_16_C_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_branchless_16_C_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_02_A_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_02_A_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_02_A_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_02_A_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_02_A_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_02_A_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_04_A_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_04_A_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_04_A_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_04_A_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_04_A_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_04_A_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_08_A_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_08_A_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_08_A_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_08_A_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_08_A_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_08_A_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_16_A_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_16_A_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_16_A_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_16_A_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_16_A_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_16_A_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    

    function sum_up_gt_loop_32_A_i8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_32_A_i8')
        Import
        Integer(c_int64_t)            :: sum_up_gt_loop_32_A_i8
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end function    

    function sum_up_gt_loop_32_A_r8(x,y,v,n) Bind(C,Name='sum_up_gt_loop_32_A_r8')
        Import
        Real(c_double)            :: sum_up_gt_loop_32_A_r8
        Integer(c_int64_t),Value  :: n
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end function    














    subroutine count_and_sum_up_gt_loop_C_i8(sum_left, count_left, x, y, v, n) Bind(C,Name='count_and_sum_up_gt_loop_C_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_C_r8(sum_left, count_left, x, y, v, n) Bind(C,Name='count_and_sum_up_gt_loop_C_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_02_C_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_02_C_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_02_C_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_02_C_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_04_C_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_04_C_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_04_C_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_04_C_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_08_C_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_08_C_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_08_C_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_08_C_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_16_C_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_16_C_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_16_C_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_16_C_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_branchless_C_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_branchless_C_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_branchless_C_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_branchless_C_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_branchless_02_C_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_branchless_02_C_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_branchless_02_C_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_branchless_02_C_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_branchless_04_C_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_branchless_04_C_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_branchless_04_C_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_branchless_04_C_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_branchless_08_C_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_branchless_08_C_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_branchless_08_C_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_branchless_08_C_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_branchless_16_C_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_branchless_16_C_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_branchless_16_C_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_branchless_16_C_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    









    subroutine count_and_sum_up_gt_loop_02_A_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_02_A_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_02_A_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_02_A_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_04_A_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_04_A_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_04_A_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_04_A_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_08_A_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_08_A_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_08_A_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_08_A_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_16_A_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_16_A_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_16_A_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_16_A_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_32_A_i8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_32_A_i8')
        Import
        Integer(c_int64_t),Value      :: n
        Integer(c_int64_t)            :: sum_left
        Integer(c_int64_t)            :: count_left
        Integer(c_int64_t),Intent(In) :: x(n), y(n)
        Integer(c_int64_t),Value      :: v
    end subroutine    

    subroutine count_and_sum_up_gt_loop_32_A_r8(sum_left, count_left, x, y, v, n) & 
        Bind(C,Name='count_and_sum_up_gt_loop_32_A_r8')
        Import
        Integer(c_int64_t),Value  :: n
        Real(c_double)            :: sum_left
        Integer(c_double)            :: count_left
        Real(c_double),Intent(In) :: x(n), y(n)
        Real(c_double),Value  :: v
    end subroutine    
end interface
