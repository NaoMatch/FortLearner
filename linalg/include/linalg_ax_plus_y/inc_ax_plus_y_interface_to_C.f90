interface 
    subroutine ax_plus_y_01_C(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_01_C')
        Import
        integer(c_int64_t), value         :: n
        real(c_double), intent(in), value :: a
        real(c_double), intent(in)        :: x(n)
        real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_01_C    

    subroutine ax_plus_y_02_C(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_02_C')
        Import
        integer(c_int64_t), value         :: n
        real(c_double), intent(in), value :: a
        real(c_double), intent(in)        :: x(n)
        real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_02_C    

    subroutine ax_plus_y_04_C(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_04_C')
        Import
        integer(c_int64_t), value         :: n
        real(c_double), intent(in), value :: a
        real(c_double), intent(in)        :: x(n)
        real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_04_C    

    subroutine ax_plus_y_08_C(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_08_C')
        Import
        integer(c_int64_t), value         :: n
        real(c_double), intent(in), value :: a
        real(c_double), intent(in)        :: x(n)
        real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_08_C    



    subroutine ax_plus_y_02x_A(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_02x_A')
        Import
        real(c_double), value     :: a
        type(c_ptr), value        :: x
        type(c_ptr), value        :: y
        integer(c_int64_t), value :: n
        ! real(c_double), intent(in)        :: x(n)
        ! real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_02x_A    

    subroutine ax_plus_y_04x_A(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_04x_A')
        Import
        real(c_double), value     :: a
        type(c_ptr), value        :: x
        type(c_ptr), value        :: y
        integer(c_int64_t), value :: n
        ! real(c_double), intent(in)        :: x(n)
        ! real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_04x_A    

    subroutine ax_plus_y_04y_A(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_04y_A')
        Import
        real(c_double), value     :: a
        type(c_ptr), value        :: x
        type(c_ptr), value        :: y
        integer(c_int64_t), value :: n
        ! real(c_double), intent(in)        :: x(n)
        ! real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_04y_A    

    subroutine ax_plus_y_08x_A(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_08x_A')
        Import
        real(c_double), value     :: a
        type(c_ptr), value        :: x
        type(c_ptr), value        :: y
        integer(c_int64_t), value :: n
        ! real(c_double), intent(in)        :: x(n)
        ! real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_08x_A    

    subroutine ax_plus_y_08y_A(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_08y_A')
        Import
        real(c_double), value     :: a
        type(c_ptr), value        :: x
        type(c_ptr), value        :: y
        integer(c_int64_t), value :: n
        ! real(c_double), intent(in)        :: x(n)
        ! real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_08y_A    

    subroutine ax_plus_y_08z_A(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_08z_A')
        Import
        real(c_double), value     :: a
        ! type(c_ptr), value        :: x
        ! type(c_ptr), value        :: y
        integer(c_int64_t), value :: n
        real(c_double), intent(in)        :: x(n)
        real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_08z_A    

    subroutine ax_plus_y_16y_A(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_16y_A')
        Import
        integer(c_int64_t), value         :: n
        real(c_double), intent(in), value :: a
        real(c_double), intent(in)        :: x(n)
        real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_16y_A    

    subroutine ax_plus_y_16z_A(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_16z_A')
        Import
        integer(c_int64_t), value         :: n
        real(c_double), intent(in), value :: a
        real(c_double), intent(in)        :: x(n)
        real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_16z_A    

    subroutine ax_plus_y_32z_A(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_32z_A')
        Import
        integer(c_int64_t), value         :: n
        real(c_double), intent(in), value :: a
        real(c_double), intent(in)        :: x(n)
        real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_32z_A    

    subroutine ax_plus_y_56z_A(a, x, y, n) & 
        Bind(C,Name='ax_plus_y_56z_A')
        Import
        integer(c_int64_t), value         :: n
        real(c_double), intent(in), value :: a
        real(c_double), intent(in)        :: x(n)
        real(c_double), intent(inout)     :: y(n)
    end subroutine ax_plus_y_56z_A    
end interface