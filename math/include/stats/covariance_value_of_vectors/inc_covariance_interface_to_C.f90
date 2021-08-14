Interface
    function covariance_loop_C(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_C')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_C
    end function covariance_loop_C

    function covariance_loop_02_C(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_02_C')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_02_C
    end function covariance_loop_02_C

    function covariance_loop_04_C(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_04_C')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_04_C
    end function covariance_loop_04_C

    function covariance_loop_08_C(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_08_C')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_08_C
    end function covariance_loop_08_C

    function covariance_loop_16_C(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_16_C')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_16_C
    end function covariance_loop_16_C

    function covariance_loop_02x_A(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_02x_A')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_02x_A
    end function covariance_loop_02x_A

    function covariance_loop_04x_A(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_04x_A')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_04x_A
    end function covariance_loop_04x_A

    function covariance_loop_04y_A(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_04y_A')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_04y_A
    end function covariance_loop_04y_A

    function covariance_loop_08x_A(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_08x_A')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_08x_A
    end function covariance_loop_08x_A

    function covariance_loop_08y_A(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_08y_A')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_08y_A
    end function covariance_loop_08y_A

    function covariance_loop_08z_A(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_08z_A')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_08z_A
    end function covariance_loop_08z_A

    function covariance_loop_16y_A(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_16y_A')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_16y_A
    end function covariance_loop_16y_A

    function covariance_loop_16z_A(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_16z_A')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_16z_A
    end function covariance_loop_16z_A

    function covariance_loop_32y_A(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_32y_A')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_32y_A
    end function covariance_loop_32y_A

    function covariance_loop_32z_A(vec1, vec2, n) & 
        Bind(C,Name='covariance_loop_32z_A')
        Import
        integer(c_int64_t), value  :: n
        real(c_double), intent(in) :: vec1(n), vec2(n)
        real(c_double)             :: covariance_loop_32z_A
    end function covariance_loop_32z_A
end interface
