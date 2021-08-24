Interface
    subroutine get_minmax_vector2vector_01_C_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_01_C_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_01_C_r8

    subroutine get_minmax_vector2vector_02_C_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_02_C_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_02_C_r8

    subroutine get_minmax_vector2vector_04_C_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_04_C_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_04_C_r8

    subroutine get_minmax_vector2vector_08_C_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_08_C_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_08_C_r8

    subroutine get_minmax_vector2vector_16_C_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_16_C_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_16_C_r8

    subroutine get_minmax_vector2vector_02x_A_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_02x_A_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_02x_A_r8

    subroutine get_minmax_vector2vector_04x_A_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_04x_A_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_04x_A_r8

    subroutine get_minmax_vector2vector_04y_A_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_04y_A_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_04y_A_r8

    subroutine get_minmax_vector2vector_08y_A_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_08y_A_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_08y_A_r8

    subroutine get_minmax_vector2vector_08z_A_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_08z_A_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_08z_A_r8

    subroutine get_minmax_vector2vector_16y_A_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_16y_A_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_16y_A_r8

    subroutine get_minmax_vector2vector_16z_A_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_16z_A_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_16z_A_r8

    subroutine get_minmax_vector2vector_32z_A_r8(vals, min_vals, max_vals, n) Bind(C,Name='get_minmax_vector2vector_32z_A_r8')
        Import
        integer(c_int64_t), value :: n
        real(c_double), intent(in)    :: vals(n)
        real(c_double), intent(inout) :: min_vals(n), max_vals(n)
    end subroutine get_minmax_vector2vector_32z_A_r8
end interface