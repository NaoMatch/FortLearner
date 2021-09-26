interface
    subroutine matrix_sqsum_row_01x01_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_01x01_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_01x01_C_r8

    subroutine matrix_sqsum_row_02x01_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x01_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x01_C_r8

    subroutine matrix_sqsum_row_04x01_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x01_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x01_C_r8

    subroutine matrix_sqsum_row_08x01_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x01_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x01_C_r8

    subroutine matrix_sqsum_row_16x01_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_16x01_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_16x01_C_r8

    subroutine matrix_sqsum_row_32x01_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_32x01_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_32x01_C_r8

    subroutine matrix_sqsum_row_01x02_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_01x02_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_01x02_C_r8

    subroutine matrix_sqsum_row_01x04_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_01x04_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_01x04_C_r8

    subroutine matrix_sqsum_row_01x08_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_01x08_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_01x08_C_r8

    subroutine matrix_sqsum_row_01x16_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_01x16_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_01x16_C_r8

    subroutine matrix_sqsum_row_01x32_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_01x32_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_01x32_C_r8

    subroutine matrix_sqsum_row_02x02_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x02_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x02_C_r8

    subroutine matrix_sqsum_row_02x04_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x04_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x04_C_r8

    subroutine matrix_sqsum_row_02x08_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x08_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x08_C_r8

    subroutine matrix_sqsum_row_02x16_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x16_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x16_C_r8

    subroutine matrix_sqsum_row_02x32_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x32_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x32_C_r8

    subroutine matrix_sqsum_row_04x02_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x02_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x02_C_r8

    subroutine matrix_sqsum_row_04x04_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x04_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x04_C_r8

    subroutine matrix_sqsum_row_04x08_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x08_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x08_C_r8

    subroutine matrix_sqsum_row_04x16_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x16_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x16_C_r8

    subroutine matrix_sqsum_row_04x32_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x32_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x32_C_r8

    subroutine matrix_sqsum_row_08x02_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x02_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x02_C_r8

    subroutine matrix_sqsum_row_08x04_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x04_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x04_C_r8

    subroutine matrix_sqsum_row_08x08_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x08_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x08_C_r8

    subroutine matrix_sqsum_row_08x16_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x16_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x16_C_r8

    subroutine matrix_sqsum_row_08x32_C_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x32_C_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x32_C_r8



    subroutine matrix_sqsum_row_02x01_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x01_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x01_A_r8

    subroutine matrix_sqsum_row_04x01_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x01_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x01_A_r8

    subroutine matrix_sqsum_row_08x01_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x01_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x01_A_r8

    subroutine matrix_sqsum_row_16x01_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_16x01_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_16x01_A_r8

    subroutine matrix_sqsum_row_32x01_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_32x01_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_32x01_A_r8

    subroutine matrix_sqsum_row_02x02_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x02_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x02_A_r8

    subroutine matrix_sqsum_row_02x04_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x04_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x04_A_r8

    subroutine matrix_sqsum_row_02x08_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x08_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x08_A_r8

    subroutine matrix_sqsum_row_02x16_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x16_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x16_A_r8

    subroutine matrix_sqsum_row_02x32_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_02x32_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_02x32_A_r8

    subroutine matrix_sqsum_row_04x02_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x02_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x02_A_r8

    subroutine matrix_sqsum_row_04x04_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x04_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x04_A_r8

    subroutine matrix_sqsum_row_04x08_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x08_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x08_A_r8

    subroutine matrix_sqsum_row_04x16_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x16_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x16_A_r8

    subroutine matrix_sqsum_row_04x32_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_04x32_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_04x32_A_r8

    subroutine matrix_sqsum_row_08x02_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x02_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x02_A_r8

    subroutine matrix_sqsum_row_08x04_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x04_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x04_A_r8

    subroutine matrix_sqsum_row_08x08_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x08_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x08_A_r8

    subroutine matrix_sqsum_row_08x16_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x16_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x16_A_r8

    subroutine matrix_sqsum_row_08x32_A_r8(mat_ptr, matrix_sqsum_vals, n_samples, n_columns) &
        Bind(C, Name="matrix_sqsum_row_08x32_A_r8")
        Import
        integer(c_int64_t), value :: n_samples, n_columns
        type(c_ptr), value :: mat_ptr
        real(c_double), intent(inout) :: matrix_sqsum_vals(n_samples)
    end subroutine matrix_sqsum_row_08x32_A_r8
end interface 