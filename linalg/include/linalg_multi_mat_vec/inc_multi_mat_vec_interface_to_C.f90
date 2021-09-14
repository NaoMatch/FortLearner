interface
    subroutine multi_mat_vec_01x01_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_01x01_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_01x01_N_C_r8

    subroutine multi_mat_vec_02x01_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_02x01_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_02x01_N_C_r8

    subroutine multi_mat_vec_04x01_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_04x01_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_04x01_N_C_r8

    subroutine multi_mat_vec_08x01_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_08x01_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_08x01_N_C_r8

    subroutine multi_mat_vec_01x02_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_01x02_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_01x02_N_C_r8

    subroutine multi_mat_vec_02x02_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_02x02_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_02x02_N_C_r8

    subroutine multi_mat_vec_04x02_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_04x02_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_04x02_N_C_r8

    subroutine multi_mat_vec_08x02_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_08x02_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_08x02_N_C_r8

    subroutine multi_mat_vec_01x04_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_01x04_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_01x04_N_C_r8

    subroutine multi_mat_vec_02x04_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_02x04_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_02x04_N_C_r8

    subroutine multi_mat_vec_04x04_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_04x04_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_04x04_N_C_r8

    subroutine multi_mat_vec_08x04_N_C_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_08x04_N_C_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_08x04_N_C_r8






    subroutine multi_mat_vec_01x01_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_01x01_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_01x01_N_A_r8

    subroutine multi_mat_vec_02x01_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_02x01_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_02x01_N_A_r8

    subroutine multi_mat_vec_04x01_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_04x01_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_04x01_N_A_r8

    subroutine multi_mat_vec_08x01_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_08x01_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_08x01_N_A_r8

    subroutine multi_mat_vec_16x01_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_16x01_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_16x01_N_A_r8

    subroutine multi_mat_vec_32x01_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_32x01_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_32x01_N_A_r8

    subroutine multi_mat_vec_01x02_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_01x02_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_01x02_N_A_r8

    subroutine multi_mat_vec_02x02_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_02x02_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_02x02_N_A_r8

    subroutine multi_mat_vec_04x02_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_04x02_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_04x02_N_A_r8

    subroutine multi_mat_vec_08x02_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_08x02_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_08x02_N_A_r8

    subroutine multi_mat_vec_16x02_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_16x02_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_16x02_N_A_r8

    subroutine multi_mat_vec_32x02_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_32x02_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_32x02_N_A_r8

    subroutine multi_mat_vec_01x04_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_01x04_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_01x04_N_A_r8

    subroutine multi_mat_vec_02x04_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_02x04_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_02x04_N_A_r8

    subroutine multi_mat_vec_04x04_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_04x04_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_04x04_N_A_r8

    subroutine multi_mat_vec_08x04_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_08x04_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_08x04_N_A_r8    

    subroutine multi_mat_vec_16x04_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_16x04_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_16x04_N_A_r8    

    subroutine multi_mat_vec_32x04_N_A_r8(mat_ptr, vec, res, n_samples, n_columns) &
        Bind(C, Name="multi_mat_vec_32x04_N_A_r8")
        Import
        integer(c_int64_t), value     :: n_samples, n_columns
        type(c_ptr), value            :: mat_ptr
        real(c_double), intent(in)    :: vec(n_columns)
        real(c_double), intent(inout) :: res(n_samples)
    end subroutine multi_mat_vec_32x04_N_A_r8    
end interface