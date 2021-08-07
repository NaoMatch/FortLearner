!> An interface to call extern sum_up_matrix functions in c and inline assembler
interface
    subroutine sum_up_matrix_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_C_r8

    subroutine sum_up_matrix_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_C_i8

    subroutine sum_up_matrix_02_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_02_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_02_C_r8

    subroutine sum_up_matrix_02_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_02_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_02_C_i8

    subroutine sum_up_matrix_04_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_C_r8

    subroutine sum_up_matrix_04_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_C_i8

    subroutine sum_up_matrix_08_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_C_r8

    subroutine sum_up_matrix_08_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_C_i8

    subroutine sum_up_matrix_16_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_C_r8

    subroutine sum_up_matrix_16_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_C_i8

    subroutine sum_up_matrix_32_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_32_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_32_C_r8

    subroutine sum_up_matrix_32_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_32_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_32_C_i8

    subroutine sum_up_matrix_64_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_64_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_64_C_r8

    subroutine sum_up_matrix_64_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_64_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_64_C_i8

    subroutine sum_up_matrix_02_02_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_02_02_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_02_02_C_r8

    subroutine sum_up_matrix_02_02_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_02_02_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_02_02_C_i8

    subroutine sum_up_matrix_02_04_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_02_04_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_02_04_C_r8

    subroutine sum_up_matrix_02_04_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_02_04_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_02_04_C_i8

    subroutine sum_up_matrix_02_08_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_02_08_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_02_08_C_r8

    subroutine sum_up_matrix_02_08_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_02_08_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_02_08_C_i8

    subroutine sum_up_matrix_04_02_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_02_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_02_C_r8

    subroutine sum_up_matrix_04_02_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_02_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_02_C_i8

    subroutine sum_up_matrix_04_04_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_04_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_04_C_r8

    subroutine sum_up_matrix_04_04_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_04_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_04_C_i8

    subroutine sum_up_matrix_04_08_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_08_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_08_C_r8

    subroutine sum_up_matrix_04_08_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_08_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_08_C_i8

    subroutine sum_up_matrix_08_02_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_02_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_02_C_r8

    subroutine sum_up_matrix_08_02_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_02_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_02_C_i8

    subroutine sum_up_matrix_08_04_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_04_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_04_C_r8

    subroutine sum_up_matrix_08_04_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_04_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_04_C_i8

    subroutine sum_up_matrix_16_02_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_02_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_02_C_r8

    subroutine sum_up_matrix_16_02_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_02_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_02_C_i8

    subroutine sum_up_matrix_16_04_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_04_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_04_C_r8

    subroutine sum_up_matrix_16_04_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_04_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_04_C_i8

    subroutine sum_up_matrix_32_02_C_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_32_02_C_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_32_02_C_r8

    subroutine sum_up_matrix_32_02_C_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_32_02_C_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_32_02_C_i8

    subroutine sum_up_matrix_04_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_ASM_r8

    subroutine sum_up_matrix_04_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_ASM_i8

    subroutine sum_up_matrix_08_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_ASM_r8

    subroutine sum_up_matrix_08_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_ASM_i8

    subroutine sum_up_matrix_16_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_ASM_r8

    subroutine sum_up_matrix_16_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_ASM_i8

    subroutine sum_up_matrix_32_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_32_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_32_ASM_r8

    subroutine sum_up_matrix_32_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_32_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_32_ASM_i8

    subroutine sum_up_matrix_64_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_64_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_64_ASM_r8

    subroutine sum_up_matrix_64_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_64_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_64_ASM_i8

    subroutine sum_up_matrix_04_02_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_02_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_02_ASM_r8

    subroutine sum_up_matrix_04_02_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_02_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_02_ASM_i8

    subroutine sum_up_matrix_04_04_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_04_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_04_ASM_r8

    subroutine sum_up_matrix_04_04_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_04_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_04_ASM_i8

    subroutine sum_up_matrix_04_04_ASM_Pre_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_04_ASM_Pre_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_04_ASM_Pre_r8

    subroutine sum_up_matrix_04_04_ASM_Pre_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_04_04_ASM_Pre_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_04_04_ASM_Pre_i8

    subroutine sum_up_matrix_08_02_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_02_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_02_ASM_r8

    subroutine sum_up_matrix_08_02_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_02_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_02_ASM_i8

    subroutine sum_up_matrix_08_04_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_04_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_04_ASM_r8

    subroutine sum_up_matrix_08_04_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_08_04_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_08_04_ASM_i8

    subroutine sum_up_matrix_16_02_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_02_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_02_ASM_r8

    subroutine sum_up_matrix_16_02_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_02_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_02_ASM_i8

    subroutine sum_up_matrix_16_04_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_04_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_04_ASM_r8

    subroutine sum_up_matrix_16_04_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_16_04_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_16_04_ASM_i8

    subroutine sum_up_matrix_32_02_ASM_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_32_02_ASM_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_32_02_ASM_r8

    subroutine sum_up_matrix_32_02_ASM_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_32_02_ASM_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_32_02_ASM_i8

    subroutine sum_up_matrix_C_hybrid_r8(x_sum, x, n, c) bind(c, name='sum_up_matrix_C_hybrid_r8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_C_hybrid_r8

    subroutine sum_up_matrix_C_hybrid_i8(x_sum, x, n, c) bind(c, name='sum_up_matrix_C_hybrid_i8')
        import :: c_ptr, c_int64_t
        type(c_ptr), value    :: x_sum, x
        integer(c_int64_t), value :: n, c
    end subroutine sum_up_matrix_C_hybrid_i8

end interface
