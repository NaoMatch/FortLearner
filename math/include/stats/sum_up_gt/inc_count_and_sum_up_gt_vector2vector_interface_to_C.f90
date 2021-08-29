interface 
    subroutine count_and_sum_up_gt_vector2vector_01_C_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_01_C_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_01_C_r8

    subroutine count_and_sum_up_gt_vector2vector_02_C_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_02_C_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_02_C_r8

    subroutine count_and_sum_up_gt_vector2vector_04_C_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_04_C_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_04_C_r8

    subroutine count_and_sum_up_gt_vector2vector_08_C_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_08_C_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_08_C_r8

    subroutine count_and_sum_up_gt_vector2vector_16_C_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_16_C_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_16_C_r8

    subroutine count_and_sum_up_gt_vector2vector_02x_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_02x_A_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_02x_A_r8

    subroutine count_and_sum_up_gt_vector2vector_04x_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_04x_A_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_04x_A_r8

    subroutine count_and_sum_up_gt_vector2vector_04y_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_04y_A_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_04y_A_r8

    subroutine count_and_sum_up_gt_vector2vector_08y_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_08y_A_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_08y_A_r8

    subroutine count_and_sum_up_gt_vector2vector_08z_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_08z_A_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_08z_A_r8

    subroutine count_and_sum_up_gt_vector2vector_16y_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_16y_A_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_16y_A_r8

    subroutine count_and_sum_up_gt_vector2vector_16z_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_16z_A_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_16z_A_r8

    subroutine count_and_sum_up_gt_vector2vector_32y_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_32y_A_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_32y_A_r8

    subroutine count_and_sum_up_gt_vector2vector_32z_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n) & 
        Bind(C,Name="count_and_sum_up_gt_vector2vector_32z_A_r8")
        Import
        real(c_double), value :: y_val
        integer(c_int64_t), value :: n
        real(c_double), intent(in) :: x_vals(n)
        real(c_double), intent(in) :: thr_vals(n)
        real(c_double), intent(inout) :: sum_vals(n)
        integer(c_int64_t), intent(inout) :: cnt_vals(n)
    end subroutine count_and_sum_up_gt_vector2vector_32z_A_r8
end interface

