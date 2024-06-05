module mod_matrix_multiplication
    use mod_timer
    use mod_variable
    implicit none

    type, extends(base_function) :: matrix_multiplication
    contains
        procedure :: forward_2in_1out => forward_matrix_multiplication
        procedure :: backward_1in_2out => backward_matrix_multiplication
    end type matrix_multiplication
    
    interface matrix_multiplication
        module procedure new_matrix_multiplication
    end interface matrix_multiplication


contains

    function matmul_(var_in_1, var_in_2) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in_1, var_in_2
        type(variable) :: var_out
        type(matrix_multiplication) :: mat_mul_
        mat_mul_ = matrix_multiplication()
        var_out = mat_mul_%act(var_in_1, var_in_2)
    end function matmul_

    function new_matrix_multiplication()
        implicit none
        type(matrix_multiplication) :: new_matrix_multiplication
        new_matrix_multiplication%fname = "matrix_multiplication"
        new_matrix_multiplication%n_in = 2
        new_matrix_multiplication%n_out = 1
    end function new_matrix_multiplication

    subroutine forward_matrix_multiplication(this, v_out, v_in_1, v_in_2)
        implicit none
        class(matrix_multiplication) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)

        integer(kind=8) :: shape1(2), shape2(2)

        shape1 = shape(v_in_1)
        shape2 = shape(v_in_2)
        if (shape1(2) /= shape2(1)) goto 990
        call dgemm("N", "N", & 
            shape1(1), shape2(2), shape1(2), &
            1d0, & 
            v_in_1, shape1(1), &
            v_in_2, shape2(1), &
            0d0, &
            v_out, shape1(1))
        return
        990 continue 
        print*, "Shape ", shape1, " ::: ", shape2
        stop "matmul_ shape mismatch error."
    end subroutine forward_matrix_multiplication

    function backward_matrix_multiplication(this, g_in) result(g_outs)
        implicit none
        class(matrix_multiplication) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: shape1(2), shape2(2)
        integer(kind=8)        :: date_value1(8), date_value2(8)
        integer(kind=8) :: n_rows1, n_cols1, n_feats

        call date_and_time(values=date_value1)
        ! g_outs(1)%g = matmul(g_in, transpose(vstack(this%id_in_2)%v))
        n_rows1 = size(g_in, dim=1)
        n_feats = size(g_in, dim=2)
        n_cols1 = size(vstack(this%id_in_2)%v, dim=1)
        allocate(g_outs(1)%g(n_rows1, n_cols1))
        call dgemm("N", "T", & 
            n_rows1, n_cols1, n_feats, &
            1d0, & 
            g_in, n_rows1, &
            vstack(this%id_in_2)%v, n_cols1, &
            0d0, &
            g_outs(1)%g, n_rows1)        
        call date_and_time(values=date_value2)
        ! print*, "       ----- Matmul Backprop_1: ", time_diff(date_value1, date_value2)
        
        
        call date_and_time(values=date_value1)
        n_rows1 = size(vstack(this%id_in_1)%v, dim=2)
        n_feats = size(vstack(this%id_in_1)%v, dim=1)
        n_cols1 = size(g_in, dim=2)
        allocate(g_outs(2)%g(n_rows1, n_cols1))
        call dgemm("T", "N", & 
            n_rows1, n_cols1, n_feats, &
            1d0, & 
            vstack(this%id_in_1)%v, n_feats, &
            g_in, n_feats, &
            0d0, &
            g_outs(2)%g, n_rows1)
        call date_and_time(values=date_value2)
        ! print*, "       ----- Matmul Backprop_2: ", time_diff(date_value1, date_value2)
    end function backward_matrix_multiplication

end module mod_matrix_multiplication