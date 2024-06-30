module mod_sparse_matrix_multiplication
    use mod_csr
    use mod_fixed_size_csr
    use mod_sparse_dgemm
    use mod_variable
    implicit none

    type, extends(base_function) :: sparse_matrix_multiplication
    contains
        procedure :: forward_2in_1out_csr => forward_sparse_matrix_multiplication
        procedure :: backward_1in_2out => backward_sparse_matrix_multiplication
    end type sparse_matrix_multiplication
    
    interface sparse_matrix_multiplication
        module procedure new_sparse_matrix_multiplication
    end interface sparse_matrix_multiplication


contains

    function sp_matmul_(var_in_1, var_in_2) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in_1, var_in_2
        type(variable) :: var_out
        type(sparse_matrix_multiplication) :: sp_mat_mul_
        sp_mat_mul_ = sparse_matrix_multiplication()
        var_out = sp_mat_mul_%act(var_in_1, var_in_2)
    end function sp_matmul_

    function new_sparse_matrix_multiplication()
        implicit none
        type(sparse_matrix_multiplication) :: new_sparse_matrix_multiplication
        new_sparse_matrix_multiplication%fname = "sparse_matrix_multiplication"
        new_sparse_matrix_multiplication%n_in = 2
        new_sparse_matrix_multiplication%n_out = 1
    end function new_sparse_matrix_multiplication

    subroutine forward_sparse_matrix_multiplication(this, v_out, v_in_1, v_in_2)
        implicit none
        class(sparse_matrix_multiplication) :: this
        type(fixed_size_csr_matrix), intent(in) :: v_in_1
        real(kind=8), intent(in) :: v_in_2(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)

        integer(kind=8) :: shape1(2), shape2(2), n_jobs, n, m, k, l

        ! print*, "               ", 1
        shape1(1) = v_in_1%n_rows
        shape1(2) = v_in_1%n_cols
        shape2 = shape(v_in_2)
        if (shape1(2) /= shape2(2)) goto 990
        
        ! print*, "               ", 2
        m = v_in_1%n_rows
        k = v_in_1%n_cols
        l = size(v_in_2, dim=1)
        n = 10_8 ! dummy
        
        ! print*, "               ", 3
        n_jobs = 1_8
        call sparse_dgemm(v_out, v_in_1%rows, v_in_1%cols, v_in_1%vals, v_in_2, n, m, k, l, n_jobs)
        
        ! print*, "               ", 4
        
        return
        990 continue 
        print*, "Shape ", shape1, " ::: ", shape2
        stop "matmul_ shape mismatch error."
    end subroutine forward_sparse_matrix_multiplication

    function backward_sparse_matrix_multiplication(this, g_in) result(g_outs)
        implicit none
        class(sparse_matrix_multiplication) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: shape1(2), shape2(2), nnz
        real(kind=8), allocatable :: v_in_1(:,:)
        integer(kind=8) :: n_rows1, n_cols1, n_feats

        ! call date_and_time(values=date_value1)
        g_outs(1)%g = matmul(transpose(vstack(this%id_in_2)%v), g_in)
        ! call date_and_time(values=date_value2)
        ! print*, "       ----- Matmul Backprop_1: ", time_diff(date_value1, date_value2)
        
        ! call date_and_time(values=date_value1)
        allocate(g_outs(1)%csr_g, source=vstack(this%id_in_1)%csr_v)
        v_in_1 = transpose(vstack(this%id_in_1)%csr_v%to_dense())
        g_outs(1)%csr_g%vals = pack(g_outs(1)%g, v_in_1>=0d0)
        deallocate(g_outs(1)%g)
        g_outs(2)%g = matmul(g_in, transpose(v_in_1))
        ! call date_and_time(values=date_value2)
        ! print*, "       ----- Matmul Backprop_1: ", time_diff(date_value1, date_value2)
    end function backward_sparse_matrix_multiplication

end module mod_sparse_matrix_multiplication