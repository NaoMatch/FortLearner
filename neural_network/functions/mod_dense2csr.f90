module mod_dense2csr
    use mod_csr
    use mod_fixed_size_csr
    use mod_variable
    implicit none

    type, extends(base_function) :: dense2csr
    contains
        procedure :: forward_1in_1out_csr => forward_dense2csr
        procedure :: backward_1in_2out_csr => backward_dense2csr
    end type dense2csr
    
    interface dense2csr
        module procedure new_dense2csr
    end interface dense2csr

contains

    function dense2csr_(var_in, top_k, how) result(var_out)
        implicit none
        type(variable) :: var_in
        integer(kind=8) :: top_k
        character(len=*) :: how
        type(variable) :: var_out

        type(dense2csr) :: rec

        rec = dense2csr(top_k, how)

        var_out = rec%act(var_in)
    end function dense2csr_

    function new_dense2csr(top_k, how)
        implicit none
        type(dense2csr) :: new_dense2csr
        character(len=*) :: how
        integer(kind=8) :: top_k
        new_dense2csr%fname = "dense2csr"
        new_dense2csr%top_k = top_k
        new_dense2csr%how = how
        new_dense2csr%n_in = 1
        new_dense2csr%n_out = 1
    end function new_dense2csr

    subroutine forward_dense2csr(this, v_out, v_in)
        implicit none
        class(dense2csr) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        type(fixed_size_csr_matrix), allocatable, intent(inout) :: v_out
        call dense2fcsr_weighted_sampling_mat(v_out, v_in, this%top_k, &
            dim=1_8, start_index=0_8, negative_weights=this%how)
    end subroutine forward_dense2csr

    function backward_dense2csr(this, g_in) result(g_outs)
        implicit none
        class(dense2csr) :: this
        type(fixed_size_csr_matrix), intent(in) :: g_in
        type(jagged_matrix) :: g_outs(2)

        allocate(g_outs(1)%g(g_in%n_cols, g_in%n_rows))
        g_outs(1)%g = transpose(g_in%to_dense())
    end function backward_dense2csr

end module mod_dense2csr