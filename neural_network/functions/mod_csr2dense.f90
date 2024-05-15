module mod_csr2dense
    use mod_csr
    use mod_variable
    implicit none

    type, extends(base_function) :: csr2dense
    contains
        procedure :: forward_1in_csr_1out => forward_csr2dense
        procedure :: backward_1in_2out => backward_csr2dense
    end type csr2dense
    
    interface csr2dense
        module procedure new_csr2dense
    end interface csr2dense

contains

    function csr2dense_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(csr2dense) :: c2d

        c2d = csr2dense()

        var_out = c2d%act(var_in)
    end function csr2dense_

    function new_csr2dense()
        implicit none
        type(csr2dense) :: new_csr2dense
        new_csr2dense%fname = "csr2dense"
        new_csr2dense%n_in = 1
        new_csr2dense%n_out = 1
    end function new_csr2dense

    function forward_csr2dense(this, v_in) result(v_out)
        implicit none
        class(csr2dense) :: this
        type(csr_matrix) :: v_in
        real(kind=8), allocatable :: v_out(:,:)
        v_out = v_in%to_dense()
    end function forward_csr2dense

    function backward_csr2dense(this, g_in) result(g_outs)
        implicit none
        class(csr2dense) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        allocate(g_outs(1)%csr_g, source=vstack(this%id_in_1)%csr_v)
        g_outs(1)%csr_g%vals = extract_corresponding_position_vals(g_in, vstack(this%id_in_1)%csr_v)
    end function backward_csr2dense

end module mod_csr2dense