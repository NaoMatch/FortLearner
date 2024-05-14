module mod_splinear
    use mod_random
    use mod_variable
    use mod_intrinsics
    use mod_functions
    implicit none
    
    type, extends(base_layer) :: splinear
        type(variable) :: w, b
    contains
        procedure :: forward => forward_splinear
    end type splinear

    interface splinear
        module procedure new_splinear
    end interface splinear

contains

    function new_splinear(in_size, out_size, no_bias)
        implicit none
        type(splinear) :: new_splinear
        integer(kind=8), optional :: in_size
        integer(kind=8), intent(in) :: out_size
        logical(kind=4), intent(in) :: no_bias
        real(kind=8), allocatable :: w(:,:), b(:,:)

        ! Set Name
        new_splinear%lname = "splinear"

        ! Get Arguments
        if (present(in_size)) new_splinear%in_size = in_size
        new_splinear%out_size = out_size
        new_splinear%no_bias = no_bias

        ! Initialize Weight
        if (new_splinear%in_size>0_8) then
            ! allocate(w(in_size, out_size))
            ! call rand_normal(w, in_size, out_size)
            allocate(w(out_size, in_size))
            call rand_normal(w, out_size, in_size)
            w = w * sqrt(1d0 / in_size)
            new_splinear%w = variable(w, is_parameter=.true.)
            call new_splinear%w%set_name("weight")
            call new_splinear%set_params(new_splinear%w)
        end if

        ! Initialize Bias
        allocate(b(1, out_size))
        b = 0d0
        new_splinear%b = variable(b, is_parameter=.true.)
        call new_splinear%b%set_name("bias")
        call new_splinear%set_params(new_splinear%b)
    end function new_splinear

    function forward_splinear(this, var_in) result(var_out)
        implicit none
        class(splinear) :: this
        type(variable) :: var_in
        type(variable) :: var_out

        integer(kind=8) :: n_rows, n_cols
        real(kind=8), allocatable :: w(:,:)

        ! n_rows = size(var_in%get_data(), dim=1)
        n_rows = vstack(var_in%id)%csr_v%n_rows

        if (this%in_size<0_8) then
            n_cols = vstack(var_in%id)%csr_v%n_cols
            this%in_size = n_cols
            ! allocate(w(n_cols, this%out_size))
            ! call rand_normal(w, n_cols, this%out_size)
            allocate(w(this%out_size, n_cols))
            call rand_normal(w, this%out_size, n_cols)
            w = w * sqrt(1d0 / n_cols)
            this%w = variable(w, is_parameter=.true.)
            call this%w%set_name("weight")
            call this%set_params(this%w)
        end if

        if (this%no_bias) then
            var_out = sp_matmul_(var_in, this%w)
        else
            var_out = sp_matmul_(var_in, this%w) + spread_(this%b, dim=1_8, ncopies=n_rows)
        end if
    end function forward_splinear

end module mod_splinear