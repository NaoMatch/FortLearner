module mod_linear
    use mod_random
    use mod_variable
    use mod_intrinsics
    implicit none
    
    type, extends(base_layer) :: linear
        type(variable) :: w, b
    contains
        procedure :: forward => forward_linear
    end type linear

    interface linear
        module procedure new_linear
    end interface linear

contains

    function new_linear(in_size, out_size, no_bias)
        implicit none
        type(linear) :: new_linear
        integer(kind=8), optional :: in_size
        integer(kind=8), intent(in) :: out_size
        logical(kind=4), intent(in) :: no_bias
        real(kind=8), allocatable :: w(:,:), b(:,:)

        ! Set Name
        new_linear%lname = "linear"

        ! Get Arguments
        if (present(in_size)) new_linear%in_size = in_size
        new_linear%out_size = out_size
        new_linear%no_bias = no_bias

        ! Initialize Weight
        if (new_linear%in_size>0_8) then
            allocate(w(in_size, out_size))
            call rand_normal(w, in_size, out_size)
            w = w * sqrt(1d0 / in_size)
            new_linear%w = variable(w, is_parameter=.true.)
            call new_linear%w%set_name("weight")
            call new_linear%set_params(new_linear%w)
        end if

        ! Initialize Bias
        allocate(b(1, out_size))
        b = 0d0
        new_linear%b = variable(b, is_parameter=.true.)
        call new_linear%b%set_name("bias")
        call new_linear%set_params(new_linear%b)
    end function new_linear

    function forward_linear(this, var_in) result(var_out)
        implicit none
        class(linear) :: this
        type(variable) :: var_in
        type(variable) :: var_out

        integer(kind=8) :: n_rows, n_cols
        real(kind=8), allocatable :: w(:,:)

        n_rows = size(var_in%get_data(), dim=1)

        if (this%in_size<0_8) then
            n_cols = size(var_in%get_data(), dim=2)
            this%in_size = n_cols
            allocate(w(n_cols, this%out_size))
            call rand_normal(w, n_cols, this%out_size)
            w = w * sqrt(1d0 / n_cols)
            this%w = variable(w, is_parameter=.true.)
            call this%w%set_name("weight")
            call this%set_params(this%w)
        end if

        if (this%no_bias) then
            var_out = matmul_(var_in, this%w)
        else
            var_out = matmul_(var_in, this%w) + spread_(this%b, dim=1_8, ncopies=n_rows)
        end if
    end function forward_linear

end module mod_linear