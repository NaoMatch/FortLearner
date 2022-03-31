module mod_functions
    use mod_variable
    implicit none

    interface operator(+)
        module procedure :: add_node_node
    end interface operator(+)

    interface operator(/)
        module procedure :: div_node_node
    end interface operator(/)    

    interface operator(**)
        module procedure :: pow_node_r8
        module procedure :: pow_node_i8
        module procedure :: pow_node_r4
        module procedure :: pow_node_i4
    end interface operator(**)    

    interface operator(+)
        module procedure :: add_var_var
    end interface operator(+)    

    interface operator(*)
        module procedure :: mul_var_var
        module procedure :: mul_scl_var
    end interface operator(*)    

    interface operator(-)
        module procedure :: sub_var_var
        module procedure :: sub_var_scl
    end interface operator(-)    

    interface operator(/)
        module procedure :: div_var_var
    end interface operator(/)    

    interface operator(**)
        module procedure :: pow_var_r8
        module procedure :: pow_var_i8
        module procedure :: pow_var_r4
        module procedure :: pow_var_i4
    end interface operator(**)    

contains

    ! Sigmoid function
    function sigmoid(x)
        type(node), target, intent(in) :: x
        type(node) :: sigmoid        
        sigmoid%val = 1.d0/(1.d0 + exp(-x%val))

        sigmoid%node_l => x

        sigmoid%grad_l = sigmoid%val * (1.d0 - sigmoid%val)
    end function sigmoid

    ! node1 + node2
    function add_node_node(x1, x2)
        type(node), target, intent(in) :: x1, x2
        type(node) :: add_node_node
        add_node_node%val = x1%val + x2%val

        add_node_node%node_l => x1
        add_node_node%node_r => x2

        add_node_node%grad_l = 1d0
        add_node_node%grad_r = 1d0
    end function add_node_node

    ! node1 / node2
    function div_node_node(x1, x2)
        type(node), intent(in) :: x1, x2
        type(node) :: div_node_node
        div_node_node%val = x1%val / x2%val
    end function div_node_node

    ! node ** val
    function pow_node_r8(x, c)
        type(node), target, intent(in)   :: x
        real(kind=8), intent(in) :: c
        type(node) :: pow_node_r8
        pow_node_r8%val = x%val ** c

        pow_node_r8%node_l => x

        pow_node_r8%grad_l = c * pow_node_r8%val ** (c-1)
    end function pow_node_r8

    function pow_node_r4(x, c)
        type(node), target, intent(in)   :: x
        real(kind=4), intent(in) :: c
        type(node) :: pow_node_r4
        pow_node_r4%val = x%val ** c

        pow_node_r4%node_l => x

        pow_node_r4%grad_l = c * pow_node_r4%val ** (c-1)
    end function pow_node_r4

    function pow_node_i8(x, c)
        type(node), target, intent(in)   :: x
        integer(kind=8), intent(in) :: c
        type(node) :: pow_node_i8
        pow_node_i8%val = x%val ** c

        pow_node_i8%node_l => x

        pow_node_i8%grad_l = c * pow_node_i8%val ** (c-1)
    end function pow_node_i8

    function pow_node_i4(x, c)
        type(node), target, intent(in)   :: x
        integer(kind=4), intent(in) :: c
        type(node) :: pow_node_i4
        pow_node_i4%val = x%val ** c

        pow_node_i4%node_l => x

        pow_node_i4%grad_l = c * pow_node_i4%val ** (c-1)
    end function pow_node_i4



    ! Sigmoid function
    function sigmoid_var(x)
        type(variable), target, intent(in) :: x
        type(variable) :: sigmoid_var        

        allocate(sigmoid_var%nptr)

        sigmoid_var%nptr%val = 1.d0/(1.d0 + exp(-x%nptr%val))

        sigmoid_var%nptr%node_l => x%nptr

        sigmoid_var%nptr%grad_l = sigmoid_var%nptr%val * (1.d0 - sigmoid_var%nptr%val)
    end function sigmoid_var

    ! node1 + node2
    function add_var_var(x1, x2)
        type(variable), target, intent(in) :: x1, x2
        type(variable) :: add_var_var

        allocate(add_var_var%nptr)

        add_var_var%nptr%val = x1%nptr%val + x2%nptr%val

        add_var_var%nptr%node_l => x1%nptr
        add_var_var%nptr%node_r => x2%nptr

        add_var_var%nptr%grad_l = 1d0
        add_var_var%nptr%grad_r = 1d0
    end function add_var_var

    ! node1 - node2
    function sub_var_var(x1, x2)
        type(variable), target, intent(in) :: x1, x2
        type(variable) :: sub_var_var

        allocate(sub_var_var%nptr)

        sub_var_var%nptr%val = x1%nptr%val - x2%nptr%val

        sub_var_var%nptr%node_l => x1%nptr
        sub_var_var%nptr%node_r => x2%nptr

        sub_var_var%nptr%grad_l = 1d0
        sub_var_var%nptr%grad_r = -1d0
    end function sub_var_var

    function sub_var_scl(x1, x2)
        type(variable), target, intent(in) :: x1
        real(kind=8), intent(in) :: x2
        type(variable) :: sub_var_scl

        allocate(sub_var_scl%nptr)

        sub_var_scl%nptr%val = x1%nptr%val - x2

        sub_var_scl%nptr%node_l => x1%nptr

        sub_var_scl%nptr%grad_l = 1d0
    end function sub_var_scl

    ! node1 * node2
    function mul_var_var(x1, x2)
        type(variable), target, intent(in) :: x1, x2
        type(variable) :: mul_var_var

        allocate(mul_var_var%nptr)

        mul_var_var%nptr%val = x1%nptr%val * x2%nptr%val

        mul_var_var%nptr%node_l => x1%nptr
        mul_var_var%nptr%node_r => x2%nptr

        mul_var_var%nptr%grad_l = x2%nptr%val
        mul_var_var%nptr%grad_r = x1%nptr%val
    end function mul_var_var

    function mul_scl_var(x1, x2)
        real(kind=8), intent(in) :: x1
        type(variable), target, intent(in) :: x2
        type(variable) :: mul_scl_var

        allocate(mul_scl_var%nptr)

        mul_scl_var%nptr%val = x1 * x2%nptr%val

        mul_scl_var%nptr%node_r => x2%nptr

        mul_scl_var%nptr%grad_r = x1
    end function mul_scl_var

    ! node1 / node2
    function div_var_var(x1, x2)
        type(variable), intent(in) :: x1, x2
        type(variable) :: div_var_var

        allocate(div_var_var%nptr)
        div_var_var%nptr%val = x1%nptr%val / x2%nptr%val
    end function div_var_var

    ! node ** val
    function pow_var_r8(x, c)
        type(variable), target, intent(in)   :: x
        real(kind=8), intent(in) :: c
        type(variable) :: pow_var_r8

        allocate(pow_var_r8%nptr)

        pow_var_r8%nptr%val = x%nptr%val ** c

        pow_var_r8%nptr%node_l => x%nptr

        pow_var_r8%nptr%grad_l = c * pow_var_r8%nptr%val ** (c-1)
    end function pow_var_r8

    function pow_var_r4(x, c)
        type(variable), target, intent(in)   :: x
        real(kind=4), intent(in) :: c
        type(variable) :: pow_var_r4

        allocate(pow_var_r4%nptr)

        pow_var_r4%nptr%val = x%nptr%val ** c

        pow_var_r4%nptr%node_l => x%nptr

        pow_var_r4%nptr%grad_l = c * pow_var_r4%nptr%val ** (c-1)
    end function pow_var_r4

    function pow_var_i8(x, c)
        type(variable), target, intent(in)   :: x
        integer(kind=8), intent(in) :: c
        type(variable) :: pow_var_i8

        allocate(pow_var_i8%nptr)

        pow_var_i8%nptr%val = x%nptr%val ** c

        pow_var_i8%nptr%node_l => x%nptr

        pow_var_i8%nptr%grad_l = c * pow_var_i8%nptr%val ** (c-1)
    end function pow_var_i8

    function pow_var_i4(x, c)
        type(variable), target, intent(in)   :: x
        integer(kind=4), intent(in) :: c
        type(variable) :: pow_var_i4

        allocate(pow_var_i4%nptr)

        pow_var_i4%nptr%val = x%nptr%val ** c

        pow_var_i4%nptr%node_l => x%nptr

        pow_var_i4%nptr%grad_l = c * pow_var_i4%nptr%val ** (c-1)
    end function pow_var_i4



end module mod_functions
