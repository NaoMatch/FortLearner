module mod_multi_layer_perceptron
    implicit none

    type variable
        integer(kind=4)           :: dim
        integer(kind=4)           :: generation
        type(layer), pointer :: creator => null()
    contains
        procedure :: info => info_variable
        procedure :: plot_graph
    end type variable

    interface variable
        module procedure :: new_variable
    end interface variable

    type variable_ptr
        type(variable), pointer :: ptr
    end type variable_ptr

    type layer_ptr
        type(layer), pointer :: ptr
    end type layer_ptr


    type model_builder
        character(len=256) :: model_name
        type(variable_ptr), ALLOCATABLE :: input_ptrs(:)
        type(variable_ptr), ALLOCATABLE :: output_ptrs(:)
        type(layer_ptr), ALLOCATABLE    :: layer_ptrs(:)
    contains
        procedure :: dense => add_dense_layer
        procedure :: sum => add_sum_layer

        procedure :: dense_func
    end type model_builder

    interface model_builder
        module procedure :: new_model_builder
    end interface model_builder


    type layer
        integer(kind=4)         :: input_dim
        integer(kind=4)         :: output_dim
        integer(kind=4)         :: generation
        character(len=12)      :: layer_type
        integer(kind=4)         :: len_layer_type
        type(variable), pointer :: input_var0 => null()
        type(variable), pointer :: input_var1 => null()
        type(variable), pointer :: output_var => null()
        type(variable_ptr), allocatable :: input_vars(:)
    contains
        procedure :: info => info_layer
    end type layer

    interface layer
        module procedure :: new_layer
    end interface layer

contains

    function new_layer()
        implicit none
        type(layer) :: new_layer
    end function new_layer


    function new_model_builder(model_name)
        implicit none
        type(model_builder) :: new_model_builder
        character(len=*), optional :: model_name

        if (present(model_name)) then
            new_model_builder%model_name = model_name
        else
            new_model_builder%model_name = "flearner_mlp"
        end if

        ! Deallocate
        if (allocated(new_model_builder%input_ptrs)) deallocate(new_model_builder%input_ptrs)
        allocate(new_model_builder%input_ptrs(0))
        if (allocated(new_model_builder%output_ptrs)) deallocate(new_model_builder%output_ptrs)
        allocate(new_model_builder%output_ptrs(0))
        if (allocated(new_model_builder%layer_ptrs)) deallocate(new_model_builder%layer_ptrs)
        allocate(new_model_builder%layer_ptrs(0))
    end function new_model_builder


    function dense_func(this, input_var, input_dim, output_dim)
        implicit none
        class(model_builder)                  :: this
        type(variable), target, intent(inout) :: input_var
        type(variable), target                :: dense_func
        integer(kind=4), intent(in), optional :: input_dim, output_dim

        type(variable), pointer                :: output_var

        type(variable), target :: in_var_dense
        type(variable), target :: out_var_dense
        type(layer), target    :: lay_dense

        type(variable_ptr)     :: var_i, var_o
        type(layer_ptr)        :: lay

        ! output_var => dense_func
        dense_func = variable()

        ! Shape Check
        if (present(input_dim)) then
            input_var%dim = input_dim
        end if

        if (present(output_dim)) then
            dense_func%dim = output_dim
        end if

        ! Generation
        dense_func%generation = input_var%generation+1
        dense_func%creator => lay_dense

        ! Layer Information
        lay_dense%input_dim = input_var%dim
        lay_dense%output_dim = dense_func%dim
        lay_dense%layer_type = "dense"
        lay_dense%generation = input_var%generation+1

        allocate(var_i%ptr)
        allocate(var_o%ptr)
        allocate(lay%ptr)
        var_i%ptr => input_var
        var_o%ptr => dense_func
        lay%ptr => lay_dense

        this%input_ptrs  = [this%input_ptrs, var_i]
        this%output_ptrs = [this%output_ptrs, var_o]
        this%layer_ptrs  = [this%layer_ptrs, lay]

        ! Check
        call input_var%info()
        call dense_func%info()
        call lay_dense%info()

    end function dense_func



    subroutine add_dense_layer(this, input_var, output_var, layer_ptr, input_dim, output_dim)
        implicit none
        class(model_builder)                   :: this
        type(variable), pointer, intent(inout) :: input_var
        type(variable), pointer, intent(inout) :: output_var
        integer(kind=4), optional, intent(in)  :: input_dim
        integer(kind=4), optional, intent(in)  :: output_dim

        type(layer), pointer :: layer_ptr

        ! Shape Check
        if (present(input_dim)) then
            input_var%dim = input_dim
        end if

        if (present(output_dim)) then
            output_var%dim = output_dim
        end if

        ! New Layer Information
        layer_ptr%layer_type = "dense"
        layer_ptr%len_layer_type = 5
        layer_ptr%input_dim  = input_var%dim
        layer_ptr%output_dim = output_dim
        layer_ptr%generation = input_var%generation+1
        layer_ptr%output_var => output_var


        ! Associate
        allocate(layer_ptr%input_var0)
        layer_ptr%input_var0 => input_var

        allocate(output_var%creator)
        output_var%generation = layer_ptr%generation
        output_var%creator => layer_ptr
    end subroutine add_dense_layer

    subroutine add_sum_layer(this, input_var0, input_var1, output_var, layer_ptr, input_dim, output_dim)
        implicit none
        class(model_builder)                   :: this
        type(variable), pointer, intent(inout) :: input_var0
        type(variable), pointer, intent(inout) :: input_var1
        type(variable), pointer, intent(inout) :: output_var
        integer(kind=4), optional, intent(in)  :: input_dim
        integer(kind=4), optional, intent(in)  :: output_dim
        type(layer), pointer :: layer_ptr

        ! Shape Check
        if (present(input_dim)) then
            input_var0%dim = input_dim
            input_var1%dim = input_dim
        end if

        if (present(output_dim)) then
            output_var%dim = output_dim
        end if

        if (input_dim .ne. output_dim) then
            stop "'input_dim' must be equal 'output_dim'."
        end if


        ! New Layer Information
        layer_ptr%layer_type = "sum"
        layer_ptr%input_dim  = input_var0%dim
        layer_ptr%output_dim = output_dim
        layer_ptr%generation = maxval((/input_var0%generation, input_var1%generation/))+1
        layer_ptr%output_var => output_var


        ! Associate
        allocate(layer_ptr%input_var0)
        allocate(layer_ptr%input_var1)
        layer_ptr%input_var0 => input_var0
        layer_ptr%input_var1 => input_var1

        allocate(output_var%creator)
        output_var%generation = layer_ptr%generation
        output_var%creator => layer_ptr
    end subroutine add_sum_layer


    function new_variable(dim, generation)
        implicit none
        type(variable)            :: new_variable
        integer(kind=4), optional :: dim
        integer(kind=4), optional :: generation

        if (present(dim)) then
            new_variable%dim = dim
        end if

        if (present(generation)) then
            new_variable%generation = generation
        else
            new_variable%generation = 0
        end if
    end function new_variable


    subroutine info_variable(this)
        implicit none
        class(variable) :: this
        integer(kind=8) :: len_type_name
        print*, "****************************************************************"
        print*, "Dimension:    ", this%dim
        print*, "Generation:   ", this%generation
        if (ASSOCIATED(this%creator)) then
            print*, "Creator:      ", trim(this%creator%layer_type(1:)), this%creator%output_dim
        else
            print*, "Creator:      ", "NO CREATOR"
        end if
    end subroutine info_variable


    recursive subroutine plot_graph(this, is_first)
        implicit none
        class(variable) :: this
        logical(kind=1), OPTIONAL :: is_first
        type(variable_ptr), ALLOCATABLE, save :: var_ptr_arr(:)

        if (present(is_first)) then
            if (allocated(var_ptr_arr)) deallocate(var_ptr_arr)
            allocate(var_ptr_arr(0))
        end if

        call this%info()

        call this%creator%info()

    end subroutine plot_graph


    subroutine info_layer(this)
        implicit none
        class(layer) :: this
        integer(kind=8) :: len_type_name
        print*, "****************************************************************"
        print*, "InputDim:   ", this%input_dim
        print*, "OutputDim:  ", this%output_dim
        print*, "Generation: ", this%generation
        print*, "LayerType:  ", this%layer_type
    end subroutine info_layer



end module mod_multi_layer_perceptron
