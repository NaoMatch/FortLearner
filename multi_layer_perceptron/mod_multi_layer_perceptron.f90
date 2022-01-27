module mod_multi_layer_perceptron
    implicit none

    type variable
        integer(kind=4)           :: dim
        integer(kind=4)           :: generation
        logical(kind=1)           :: is_check = .false.
        type(layer), pointer :: creator => null()
    contains
        procedure :: info => info_variable
        procedure :: plot_graph
    end type variable

    type layer
        integer(kind=4)         :: input_dim
        integer(kind=4)         :: output_dim
        integer(kind=4)         :: generation
        character(len=12)       :: layer_type
        integer(kind=4)         :: len_layer_type
        type(variable), pointer :: input_var0 => null()
        type(variable), pointer :: input_var1 => null()
        type(variable), pointer :: output_var => null()
    contains
        procedure :: info => info_layer
    end type layer

    interface layer
        module procedure :: new_layer
    end interface layer

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

        type(variable), allocatable :: input_array(:)
        type(variable), allocatable :: output_array(:)
        type(layer), allocatable :: layer_array(:)
    contains
        procedure :: dense
        procedure :: add_layer
    end type model_builder

    interface model_builder
        module procedure :: new_model_builder
    end interface model_builder




contains

    function new_layer(layer_type, input_dim, output_dim)
        implicit none
        type(layer) :: new_layer
        character(len=*) :: layer_type
        integer(kind=4)  :: output_dim, input_dim

        new_layer%layer_type = layer_type
        new_layer%input_dim = input_dim
        new_layer%output_dim = output_dim
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

        if (allocated(new_model_builder%input_array)) deallocate(new_model_builder%input_array)
        allocate(new_model_builder%input_array(0))
        if (allocated(new_model_builder%output_array)) deallocate(new_model_builder%output_array)
        allocate(new_model_builder%output_array(0))
        if (allocated(new_model_builder%layer_array)) deallocate(new_model_builder%layer_array)
        allocate(new_model_builder%layer_array(0))
    end function new_model_builder


    function dense(this, input_var, input_dim, output_dim)
        implicit none
        class(model_builder)                  :: this
        type(variable), target, intent(inout) :: input_var
        type(variable), target                :: dense
        integer(kind=4), intent(in), optional :: input_dim, output_dim

        type(variable), pointer                :: output_var

        type(variable), target :: in_var_dense
        type(variable), target :: out_var_dense
        type(layer), target    :: lay_dense

        ! output_var => dense
        dense = variable()

        ! Shape Check
        if (present(input_dim)) then
            input_var%dim = input_dim
        end if

        if (present(output_dim)) then
            dense%dim = output_dim
        end if

        ! Layer Information
        lay_dense%input_dim = input_dim
        lay_dense%output_dim = output_dim
        lay_dense%layer_type = "dense"
        lay_dense%generation = input_var%generation+1

        ! Generation
        dense%generation = input_var%generation+1
        allocate(dense%creator)
        dense%creator => lay_dense

        this%input_array  = [this%input_array,  input_var]
        this%output_array = [this%output_array, dense]
        this%layer_array  = [this%layer_array,  lay_dense]

        ! Check
        call input_var%info()
        call dense%info()
        call lay_dense%info()
    end function dense


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
        print*, "VARIABLE: "
        print*, "    Dimension:    ", this%dim
        print*, "    Generation:   ", this%generation
        if (ASSOCIATED(this%creator)) then
            print*, "    Creator:      ", trim(this%creator%layer_type(1:)), this%creator%output_dim
        else
            print*, "    Creator:      ", "NO CREATOR"
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
        print*, "LAYER: "
        print*, "    InputDim:   ", this%input_dim
        print*, "    OutputDim:  ", this%output_dim
        print*, "    Generation: ", this%generation
        print*, "    LayerType:  ", this%layer_type
    end subroutine info_layer


    function add_layer(this, input_var0, input_var1, layer_type)
        implicit none
        class(model_builder)     :: this
        type(variable), target   :: input_var0
        type(variable), optional :: input_var1
        type(variable)           :: add_layer
        type(layer), target      :: layer_type

        allocate(add_layer%creator)
        add_layer%creator => layer_type

        allocate(layer_type%input_var0)
        input_var0%dim = layer_type%input_dim
        layer_type%input_var0 => input_var0

    end function add_layer



end module mod_multi_layer_perceptron
