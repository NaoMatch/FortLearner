module mod_multi_layer_perceptron
    implicit none

    type variable
        integer(kind=4)           :: dim
        integer(kind=4)           :: generation
        logical(kind=1)           :: is_check = .false.
        type(layer), pointer      :: creator => null()
        type(variable), pointer   :: grad => null()
        real(kind=8), allocatable :: x(:,:)
    contains
        procedure :: info => info_variable
    end type variable

    interface variable
        module procedure :: new_variable
    end interface variable


    type variable_ptr
        type(variable), pointer :: ptr
    end type variable_ptr


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
        procedure :: relu
        procedure :: dense_relu
    end type model_builder

    interface model_builder
        module procedure :: new_model_builder
    end interface model_builder


    type layer_stacker
    contains
        procedure :: hoge
    end type layer_stacker

contains

    subroutine hoge(this)
        implicit none
        class(layer_stacker) :: this
        print*, "hoge"
    end subroutine hoge

    function dense(this, input_var, input_dim, output_dim, layer_info)
        implicit none
        class(model_builder)        :: this
        type(variable), target      :: input_var
        integer(kind=4), intent(in) :: input_dim, output_dim
        type(variable), target      :: dense
        type(layer), target         :: layer_info

        layer_info%layer_type = "dense"
        layer_info%input_var0 => input_var
        layer_info%input_dim = input_dim
        layer_info%output_dim = output_dim

        dense%creator => layer_info
        dense%dim = output_dim

        layer_info%output_var => dense
    end function dense

    function relu(this, input_var, input_dim, output_dim, layer_info)
        implicit none
        class(model_builder)        :: this
        type(variable), target      :: input_var
        integer(kind=4), intent(in) :: input_dim, output_dim
        type(variable), target      :: relu
        type(layer), target         :: layer_info

        layer_info%layer_type = "relu"
        layer_info%input_var0 => input_var
        layer_info%input_dim = input_dim
        layer_info%output_dim = output_dim

        relu%creator => layer_info
        relu%dim = output_dim

        layer_info%output_var => relu
    end function relu

    recursive function dense_relu(this, input_var, input_dim, output_dim, layer_infos) result(out_var)
        implicit none
        class(model_builder)        :: this
        type(variable), target      :: input_var
        integer(kind=4), intent(in) :: input_dim, output_dim
        type(variable), target      :: out_var
        type(layer), target         :: layer_infos(2)

        type(variable), target      :: med_var

        med_var = this%dense(input_var, input_dim, output_dim, layer_infos(1))
        out_var = this%relu(med_var, output_dim, output_dim, layer_infos(2))
    end function dense_relu


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


    function new_variable(x)
        implicit none
        type(variable)         :: new_variable
        real(kind=8), optional :: x(:,:)

        if (present(x)) then
            new_variable%x = x
            new_variable%dim = size(x, dim=2)
        end if
        new_variable%generation = 0
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


    subroutine info_layer(this)
        implicit none
        class(layer) :: this
        integer(kind=8) :: len_type_name
        print*, "****************************************************************"
        print*, "LAYER: "
        print*, "    LayerType:  ", this%layer_type
        print*, "    InputDim:   ", this%input_dim
        print*, "    OutputDim:  ", this%output_dim
        print*, "    Generation: ", this%generation
    end subroutine info_layer


end module mod_multi_layer_perceptron
