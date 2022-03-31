module mod_neural_network_utils
    use mod_variable
    implicit none



contains

    subroutine allocate_new_layer(layers, new_layer, idx_new_lyr)
        implicit none
        type(layer_ptr), allocatable, target :: layers(:)
        integer(kind=8), intent(inout)       :: idx_new_lyr
        type(layer_ptr), target       :: new_layer

        idx_new_lyr = size(layers(:))+1
        layers = [layers, new_layer]
        allocate(layers(idx_new_lyr)%ptr)
    end subroutine allocate_new_layer

    subroutine is_multi_inout(layers, idx_new_lyr, is_multi_in, is_multi_out)
        implicit none
        type(layer_ptr), allocatable, target :: layers(:)
        integer(kind=8), intent(in)          :: idx_new_lyr
        logical(kind=4), intent(in)          :: is_multi_in, is_multi_out
        layers(idx_new_lyr)%ptr%is_multi_input  = is_multi_in
        layers(idx_new_lyr)%ptr%is_multi_output = is_multi_out
    end subroutine is_multi_inout

    subroutine set_layer_inout_shape(layers, idx_new_lyr, in_shape, out_shape)
        implicit none
        type(layer_ptr), allocatable, target :: layers(:)
        integer(kind=8), intent(in)          :: idx_new_lyr
        integer(kind=4), intent(in)          :: in_shape(:), out_shape(:)

        allocate(layers(idx_new_lyr)%ptr%in_shape(size(in_shape)))
        allocate(layers(idx_new_lyr)%ptr%out_shape(size(out_shape)))
        layers(idx_new_lyr)%ptr%in_shape  = in_shape(:)
        layers(idx_new_lyr)%ptr%out_shape = out_shape(:)
    end subroutine set_layer_inout_shape

    subroutine set_layer_type(layers, idx_new_lyr, layer_type)
        implicit none
        type(layer_ptr), allocatable, target :: layers(:)
        integer(kind=8), intent(in)          :: idx_new_lyr
        character(len=*), intent(in)         :: layer_type

        layers(idx_new_lyr)%ptr%layer_type = layer_type
    end subroutine set_layer_type


end module mod_neural_network_utils
