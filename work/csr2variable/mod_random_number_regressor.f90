module mod_random_number_regressor
    use mod_variable
    use mod_intrinsics
    use mod_functions
    use mod_layers
    implicit none

    type, extends(base_model) :: random_number_regressor
        type(linear), allocatable :: linears(:)
    contains
        procedure :: forward => forward_random_number_regressor
    end type random_number_regressor

    interface random_number_regressor
        module procedure :: new_random_number_regressor
    end interface random_number_regressor
    
contains

    function new_random_number_regressor() result(model)
        implicit none
        type(random_number_regressor) :: model

        allocate(model%linears(3))
        model%linears(1) = linear(out_size=512_8, no_bias=.false.)
        model%linears(2) = linear(out_size=256_8, no_bias=.false.)
        model%linears(3) = linear(out_size=16_8,  no_bias=.false.)
    end function new_random_number_regressor


    function forward_random_number_regressor(this, var_in) result(var_out)
        implicit none
        class(random_number_regressor) :: this
        type(variable) :: var_in
        type(variable) :: var_out
        integer(kind=8)        :: date_value1(8), date_value2(8)
        
        var_out = this%linears(1)%act(var_in)
        print*, '*********************************************************************************************'
        call date_and_time(values=date_value1)
        var_out = relu_(var_out)
        var_out = this%linears(2)%act(var_out)
        call date_and_time(values=date_value2)
        print*, "relu -> linear", time_diff(date_value1, date_value2)
        var_out = relu_(var_out)
        var_out = this%linears(3)%act(var_out)
    end function forward_random_number_regressor
    
end module mod_random_number_regressor