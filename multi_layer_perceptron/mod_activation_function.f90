module mod_activation_function
    use mod_wengert_list
    implicit none

    public activation_function_

    type, abstract :: activation_function_
        character(len=256) :: act_name = "None"
    contains
        procedure :: set_activation_type_name
    end type activation_function_
    
contains

    subroutine set_activation_type_name(this, activation_type_name)
        implicit none
        class(activation_function_) :: this
        character(len=*), intent(in) :: activation_type_name
        this%act_name = activation_type_name
    end subroutine set_activation_type_name


end module mod_activation_function