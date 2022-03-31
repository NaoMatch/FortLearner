module mod_my_model
    use mod_variable
    use mod_base_model
    use mod_square
    use mod_sum
    implicit none


    type, extends(base_model) :: my_model
        type(variable), allocatable :: vars_out(:)

        type(variable) :: var00, var01, var02
        type(layer)    :: lyr00,    lyr01
        type(layer), pointer    :: lyr02
    contains
        procedure :: forward
    end type my_model

    interface my_model
        module procedure :: new_my_model
    end interface my_model
    
contains

    function new_my_model()
        implicit none
        type(my_model) :: new_my_model
        allocate(new_my_model%vars(0))
        allocate(new_my_model%lyrs(0))
        allocate(new_my_model%var_ptrs(0))
        allocate(new_my_model%lyr_ptrs(0))
    end function new_my_model

    function forward(this, x) result(res)
        implicit none
        class(my_model), target  :: this
        real(kind=8), intent(in) :: x(:,:)
        type(variable), target   :: res

        print*, 1
        this%var00 = variable(x)
        print*, this%var00%x

        print*, 2
        this%var01 = square_var_var(this%var00, this%lyr_ptrs)
        print*, this%var01%x

        print*, 3
        res = sum_var_var([this%var01, this%var01], this%lyr_ptrs)
        print*, res%x

        print*, '*********************************************************************************************'
        print*, '*********************************************************************************************'


        print*, ""
        print*, ""
        print*, ""
        ! print*, res%creator_ptr%ptr%vars_in(1)%creator_ptr%ptr%var_in%x(1,1)
        ! print*,  allocated(res%creator_ptr%ptr%vars_in(1)%x)
        print*, res%creator_ptr%ptr%var_out%x_shape
        print*, allocated(res%creator_ptr%ptr%var_out%x_shape)
        print*, associated(res%creator_ptr%ptr%var_out)
        print*, associated(res%creator_ptr%ptr)
        print*, associated(res%creator_ptr)
        print*, res%x
    end function forward


end module mod_my_model
