module mod_var
    ! use mod_activation_functions2
    implicit none

    type variable
        real(kind=8), allocatable :: v(:,:)
        real(kind=8), allocatable :: g(:,:)
        real(kind=8), allocatable :: g_(:,:)
        type(activation_function), pointer :: creator_ptr
        class(*), allocatable :: creator_tmp
    contains
        procedure :: backward => backward_variable
        procedure :: clear => clear_variable
    end type variable

    type variable_ptr
        type(variable), pointer :: ptr
    end type variable_ptr

    interface variable
        module procedure :: new_variable_s
        module procedure :: new_variable_m
    end interface variable

    type activation_function
        integer(kind=4) :: dim=-1
        character(len=256) :: act_name
        real(kind=8), allocatable :: w(:,:)
        real(kind=8), allocatable :: m(:,:) ! mask
        type(variable), pointer :: input_ptr
        type(variable), pointer :: input_ptr2
        type(variable_ptr), allocatable :: input_ptrs(:)
    contains
        ! ! Forward
        ! ! Elementary function
        ! procedure :: square
        ! procedure :: exponential
        ! ! Operations that perform four arithmetic operations
        ! procedure :: addition
        ! procedure :: substraction
        ! procedure :: multiply
        ! procedure :: division
        ! ! Operation to change the shape of the array
        ! procedure :: summation
        ! procedure :: broadcast
        ! ! Othres
        ! procedure :: absolute_value
        ! ! Backward
        ! procedure :: backward => backward_activation_funcion
    end type activation_function

    type creator_ptr
        type(activation_function), pointer :: ptr
    end type creator_ptr

contains

    function new_variable_s(sclr)
        implicit none
        type(variable) :: new_variable_s
        real(kind=8), intent(in) :: sclr
        new_variable_s%v = reshape( (/sclr/), shape = [1,1] )
        nullify(new_variable_s%creator_ptr)
    end function new_variable_s
    
    function new_variable_m(mtrx)
        implicit none
        type(variable) :: new_variable_m
        real(kind=8), intent(in) :: mtrx(:,:)
        new_variable_m%v = mtrx(:,:)
        nullify(new_variable_m%creator_ptr)
    end function new_variable_m

    recursive subroutine clear_variable(this)
        implicit none
        class(variable)   :: this
    end subroutine clear_variable

    recursive subroutine backward_variable(this)
        implicit none
        class(variable) :: this
        type(variable)  :: var
        type(variable_ptr), allocatable :: var_ptrs(:)
        type(creator_ptr), allocatable :: creator_ptrs(:)
        type(creator_ptr) :: creator_ptr_tmp
        integer(kind=8) :: i
        
        if (associated(this%creator_ptr)) then
            ! print*, '*********************************************************************************************'
            ! print*, this%creator_ptr%act_name, size(this%creator_ptr%input_ptrs)
            ! call this%creator_ptr%backward(this%g)
            allocate(var_ptrs(0))
            var_ptrs = [var_ptrs, this%creator_ptr%input_ptrs]
            do i=1, size(var_ptrs), 1
                call var_ptrs(i)%ptr%backward()
            end do
        end if
    end subroutine backward_variable



    ! function square(this, input_var) result(output_var)
    !     implicit none
    !     class(activation_function), target :: this
    !     type(variable), target :: input_var
    !     type(variable) :: output_var
    !     this%act_name = "square"
    !     output_var%creator_ptr => this
    !     output_var%creator_ptr%input_ptr => input_var
    !     if (allocated(output_var%creator_ptr%input_ptrs)) deallocate(output_var%creator_ptr%input_ptrs)
    !     allocate(output_var%creator_ptr%input_ptrs(1))
    !     output_var%creator_ptr%input_ptrs(1)%ptr => input_var
    !     call allocater_mat(output_var%v, shape(input_var%v))
    !     output_var%v = input_var%v**2d0
    ! end function square
    
    ! function exponential(this, input_var) result(output_var)
    !     implicit none
    !     class(activation_function), target :: this
    !     type(variable), target :: input_var
    !     type(variable) :: output_var
    !     this%act_name = "exponential"
    !     output_var%creator_ptr => this
    !     output_var%creator_ptr%input_ptr => input_var
    !     if (allocated(output_var%creator_ptr%input_ptrs)) deallocate(output_var%creator_ptr%input_ptrs)
    !     allocate(output_var%creator_ptr%input_ptrs(1))
    !     output_var%creator_ptr%input_ptrs(1)%ptr => input_var
    !     call allocater_mat(output_var%v, shape(input_var%v))
    !     output_var%v = exp(input_var%v)
    ! end function exponential

    ! function summation(this, input_var, dim) result(output_var)
    !     implicit none
    !     class(activation_function), target :: this
    !     type(variable), target :: input_var
    !     integer(kind=4), optional :: dim
    !     type(variable) :: output_var
    !     real(kind=8), allocatable :: v_sum(:,:)
    !     integer(kind=4) :: out_shape(2)
    !     this%act_name = "summation"
    !     output_var%creator_ptr => this
    !     output_var%creator_ptr%input_ptr => input_var
    !     if (allocated(output_var%creator_ptr%input_ptrs)) deallocate(output_var%creator_ptr%input_ptrs)
    !     allocate(output_var%creator_ptr%input_ptrs(1))
    !     output_var%creator_ptr%input_ptrs(1)%ptr => input_var
    !     if (present(dim)) then
    !         this%dim = dim
    !         if (dim>=3 .or. dim<=0) stop "dim -> 1<=dim<=2 or None."
    !         if (dim==1) then
    !             out_shape = [1, size(input_var%v(1,:))]
    !         else
    !             out_shape = [size(input_var%v(:,1)), 1]
    !         end if
    !         call allocater_mat(output_var%v, out_shape)
    !         output_var%v(:,:) = reshape(sum(input_var%v(:,:), dim=this%dim), shape=out_shape)
    !     else
    !         this%dim = -1
    !         call allocater_mat(output_var%v, [1,1])
    !         output_var%v = reshape( (/sum(input_var%v(:,:))/), shape = [1,1] )
    !     end if
    ! end function summation

    ! function multiply(this, input_var1, input_var2) result(output_var)
    !     implicit none
    !     class(activation_function), target :: this
    !     type(variable), target :: input_var1
    !     type(variable), target :: input_var2
    !     type(variable) :: output_var
    !     this%act_name = "multiply"
    !     output_var%creator_ptr => this
    !     output_var%creator_ptr%input_ptr => input_var1
    !     output_var%creator_ptr%input_ptr2 => input_var2
    !     if (allocated(output_var%creator_ptr%input_ptrs)) deallocate(output_var%creator_ptr%input_ptrs)
    !     allocate(output_var%creator_ptr%input_ptrs(2))
    !     output_var%creator_ptr%input_ptrs(1)%ptr => input_var1
    !     output_var%creator_ptr%input_ptrs(2)%ptr => input_var2
    !     call allocater_mat(output_var%v, shape(input_var1%v))
    !     output_var%v = input_var1%v*input_var2%v
    ! end function multiply

    ! function broadcast(this, input_var, dim, n_copies) result(output_var)
    !     implicit none
    !     class(activation_function), target :: this
    !     type(variable), target :: input_var
    !     type(variable) :: output_var
    !     integer(kind=4), intent(in) :: dim
    !     integer(kind=4), intent(in) :: n_copies
    !     integer(kind=4) :: n, out_shape(2)

        
    !     this%act_name = "broadcast"
    !     output_var%creator_ptr => this
    !     output_var%creator_ptr%input_ptr => input_var
    !     if (allocated(output_var%creator_ptr%input_ptrs)) deallocate(output_var%creator_ptr%input_ptrs)
    !     allocate(output_var%creator_ptr%input_ptrs(1))
    !     output_var%creator_ptr%input_ptrs(1)%ptr => input_var

    !     this%dim = dim
    !     if (dim>=3 .or. dim<=0) stop "dim -> 1<=dim<=2 or None."
    !     if (dim==1) then
    !         out_shape = [n_copies, size(input_var%v(1,:))]
    !     else
    !         out_shape = [size(input_var%v(:,1)), n_copies]
    !     end if
    !     call allocater_mat(output_var%v, out_shape)

    !     if (dim==1) then
    !         do n=1, n_copies, 1
    !             output_var%v(n,:) = input_var%v(1,:)
    !         end do    
    !     else
    !         do n=1, n_copies, 1
    !             output_var%v(:,n) = input_var%v(:,1)
    !         end do    
    !     end if
    ! end function broadcast

    ! function addition(this, input_var1, input_var2) result(output_var)
    !     implicit none
    !     class(activation_function), target :: this
    !     type(variable), target :: input_var1
    !     type(variable), target :: input_var2
    !     type(variable) :: output_var

    !     this%act_name = "addition"
    !     output_var%creator_ptr => this
    !     output_var%creator_ptr%input_ptr => input_var1
    !     output_var%creator_ptr%input_ptr2 => input_var2
    !     if (allocated(output_var%creator_ptr%input_ptrs)) deallocate(output_var%creator_ptr%input_ptrs)
    !     allocate(output_var%creator_ptr%input_ptrs(2))
    !     output_var%creator_ptr%input_ptrs(1)%ptr => input_var1
    !     output_var%creator_ptr%input_ptrs(2)%ptr => input_var2
    !     call allocater_mat(output_var%v, shape(input_var1%v))
    !     output_var%v = input_var1%v + input_var2%v
    ! end function addition

    ! function substraction(this, input_var1, input_var2) result(output_var)
    !     implicit none
    !     class(activation_function), target :: this
    !     type(variable), target :: input_var1
    !     type(variable), target :: input_var2
    !     type(variable) :: output_var

    !     this%act_name = "substraction"
    !     output_var%creator_ptr => this
    !     output_var%creator_ptr%input_ptr => input_var1
    !     output_var%creator_ptr%input_ptr2 => input_var2
    !     if (allocated(output_var%creator_ptr%input_ptrs)) deallocate(output_var%creator_ptr%input_ptrs)
    !     allocate(output_var%creator_ptr%input_ptrs(2))
    !     output_var%creator_ptr%input_ptrs(1)%ptr => input_var1
    !     output_var%creator_ptr%input_ptrs(2)%ptr => input_var2
    !     call allocater_mat(output_var%v, shape(input_var1%v))
    !     output_var%v = input_var1%v - input_var2%v
    ! end function substraction

    ! function division(this, input_var1, input_var2) result(output_var)
    !     implicit none
    !     class(activation_function), target :: this
    !     type(variable), target :: input_var1
    !     type(variable), target :: input_var2
    !     type(variable) :: output_var

    !     this%act_name = "division"
    !     output_var%creator_ptr => this
    !     output_var%creator_ptr%input_ptr => input_var1
    !     output_var%creator_ptr%input_ptr2 => input_var2
    !     if (allocated(output_var%creator_ptr%input_ptrs)) deallocate(output_var%creator_ptr%input_ptrs)
    !     allocate(output_var%creator_ptr%input_ptrs(2))
    !     output_var%creator_ptr%input_ptrs(1)%ptr => input_var1
    !     output_var%creator_ptr%input_ptrs(2)%ptr => input_var2
    !     call allocater_mat(output_var%v, shape(input_var1%v))
    !     output_var%v = input_var1%v / input_var2%v
    ! end function division

    ! function absolute_value(this, input_var) result(output_var)
    !     implicit none
    !     class(activation_function), target :: this
    !     type(variable), target :: input_var
    !     type(variable) :: output_var

    !     this%act_name = "absolute_value"
    !     output_var%creator_ptr => this
    !     output_var%creator_ptr%input_ptr => input_var
    !     if (allocated(output_var%creator_ptr%input_ptrs)) deallocate(output_var%creator_ptr%input_ptrs)
    !     allocate(output_var%creator_ptr%input_ptrs(1))
    !     output_var%creator_ptr%input_ptrs(1)%ptr => input_var
    !     call allocater_mat(output_var%v, shape(input_var%v))
    !     output_var%v = abs(input_var%v)
    !     call allocater_mat(output_var%creator_ptr%m, shape(input_var%v))
    !     call create_mask(output_var%creator_ptr%m, input_var%v, 0d0, -1d0)
    ! end function absolute_value


    ! subroutine backward_activation_funcion(this, grad_in)
    !     implicit none
    !     class(activation_function) :: this
    !     real(kind=8), intent(in) :: grad_in(:,:)
    !     integer(kind=8) :: i, j

    !     if (this%act_name == "square") then
    !         call allocater_mat(this%input_ptr%g, shape(this%input_ptr%v(:,:)))
    !         this%input_ptrs(1)%ptr%g = 2d0 * this%input_ptrs(1)%ptr%v(:,:) * grad_in(:,:)
    !     elseif (this%act_name == "exponential") then
    !         call allocater_mat(this%input_ptr%g, shape(this%input_ptr%v(:,:)))
    !         this%input_ptrs(1)%ptr%g = exp(this%input_ptrs(1)%ptr%v(:,:))  * grad_in(:,:)
    !     elseif (this%act_name == "multiply") then
    !         call allocater_mat(this%input_ptrs(1)%ptr%g, shape(this%input_ptr%v(:,:)))
    !         call allocater_mat(this%input_ptrs(2)%ptr%g, shape(this%input_ptr%v(:,:)))
    !         this%input_ptrs(1)%ptr%g = this%input_ptrs(2)%ptr%v(:,:)  * grad_in(:,:)
    !         this%input_ptrs(2)%ptr%g = this%input_ptrs(1)%ptr%v(:,:)  * grad_in(:,:)
    !     elseif (this%act_name == "substraction") then
    !         call allocater_mat(this%input_ptrs(1)%ptr%g, shape(this%input_ptr%v(:,:)))
    !         call allocater_mat(this%input_ptrs(2)%ptr%g, shape(this%input_ptr%v(:,:)))
    !         this%input_ptrs(1)%ptr%g = grad_in(:,:)
    !         this%input_ptrs(2)%ptr%g = -grad_in(:,:)
    !     elseif (this%act_name == "addition") then
    !         call allocater_mat(this%input_ptrs(1)%ptr%g, shape(this%input_ptr%v(:,:)))
    !         call allocater_mat(this%input_ptrs(2)%ptr%g, shape(this%input_ptr%v(:,:)))
    !         this%input_ptrs(1)%ptr%g = grad_in(:,:)
    !         this%input_ptrs(2)%ptr%g = grad_in(:,:)
    !     elseif (this%act_name == "division") then
    !         call allocater_mat(this%input_ptrs(1)%ptr%g, shape(this%input_ptr%v(:,:)))
    !         call allocater_mat(this%input_ptrs(2)%ptr%g, shape(this%input_ptr%v(:,:)))
    !         this%input_ptrs(1)%ptr%g = 1d0 / this%input_ptrs(2)%ptr%v * grad_in(:,:)
    !         this%input_ptrs(2)%ptr%g = -this%input_ptrs(1)%ptr%v / (this%input_ptrs(2)%ptr%v)**2d0 * grad_in(:,:)
    !     elseif (this%act_name == "absolute_value") then
    !         call allocater_mat(this%input_ptrs(1)%ptr%g, shape(this%input_ptr%v(:,:)))
    !         this%input_ptrs(1)%ptr%g = grad_in(:,:) * this%m(:,:)
    !     elseif (this%act_name == "broadcast") then
    !         call allocater_mat(this%input_ptrs(1)%ptr%g, [1, size(this%input_ptr%v(1,:))])
    !         this%input_ptrs(1)%ptr%g(1,:) = sum(grad_in(:,:), dim=1)
    !     elseif (this%act_name == "summation") then
    !         if (this%dim == -1) then
    !             this%input_ptrs(1)%ptr%g = ones_array_2d(shape(this%input_ptrs(1)%ptr%v)+0_8) * grad_in(1,1)
    !         elseif (this%dim == 1) then
    !             call allocater_mat(this%input_ptrs(1)%ptr%g, shape(this%input_ptrs(1)%ptr%v))
    !             do i=1, size(this%input_ptrs(1)%ptr%v(:,1)), 1
    !                 this%input_ptrs(1)%ptr%g(i,:) = grad_in(1,:)
    !             end do
    !         else ! this%dim == 2
    !             call allocater_mat(this%input_ptrs(1)%ptr%g, shape(this%input_ptrs(1)%ptr%v))
    !             do j=1, size(this%input_ptrs(1)%ptr%v(1,:)), 1
    !                 this%input_ptrs(1)%ptr%g(:,j) = grad_in(:,1)
    !             end do
    !         end if
    !     else
    !         print*, trim(this%act_name)
    !         stop "NotImplementedError."
    !     end if
    ! end subroutine backward_activation_funcion


    function ones_array_1d(arr_shape)
        implicit none
        integer(kind=8), intent(in) :: arr_shape(1) 
        real(kind=8), allocatable :: ones_array_1d(:)
        allocate(ones_array_1d(arr_shape(1)))
        ones_array_1d(:) = 1d0
    end function ones_array_1d

    function ones_array_2d(arr_shape)
        implicit none
        integer(kind=8), intent(in) :: arr_shape(2) 
        real(kind=8), allocatable :: ones_array_2d(:,:)
        allocate(ones_array_2d(arr_shape(1), arr_shape(2)))
        ones_array_2d(:,:) = 1d0
    end function ones_array_2d


    subroutine allocater_mat(mat, mat_shape)
        implicit none
        real(kind=8), allocatable :: mat(:,:)
        integer(kind=4), intent(in) :: mat_shape(2)
        if (allocated(mat)) deallocate(mat)
        allocate(mat(mat_shape(1), mat_shape(2)))
    end subroutine allocater_mat

    subroutine create_mask(m, v, threshold_val, default_val)
        implicit none
        real(kind=8), intent(inout) :: m(:,:)
        real(kind=8), intent(in) :: v(:,:)
        real(kind=8), intent(in) :: threshold_val
        real(kind=8), intent(in) :: default_val
        integer(kind=8) :: i, j, m_shape(2)

        m_shape = shape(m)
        m(:,:) = default_val

        do j=1, m_shape(2)
            do i=1, m_shape(1)
                if (v(i,j)>=threshold_val) m(i,j) = 1d0
            end do
        end do
    end subroutine create_mask


end module mod_var