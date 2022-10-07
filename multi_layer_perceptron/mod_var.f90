module mod_var
    ! use mod_activation_functions2
    implicit none

    type variable_variable
        real(kind=8)              :: s
        real(kind=8), allocatable :: v(:)
        real(kind=8), allocatable :: m(:,:)
        real(kind=8), allocatable :: t(:,:,:,:)
    end type variable_variable

    type variable
        real(kind=8), allocatable :: v(:,:)
        real(kind=8), allocatable :: g(:,:)
        real(kind=8), allocatable :: g_(:,:)
        ! type(activation_function), pointer :: creator_ptr
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

contains

    function new_variable_s(sclr)
        implicit none
        type(variable) :: new_variable_s
        real(kind=8), intent(in) :: sclr
        new_variable_s%v = reshape( (/sclr/), shape = [1,1] )
        ! nullify(new_variable_s%creator_ptr)
    end function new_variable_s
    
    function new_variable_m(mtrx)
        implicit none
        type(variable) :: new_variable_m
        real(kind=8), intent(in) :: mtrx(:,:)
        new_variable_m%v = mtrx(:,:)
        ! nullify(new_variable_m%creator_ptr)
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
        ! type(creator_ptr), allocatable :: creator_ptrs(:)
        ! type(creator_ptr) :: creator_ptr_tmp
        integer(kind=8) :: i
        
        ! if (associated(this%creator_ptr)) then
        !     ! print*, '*********************************************************************************************'
        !     ! print*, this%creator_ptr%act_name, size(this%creator_ptr%input_ptrs)
        !     ! call this%creator_ptr%backward(this%g)
        !     allocate(var_ptrs(0))
        !     ! var_ptrs = [var_ptrs, this%creator_ptr%input_ptrs]
        !     do i=1, size(var_ptrs), 1
        !         call var_ptrs(i)%ptr%backward()
        !     end do
        ! end if
    end subroutine backward_variable



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