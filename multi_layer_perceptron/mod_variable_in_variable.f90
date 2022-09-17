module mod_variable_in_variable
    use mod_const
    implicit none

    type variable_in_variable
        real(kind=8)              :: s=huge(0d0)
        real(kind=8), allocatable :: v(:)
        real(kind=8), allocatable :: m(:,:)
        integer(kind=4) :: dtype=-1
    contains
        procedure :: batch_sizes => batch_sizes_viv
        procedure :: dims
        procedure :: compare_shape
        procedure :: clear_all
        procedure :: allocate_check
    end type variable_in_variable


    interface variable_in_variable
        module procedure :: new_variable_in_variable_s
        module procedure :: new_variable_in_variable_v
        module procedure :: new_variable_in_variable_m
    end interface variable_in_variable

    interface assignment(=)
        module procedure assign_s
        module procedure assign_v
        module procedure assign_m
    end interface

    interface operator(+)
        module procedure :: addition_vv_vv
        module procedure :: addition_vv_m
        module procedure :: addition_vv_v
        module procedure :: addition_vv_s
        module procedure :: addition_m_vv
        module procedure :: addition_v_vv
        module procedure :: addition_s_vv
    end interface operator(+)

    interface operator(-)
        module procedure :: substraction_vv_vv
        module procedure :: substraction_vv_m
        module procedure :: substraction_vv_v
        module procedure :: substraction_vv_s
        module procedure :: substraction_m_vv
        module procedure :: substraction_v_vv
        module procedure :: substraction_s_vv
    end interface operator(-)

    interface operator(*)
        module procedure :: multiplication_vv_vv
        module procedure :: multiplication_vv_m
        module procedure :: multiplication_vv_v
        module procedure :: multiplication_vv_s
        module procedure :: multiplication_m_vv
        module procedure :: multiplication_v_vv
        module procedure :: multiplication_s_vv
    end interface operator(*)

    interface operator(/)
        module procedure :: division_vv_vv
        module procedure :: division_vv_m
        module procedure :: division_vv_v
        module procedure :: division_vv_s
        module procedure :: division_m_vv
        module procedure :: division_v_vv
        module procedure :: division_s_vv        
    end interface operator(/)

    interface operator(**)
        module procedure :: power_vv_r4
        module procedure :: power_vv_r8
        module procedure :: power_vv_i4
        module procedure :: power_vv_i8
        module procedure :: power_vv_vv
    end interface operator(**)

    interface abs
        module procedure abs_viv
    end interface abs

    interface ceiling
        module procedure ceiling_viv
    end interface ceiling

    interface floor
        module procedure floor_viv
    end interface floor

    interface acos
        module procedure acos_viv
    end interface acos

    interface asin
        module procedure asin_viv
    end interface asin

    interface atan
        module procedure atan_viv
    end interface atan

    interface cos
        module procedure cos_viv
    end interface cos

    interface cosh
        module procedure cosh_viv
    end interface cosh

    interface exp
        module procedure exp_viv
    end interface exp

    interface log
        module procedure log_viv
    end interface log

    interface log10
        module procedure log10_viv
    end interface log10

    interface sin
        module procedure sin_viv
    end interface sin

    interface sinh
        module procedure sinh_viv
    end interface sinh

    interface sqrt
        module procedure sqrt_viv
    end interface sqrt

    interface tan
        module procedure tan_viv
    end interface tan

    interface tanh
        module procedure tanh_viv
    end interface tanh

    interface matmul
        module procedure matmul_viv_viv
        module procedure matmul_viv_viv_trans
    end interface matmul

    interface sum
        module procedure sum_viv
    end interface sum

    interface transpose
        module procedure transpose_viv
    end interface transpose

    interface shape
        module procedure shape_viv
    end interface shape
            
    interface spread
        module procedure spread_viv_scalar_to_matrix
        module procedure spread_viv
    end interface spread

    interface relu
        module procedure relu_viv
    end interface relu

    interface mask_one
        module procedure mask_one_viv
    end interface mask_one

contains

    elemental function mask_one_elemental(x)
        implicit none
        real(kind=8), intent(in) :: x
        real(kind=8) :: mask_one_elemental
        mask_one_elemental = maxval([0d0, x]) / x
    end function mask_one_elemental
            
    function mask_one_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable) :: res
        if (viv%dtype==2) then
            res = mask_one_elemental(viv%m)
        elseif (viv%dtype==1) then
            res = mask_one_elemental(viv%v)
        else
            res = mask_one_elemental(viv%s)
        end if
    end function mask_one_viv

    elemental function relu_elemental(x)
        implicit none
        real(kind=8), intent(in) :: x
        real(kind=8) :: relu_elemental
        relu_elemental = maxval([0d0, x])
    end function relu_elemental
            
    function relu_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable) :: res
        if (viv%dtype==2) then
            res = relu_elemental(viv%m)
        elseif (viv%dtype==1) then
            res = relu_elemental(viv%v)
        else
            res = relu_elemental(viv%s)
        end if
    end function relu_viv

    function matmul_viv_viv_trans(viv1, viv2, trans1, trans2) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv1, viv2
        logical(kind=4), intent(in) :: trans1, trans2
        type(variable_in_variable)  :: res

        integer(kind=8) :: shape1(2), shape2(2), shape_res(2)
        character(len=1) :: transA, transB

        shape1 = shape(viv1)
        shape2 = shape(viv2)

        res%dtype = 2
        if (trans1 .and. trans2) then
            ! print*, "1", __FILE__, __LINE__
            allocate(res%m(shape1(2), shape2(1)))
            call dgemm("T", "T", & 
                shape1(2), shape2(1), shape1(1), &
                1d0, & 
                viv1%m, shape1(1), &
                viv2%m, shape2(1), &
                0d0, &
                res%m, shape1(2))
        elseif (.not. trans1 .and. trans2) then
            ! print*, "2", __FILE__, __LINE__
            allocate(res%m(shape1(1), shape2(1)))
            res%m = matmul(viv1%m, transpose(viv2%m))
            call dgemm("N", "T", & 
                shape1(1), shape2(1), shape1(2), &
                1d0, & 
                viv1%m(:,:), shape1(1), &
                viv2%m(:,:), shape2(1), &
                0d0, &
                res%m(:,:), shape1(1))
            
        elseif (trans1 .and. .not. trans2) then
            ! print*, "3", __FILE__, __LINE__
            allocate(res%m(shape1(2), shape2(2)))
            call dgemm("T", "N", & 
                shape1(2), shape2(2), shape1(1), &
                1d0, & 
                viv1%m, shape1(1), &
                viv2%m, shape2(1), &
                0d0, &
                res%m, shape1(2))
        elseif (.not. trans1 .and. .not. trans2) then
            ! print*, "4", __FILE__, __LINE__
            allocate(res%m(shape1(1), shape2(2)))
            call dgemm("N", "N", & 
                shape1(1), shape2(2), shape1(2), &
                1d0, & 
                viv1%m, shape1(1), &
                viv2%m, shape2(1), &
                0d0, &
                res%m, shape1(1))
        end if
    end function matmul_viv_viv_trans


    function matmul_viv_viv(viv1, viv2) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv1, viv2
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv1, dtype_viv2

        dtype_viv1 = viv1%dtype
        dtype_viv2 = viv2%dtype
        if (dtype_viv1==2 .and. dtype_viv2==2) then
            res%dtype = viv1%dtype
            res%m = matmul(viv1%m, viv2%m)
        else
            print '(A)', "Input Shape Mismatch in " // __FILE__ // "."
            stop 
        end if
    end function matmul_viv_viv

    function spread_viv_scalar_to_matrix(viv, output_shape) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        integer(kind=8), intent(in) :: output_shape(2)
        type(variable_in_variable) :: res

        if (viv%dtype==0) then
            res%dtype=2
            allocate(res%m(output_shape(1), output_shape(2)))
            res%m = viv%s
        else
            print*, __FILE__, __LINE__, " in spread_viv_scalar_to_matrix."
            stop 
        end if
    end function spread_viv_scalar_to_matrix

    function spread_viv(viv, dim, ncopies) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        integer(kind=8), intent(in) :: dim, ncopies
        type(variable_in_variable) :: res

        if (viv%dtype==1) then
            res = spread(viv%v, dim=dim, ncopies=ncopies)
        elseif (viv%dtype==0) then
            res = spread(viv%s, dim=dim, ncopies=ncopies)
        end if
    end function spread_viv

    ! Shape
    function shape_viv(viv) result(res)
        implicit none
        type(variable_in_variable) :: viv
        integer(kind=8), allocatable :: res(:)
        if (viv%dtype==2) then
            res = shape(viv%m)
        elseif (viv%dtype==1) then
            res = shape(viv%v)
        else
            allocate(res(0))
        end if
    end function shape_viv

    subroutine allocate_check(this)
        implicit none
        class(variable_in_variable) :: this
        if (this%dtype==-1) then
            print'(A)', "Is not allocated to variable in " // __FILE__ // "."
            stop
        end if
    end subroutine allocate_check

    function transpose_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable) :: res
        
        if (viv%dtype==2) then
            res%dtype = 2
            allocate(res%m, source=transpose(viv%m))
            res%m = transpose(viv%m)
        else
            call viv%allocate_check()
        end if
    end function 

    subroutine clear_all(this)
        implicit none
        class(variable_in_variable) :: this
        this%dtype=-1
        if (allocated(this%m)) deallocate(this%m)
        if (allocated(this%v)) deallocate(this%v)
    end subroutine clear_all

    function batch_sizes_viv(this) result(n_batches)
        implicit none
        class(variable_in_variable) :: this
        integer(kind=8) :: n_batches

        if (this%dtype==2) then
            n_batches = size(this%m(:,1))
        elseif (this%dtype==1) then
            n_batches = size(this%v(:))
        else
            print*, __FILE__, __LINE__
            stop "Not Implemented Error in batch_sizes_viv."
        end if
    end function batch_sizes_viv


    function sum_viv(viv, dim) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        integer(kind=8), intent(in), optional :: dim
        type(variable_in_variable) :: res

        if (viv%dtype==2) then
            if (present(dim)) then
                res = sum(viv%m, dim=dim)
            else
                res = sum(viv%m)
            end if
        elseif (viv%dtype==1) then
            res = sum(viv%v)
        else
            stop "NOT IMPLEMENTED ERROR, sum_viv."
        end if
    end function sum_viv


    function tanh_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = tanh(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = tanh(viv%v)
        elseif (dtype_viv==0) then
            res%s = tanh(viv%s)
        end if
    end function tanh_viv

    function tan_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = tan(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = tan(viv%v)
        elseif (dtype_viv==0) then
            res%s = tan(viv%s)
        end if
    end function tan_viv

    function sqrt_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = sqrt(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = sqrt(viv%v)
        elseif (dtype_viv==0) then
            res%s = sqrt(viv%s)
        end if
    end function sqrt_viv

    function sinh_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = sinh(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = sinh(viv%v)
        elseif (dtype_viv==0) then
            res%s = sinh(viv%s)
        end if
    end function sinh_viv

    function sin_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = sin(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = sin(viv%v)
        elseif (dtype_viv==0) then
            res%s = sin(viv%s)
        end if
    end function sin_viv

    function log10_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = log10(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = log10(viv%v)
        elseif (dtype_viv==0) then
            res%s = log10(viv%s)
        end if
    end function log10_viv

    function log_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = log(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = log(viv%v)
        elseif (dtype_viv==0) then
            res%s = log(viv%s)
        end if
    end function log_viv

    function exp_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = exp(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = exp(viv%v)
        elseif (dtype_viv==0) then
            res%s = exp(viv%s)
        end if
    end function exp_viv

    function cosh_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = cosh(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = cosh(viv%v)
        elseif (dtype_viv==0) then
            res%s = cosh(viv%s)
        end if
    end function cosh_viv

    function cos_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = cos(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = cos(viv%v)
        elseif (dtype_viv==0) then
            res%s = cos(viv%s)
        end if
    end function cos_viv

    function atan_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = atan(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = atan(viv%v)
        elseif (dtype_viv==0) then
            res%s = atan(viv%s)
        end if
    end function atan_viv

    function asin_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = asin(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = asin(viv%v)
        elseif (dtype_viv==0) then
            res%s = asin(viv%s)
        end if
    end function asin_viv

    function acos_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = acos(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = acos(viv%v)
        elseif (dtype_viv==0) then
            res%s = acos(viv%s)
        end if
    end function acos_viv

    function floor_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = floor(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = floor(viv%v)
        elseif (dtype_viv==0) then
            res%s = floor(viv%s)
        end if
    end function floor_viv

    function ceiling_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = ceiling(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = ceiling(viv%v)
        elseif (dtype_viv==0) then
            res%s = ceiling(viv%s)
        end if
    end function ceiling_viv

    function abs_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = abs(viv%m)
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = abs(viv%v)
        elseif (dtype_viv==0) then
            res%s = abs(viv%s)
        end if
    end function abs_viv


    ! Assignment Overload
    subroutine assign_s(viv, s)
        implicit none
        type(variable_in_variable), intent(inout) :: viv
        real(kind=8), intent(in) :: s
        viv = variable_in_variable(s)
    end subroutine assign_s
    
    subroutine assign_v(viv, v)
        implicit none
        type(variable_in_variable), intent(inout) :: viv
        real(kind=8), intent(in) :: v(:)
        viv = variable_in_variable(v)
    end subroutine assign_v
    
    subroutine assign_m(viv, m)
        implicit none
        type(variable_in_variable), intent(inout) :: viv
        real(kind=8), intent(in) :: m(:,:)
        viv = variable_in_variable(m)
    end subroutine assign_m



    ! Create Object
    function new_variable_in_variable_s(s) result(output)
        implicit none
        real(kind=8), intent(in) :: s
        type(variable_in_variable)  :: output
        output%s = s
        output%dtype = 0
    end function 

    function new_variable_in_variable_v(v) result(output)
        implicit none
        real(kind=8), intent(in) :: v(:)
        type(variable_in_variable)  :: output
        allocate(output%v, source=v)
        output%v = v
        output%dtype = 1
    end function 
    
    function new_variable_in_variable_m(m) result(output)
        implicit none
        real(kind=8), intent(in) :: m(:,:)
        type(variable_in_variable)  :: output
        allocate(output%m, source=m)
        output%m = m
        output%dtype = 2
    end function 



    ! Dim
    function dims(this)
        implicit none
        class(variable_in_variable) :: this
        integer(kind=8) :: dims
        dims = 0
        if (allocated(this%m)) dims = 2
        if (allocated(this%v)) dims = 1
    end function dims

    ! Shape Comparison
    function compare_shape(this, that) result(is_same)
        implicit none
        class(variable_in_variable) :: this
        type(variable_in_variable) :: that
        logical(kind=4) :: is_same

        integer(kind=8), allocatable :: shape_this(:), shape_that(:)
        integer(kind=8) :: dim_this, dim_that

        shape_this = shape(this); dim_this = this%dtype
        shape_that = shape(that); dim_that = that%dtype

        if (size(shape_this) == 0 .and. size(shape_this) == 0) then
            is_same = .true.
        elseif (dim_this /= dim_that) then
            is_same = .false.
        elseif (dim_this == dim_that) then
            if (all(shape_this == shape_that)) then
                is_same = .true.
            else
                is_same = .false.
            end if
        end if
    end function compare_shape

    ! -------------------------------------------------------------------------
    ! -------------------------------------------------------------------------
    ! Power Overload
    function power_vv_i8(viv, c) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        integer(kind=8), intent(in) :: c
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = viv%m**c
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = viv%v**c
        elseif (dtype_viv==0) then
            res%s = viv%s**c
        end if
    end function power_vv_i8

    function power_vv_i4(viv, c) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        integer(kind=4), intent(in) :: c
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = viv%m**c
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = viv%v**c
        elseif (dtype_viv==0) then
            res%s = viv%s**c
        end if
    end function power_vv_i4

    function power_vv_r8(viv, c) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        real(kind=8), intent(in) :: c
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = viv%m**c
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = viv%v**c
        elseif (dtype_viv==0) then
            res%s = viv%s**c
        end if
    end function power_vv_r8

    function power_vv_r4(viv, c) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        real(kind=4), intent(in) :: c
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv

        dtype_viv = viv%dtype
        res%dtype = viv%dtype
        if (dtype_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = viv%m**c
        elseif (dtype_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = viv%v**c
        elseif (dtype_viv==0) then
            res%s = viv%s**c
        end if
    end function power_vv_r4

    function power_vv_vv(viv1, viv2) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv1, viv2
        type(variable_in_variable)  :: res

        integer(kind=8) :: dtype_viv1, dtype_viv2

        dtype_viv1 = viv1%dtype
        dtype_viv2 = viv2%dtype
        res%dtype = viv1%dtype
        if (dtype_viv2 /= 0) stop "power_vv_vv viv2 scalar."

        if (dtype_viv1==2) then
            allocate(res%m, source=viv1%m)
            res%m = viv1%m**viv2%s
        elseif (dtype_viv1==1) then
            allocate(res%v, source=viv1%v)
            res%v = viv1%v**viv2%s
        elseif (dtype_viv1==0) then
            res%s = viv1%s**viv2%s
        end if
    end function power_vv_vv


    ! -------------------------------------------------------------------------
    ! -------------------------------------------------------------------------
    ! Addition Overload
    function addition_vv_vv(v1, v2) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1, v2
        type(variable_in_variable) :: res

        integer(kind=8) :: dim_v1, dim_v2
        integer(kind=8) :: dim1
        integer(kind=8), allocatable :: shape_v1(:), shape_v2(:)

        dim_v1 = v1%dtype
        dim_v2 = v2%dtype

        shape_v1 = shape(v1)
        shape_v2 = shape(v2)

        res%dtype = maxval([v1%dtype, v2%dtype])
        if (dim_v1 == dim_v2) then
            ! No Broad Cast
            if (dim_v1 == 0) then
                res%s = v1%s + v2%s
            elseif (all(shape_v1 == shape_v2)) then
                if (dim_v1 == 1) then
                    allocate(res%v, source=v1%v)
                    res%v = v1%v + v2%v
                elseif (dim_v2 == 2) then
                    allocate(res%m, source=v1%m)
                    res%m = v1%m + v2%m
                end if
            else
                stop "Same Rank but, different input shape."
            end if

        elseif (dim_v1==0) then
            ! v1=scalar and v2=vector/matrix/tensor
            if (dim_v2==1) then
                allocate(res%v, source=v2%v)
                res%v = v1%s + v2%v
            elseif (dim_v2==2) then
                allocate(res%m, source=v2%m)
                res%m = v1%s + v2%m
            end if

        elseif (dim_v2==0) then
            ! v1=vector/matrix/tensor and v2=scalar
            if (dim_v1==1) then
                allocate(res%v, source=v1%v)
                res%v = v1%v + v2%s
            elseif (dim_v1==2) then
                allocate(res%m, source=v1%m)
                res%m = v1%m + v2%s
            end if

        elseif (dim_v1==1 .and. dim_v2==2) then
            if (shape_v1(1) == shape_v2(2)) then
                allocate(res%m, source=v2%m)
                do dim1=1, shape_v2(1), 1
                    res%m(dim1, :) = v1%v(:) + v2%m(dim1, :)
                end do
            end if

        elseif (dim_v1==2 .and. dim_v2==1) then
            if (shape_v1(2) == shape_v2(1)) then
                allocate(res%m, source=v1%m)
                do dim1=1, shape_v1(1), 1
                    res%m(dim1, :) = v1%m(dim1, :) + v2%v(:)
                end do
            end if

        else
            stop "Shape Mismatch in addition."
        end if
    end function addition_vv_vv
    
    function addition_vv_m(v1, m) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: m(:,:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(m)
        res = addition_vv_vv(v1, v2)
    end function addition_vv_m
    
    function addition_vv_v(v1, v) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: v(:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(v)
        res = addition_vv_vv(v1, v2)
    end function addition_vv_v
    
    function addition_vv_s(v1, s) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: s
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(s)
        res = addition_vv_vv(v1, v2)
    end function addition_vv_s
    
    function addition_m_vv(m, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: m(:,:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(m)
        res = addition_vv_vv(v1, v2)
    end function addition_m_vv
    
    function addition_v_vv(v, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: v(:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(v)
        res = addition_vv_vv(v1, v2)
    end function addition_v_vv
    
    function addition_s_vv(s, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: s
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(s)
        res = addition_vv_vv(v1, v2)
    end function addition_s_vv

    ! -------------------------------------------------------------------------
    ! -------------------------------------------------------------------------
    ! Substraction Overload    
    function substraction_vv_vv(v1, v2) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1, v2
        type(variable_in_variable) :: res

        integer(kind=8) :: dim_v1, dim_v2
        integer(kind=8) :: dim1
        integer(kind=8), allocatable :: shape_v1(:), shape_v2(:)

        dim_v1 = v1%dtype
        dim_v2 = v2%dtype

        shape_v1 = shape(v1)
        shape_v2 = shape(v2)

        res%dtype = maxval([v1%dtype, v2%dtype])
        if (dim_v1 == dim_v2) then
            ! No Broad Cast
            if (dim_v1 == 0) then
                res%s = v1%s - v2%s
            elseif (all(shape_v1 == shape_v2)) then
                if (dim_v1 == 1) then
                    allocate(res%v, source=v1%v)
                    res%v = v1%v - v2%v
                elseif (dim_v2 == 2) then
                    allocate(res%m, source=v1%m)
                    res%m = v1%m - v2%m
                end if
            else
                stop "Same Rank but, different input shape."
            end if

        elseif (dim_v1==0) then
            ! v1=scalar and v2=vector/matrix/tensor
            if (dim_v2==1) then
                allocate(res%v, source=v2%v)
                res%v = v1%s - v2%v
            elseif (dim_v2==2) then
                allocate(res%m, source=v2%m)
                res%m = v1%s - v2%m
            end if

        elseif (dim_v2==0) then
            ! v1=vector/matrix/tensor and v2=scalar
            if (dim_v1==1) then
                allocate(res%v, source=v1%v)
                res%v = v1%v - v2%s
            elseif (dim_v1==2) then
                allocate(res%m, source=v1%m)
                res%m = v1%m - v2%s
            end if

        elseif (dim_v1==1 .and. dim_v2==2) then
            if (shape_v1(1) == shape_v2(2)) then
                allocate(res%m, source=v2%m)
                do dim1=1, shape_v2(1), 1
                    res%m(dim1, :) = v1%v(:) - v2%m(dim1, :)
                end do
            end if

        elseif (dim_v1==2 .and. dim_v2==1) then
            if (shape_v1(2) == shape_v2(1)) then
                allocate(res%m, source=v1%m)
                do dim1=1, shape_v1(1), 1
                    res%m(dim1, :) = v1%m(dim1, :) - v2%v(:)
                end do
            end if
        else
            stop "Shape Mismatch in substraction."
        end if
    end function substraction_vv_vv
    
    function substraction_vv_m(v1, m) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: m(:,:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(m)
        res = substraction_vv_vv(v1, v2)
    end function substraction_vv_m
    
    function substraction_vv_v(v1, v) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: v(:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(v)
        res = substraction_vv_vv(v1, v2)
    end function substraction_vv_v
    
    function substraction_vv_s(v1, s) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: s
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(s)
        res = substraction_vv_vv(v1, v2)
    end function substraction_vv_s
    
    function substraction_m_vv(m, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: m(:,:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(m)
        res = substraction_vv_vv(v1, v2)
    end function substraction_m_vv
    
    function substraction_v_vv(v, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: v(:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(v)
        res = substraction_vv_vv(v1, v2)
    end function substraction_v_vv
    
    function substraction_s_vv(s, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: s
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(s)
        res = substraction_vv_vv(v1, v2)
    end function substraction_s_vv

    ! -------------------------------------------------------------------------
    ! -------------------------------------------------------------------------
    ! Multiplication Overload    
    function multiplication_vv_vv(v1, v2) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1, v2
        type(variable_in_variable) :: res

        integer(kind=8) :: dim_v1, dim_v2
        integer(kind=8) :: dim1
        integer(kind=8), allocatable :: shape_v1(:), shape_v2(:)

        dim_v1 = v1%dtype
        dim_v2 = v2%dtype

        shape_v1 = shape(v1)
        shape_v2 = shape(v2)

        res%dtype = maxval([v1%dtype, v2%dtype])
        if (dim_v1 == dim_v2) then
            ! No Broad Cast
            if (dim_v1 == 0) then
                res%s = v1%s * v2%s
            elseif (all(shape_v1 == shape_v2)) then
                if (dim_v1 == 1) then
                    allocate(res%v, source=v1%v)
                    res%v = v1%v * v2%v
                elseif (dim_v2 == 2) then
                    allocate(res%m, source=v1%m)
                    res%m = v1%m * v2%m
                end if
            else
                stop "Same Rank but, different input shape."
            end if

        elseif (dim_v1==0) then
            ! v1=scalar and v2=vector/matrix/tensor
            if (dim_v2==1) then
                allocate(res%v, source=v2%v)
                res%v = v1%s * v2%v
            elseif (dim_v2==2) then
                allocate(res%m, source=v2%m)
                res%m = v1%s * v2%m
            end if

        elseif (dim_v2==0) then
            ! v1=vector/matrix/tensor and v2=scalar
            if (dim_v1==1) then
                allocate(res%v, source=v1%v)
                res%v = v1%v * v2%s
            elseif (dim_v1==2) then
                allocate(res%m, source=v1%m)
                res%m = v1%m * v2%s
            end if

        elseif (dim_v1==1 .and. dim_v2==2) then
            if (shape_v1(1) == shape_v2(2)) then
                allocate(res%m, source=v2%m)
                do dim1=1, shape_v2(1), 1
                    res%m(dim1, :) = v1%v(:) * v2%m(dim1, :)
                end do
            end if

        elseif (dim_v1==2 .and. dim_v2==1) then
            if (shape_v1(2) == shape_v2(1)) then
                allocate(res%m, source=v1%m)
                do dim1=1, shape_v1(1), 1
                    res%m(dim1, :) = v1%m(dim1, :) * v2%v(:)
                end do
            end if
        else
            stop "Shape Mismatch in multiplication."
        end if
    end function multiplication_vv_vv
    
    function multiplication_vv_m(v1, m) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: m(:,:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(m)
        res = multiplication_vv_vv(v1, v2)
    end function multiplication_vv_m
    
    function multiplication_vv_v(v1, v) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: v(:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(v)
        res = multiplication_vv_vv(v1, v2)
    end function multiplication_vv_v
    
    function multiplication_vv_s(v1, s) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: s
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(s)
        res = multiplication_vv_vv(v1, v2)
    end function multiplication_vv_s
    
    function multiplication_m_vv(m, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: m(:,:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(m)
        res = multiplication_vv_vv(v1, v2)
    end function multiplication_m_vv
    
    function multiplication_v_vv(v, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: v(:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(v)
        res = multiplication_vv_vv(v1, v2)
    end function multiplication_v_vv
    
    function multiplication_s_vv(s, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: s
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(s)
        res = multiplication_vv_vv(v1, v2)
    end function multiplication_s_vv

    ! -------------------------------------------------------------------------
    ! -------------------------------------------------------------------------
    ! Division Overload    
    function division_vv_vv(v1, v2) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1, v2
        type(variable_in_variable) :: res

        integer(kind=8) :: dim_v1, dim_v2
        integer(kind=8) :: dim1, err_line
        integer(kind=8), allocatable :: shape_v1(:), shape_v2(:)

        dim_v1 = v1%dtype
        dim_v2 = v2%dtype

        shape_v1 = shape(v1)
        shape_v2 = shape(v2)

        res%dtype = maxval([v1%dtype, v2%dtype])
        if (dim_v1 == dim_v2) then
            ! No Broad Cast
            if (dim_v1 == 0) then
                res%s = v1%s / v2%s
            elseif (all(shape_v1 == shape_v2)) then
                if (dim_v1 == 1) then
                    allocate(res%v, source=v1%v)
                    res%v = v1%v / v2%v
                elseif (dim_v2 == 2) then
                    allocate(res%m, source=v1%m)
                    res%m = v1%m / v2%m
                end if
            else
                stop "Same Rank but, different input shape."
            end if

        elseif (dim_v1==0) then
            ! v1=scalar and v2=vector/matrix/tensor
            if (dim_v2==1) then
                allocate(res%v, source=v2%v)
                res%v = v1%s / v2%v
            elseif (dim_v2==2) then
                allocate(res%m, source=v2%m)
                res%m = v1%s / v2%m
            end if

        elseif (dim_v2==0) then
            ! v1=vector/matrix/tensor and v2=scalar
            if (dim_v1==1) then
                allocate(res%v, source=v1%v)
                res%v = v1%v / v2%s
            elseif (dim_v1==2) then
                allocate(res%m, source=v1%m)
                res%m = v1%m / v2%s
            end if

        elseif (dim_v1==1 .and. dim_v2==2) then
            if (shape_v1(1) == shape_v2(2)) then
                allocate(res%m, source=v2%m)
                do dim1=1, shape_v2(1), 1
                    res%m(dim1, :) = v1%v(:) / v2%m(dim1, :)
                end do
            end if

        elseif (dim_v1==2 .and. dim_v2==1) then
            if (shape_v1(2) == shape_v2(1)) then
                allocate(res%m, source=v1%m)
                do dim1=1, shape_v1(1), 1
                    res%m(dim1, :) = v1%m(dim1, :) / v2%v(:)
                end do
            end if
        else
            print '(A,I0)', "ShapeMissmatchError in " // __FILE__ // " on line ", __LINE__
            stop
        end if
    end function division_vv_vv
    
    function division_vv_m(v1, m) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: m(:,:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(m)
        res = division_vv_vv(v1, v2)
    end function division_vv_m
    
    function division_vv_v(v1, v) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: v(:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(v)
        res = division_vv_vv(v1, v2)
    end function division_vv_v
    
    function division_vv_s(v1, s) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: s
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(s)
        res = division_vv_vv(v1, v2)
    end function division_vv_s
    
    function division_m_vv(m, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: m(:,:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(m)
        res = division_vv_vv(v1, v2)
    end function division_m_vv
    
    function division_v_vv(v, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: v(:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(v)
        res = division_vv_vv(v1, v2)
    end function division_v_vv
    
    function division_s_vv(s, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: s
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(s)
        res = division_vv_vv(v1, v2)
    end function division_s_vv

end module mod_variable_in_variable
