module mod_variable_in_variable
    implicit none

    type variable_in_variable
        real(kind=8)              :: s
        real(kind=8), allocatable :: v(:)
        real(kind=8), allocatable :: m(:,:)
        real(kind=8), allocatable :: t(:,:,:,:)
    contains
        procedure :: shapes
        procedure :: dims
        procedure :: compare_shape
    end type variable_in_variable

    interface operator(**)
        module procedure :: power_vv
    end interface operator(**)

    interface operator(+)
        module procedure :: addition_vv_vv
        module procedure :: addition_vv_t
        module procedure :: addition_vv_m
        module procedure :: addition_vv_v
        module procedure :: addition_vv_s
        module procedure :: addition_t_vv
        module procedure :: addition_m_vv
        module procedure :: addition_v_vv
        module procedure :: addition_s_vv
    end interface operator(+)

    interface operator(-)
        module procedure :: substraction_vv_vv
        module procedure :: substraction_vv_t
        module procedure :: substraction_vv_m
        module procedure :: substraction_vv_v
        module procedure :: substraction_vv_s
        module procedure :: substraction_t_vv
        module procedure :: substraction_m_vv
        module procedure :: substraction_v_vv
        module procedure :: substraction_s_vv
    end interface operator(-)

    interface operator(*)
        module procedure :: multiplication_vv_vv
        module procedure :: multiplication_vv_t
        module procedure :: multiplication_vv_m
        module procedure :: multiplication_vv_v
        module procedure :: multiplication_vv_s
        module procedure :: multiplication_t_vv
        module procedure :: multiplication_m_vv
        module procedure :: multiplication_v_vv
        module procedure :: multiplication_s_vv
    end interface operator(*)

    interface operator(/)
        module procedure :: division_vv_vv
        module procedure :: division_vv_t
        module procedure :: division_vv_m
        module procedure :: division_vv_v
        module procedure :: division_vv_s
        module procedure :: division_t_vv
        module procedure :: division_m_vv
        module procedure :: division_v_vv
        module procedure :: division_s_vv        
    end interface operator(/)

    interface variable_in_variable
        module procedure :: new_variable_in_variable_s
        module procedure :: new_variable_in_variable_v
        module procedure :: new_variable_in_variable_m
        module procedure :: new_variable_in_variable_t
    end interface variable_in_variable

    interface assignment(=)
        module procedure assign_s
        module procedure assign_v
        module procedure assign_m
        module procedure assign_t
    end interface

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

contains

    function tanh_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = tanh(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = tanh(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = tanh(viv%v)
        elseif (dim_viv==0) then
            res%s = tanh(viv%s)
        end if
    end function tanh_viv

    function tan_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = tan(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = tan(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = tan(viv%v)
        elseif (dim_viv==0) then
            res%s = tan(viv%s)
        end if
    end function tan_viv

    function sqrt_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = sqrt(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = sqrt(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = sqrt(viv%v)
        elseif (dim_viv==0) then
            res%s = sqrt(viv%s)
        end if
    end function sqrt_viv

    function sinh_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = sinh(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = sinh(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = sinh(viv%v)
        elseif (dim_viv==0) then
            res%s = sinh(viv%s)
        end if
    end function sinh_viv

    function sin_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = sin(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = sin(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = sin(viv%v)
        elseif (dim_viv==0) then
            res%s = sin(viv%s)
        end if
    end function sin_viv

    function log10_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = log10(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = log10(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = log10(viv%v)
        elseif (dim_viv==0) then
            res%s = log10(viv%s)
        end if
    end function log10_viv

    function log_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = log(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = log(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = log(viv%v)
        elseif (dim_viv==0) then
            res%s = log(viv%s)
        end if
    end function log_viv

    function exp_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = exp(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = exp(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = exp(viv%v)
        elseif (dim_viv==0) then
            res%s = exp(viv%s)
        end if
    end function exp_viv

    function cosh_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = cosh(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = cosh(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = cosh(viv%v)
        elseif (dim_viv==0) then
            res%s = cosh(viv%s)
        end if
    end function cosh_viv

    function cos_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = cos(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = cos(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = cos(viv%v)
        elseif (dim_viv==0) then
            res%s = cos(viv%s)
        end if
    end function cos_viv

    function atan_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = atan(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = atan(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = atan(viv%v)
        elseif (dim_viv==0) then
            res%s = atan(viv%s)
        end if
    end function atan_viv

    function asin_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = asin(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = asin(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = asin(viv%v)
        elseif (dim_viv==0) then
            res%s = asin(viv%s)
        end if
    end function asin_viv

    function acos_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = acos(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = acos(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = acos(viv%v)
        elseif (dim_viv==0) then
            res%s = acos(viv%s)
        end if
    end function acos_viv

    function floor_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = floor(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = floor(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = floor(viv%v)
        elseif (dim_viv==0) then
            res%s = floor(viv%s)
        end if
    end function floor_viv

    function ceiling_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = ceiling(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = ceiling(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = ceiling(viv%v)
        elseif (dim_viv==0) then
            res%s = ceiling(viv%s)
        end if
    end function ceiling_viv

    function abs_viv(viv) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = abs(viv%t)
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = abs(viv%m)
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = abs(viv%v)
        elseif (dim_viv==0) then
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

    subroutine assign_t(viv, t)
        implicit none
        type(variable_in_variable), intent(inout) :: viv
        real(kind=8), intent(in) :: t(:,:,:,:)
        viv = variable_in_variable(t)
    end subroutine assign_t


    ! Create Object
    function new_variable_in_variable_s(s) result(output)
        implicit none
        real(kind=8), intent(in) :: s
        type(variable_in_variable)  :: output
        output%s = s
    end function 

    function new_variable_in_variable_v(v) result(output)
        implicit none
        real(kind=8), intent(in) :: v(:)
        type(variable_in_variable)  :: output
        allocate(output%v, source=v)
        output%v = v
    end function 

    function new_variable_in_variable_m(m) result(output)
        implicit none
        real(kind=8), intent(in) :: m(:,:)
        type(variable_in_variable)  :: output
        allocate(output%m, source=m)
        output%m = m
    end function 

    function new_variable_in_variable_t(t) result(output)
        implicit none
        real(kind=8), intent(in) :: t(:,:,:,:)
        type(variable_in_variable)  :: output
        allocate(output%t, source=t)
        output%t = t
    end function 


    ! Shape
    function shapes(this)
        implicit none
        class(variable_in_variable) :: this
        integer(kind=8), allocatable :: shapes(:)
        if (allocated(this%t)) then
            shapes = shape(this%t)
        elseif (allocated(this%m)) then
            shapes = shape(this%m)
        elseif (allocated(this%v)) then
            shapes = shape(this%v)
        else
            allocate(shapes(0))
        end if
    end function shapes

    ! Dim
    function dims(this)
        implicit none
        class(variable_in_variable) :: this
        integer(kind=8) :: dims
        dims = 0
        if (allocated(this%t)) dims = 4
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

        shape_this = this%shapes(); dim_this = this%dims()
        shape_that = that%shapes(); dim_that = that%dims()

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
    function power_vv(viv, c) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: viv
        integer(kind=8), intent(in) :: c
        type(variable_in_variable)  :: res

        integer(kind=8) :: dim_viv

        dim_viv = viv%dims()
        if (dim_viv==4) then
            allocate(res%t, source=viv%t)
            res%t = viv%t**c
        elseif (dim_viv==2) then
            allocate(res%m, source=viv%m)
            res%m = viv%m**c
        elseif (dim_viv==1) then
            allocate(res%v, source=viv%v)
            res%v = viv%v**c
        elseif (dim_viv==0) then
            res%s = viv%s**c
        end if
    end function power_vv


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

        dim_v1 = v1%dims()
        dim_v2 = v2%dims()

        shape_v1 = v1%shapes()
        shape_v2 = v2%shapes()

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
                elseif (dim_v2 == 4) then
                    allocate(res%t, source=v1%t)
                    res%t = v1%t + v2%t
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
            elseif (dim_v2==4) then
                allocate(res%t, source=v2%t)
                res%t = v1%s + v2%t
            end if

        elseif (dim_v2==0) then
            ! v1=vector/matrix/tensor and v2=scalar
            if (dim_v1==1) then
                allocate(res%v, source=v1%v)
                res%v = v1%v + v2%s
            elseif (dim_v1==2) then
                allocate(res%m, source=v1%m)
                res%m = v1%m + v2%s
            elseif (dim_v1==4) then
                allocate(res%t, source=v1%t)
                res%t = v1%t + v2%s
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
    
    function addition_vv_t(v1, t) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: t(:,:,:,:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(t)
        res = addition_vv_vv(v1, v2)
    end function addition_vv_t
    
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
    
    function addition_t_vv(t, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: t(:,:,:,:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(t)
        res = addition_vv_vv(v1, v2)
    end function addition_t_vv
    
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

        dim_v1 = v1%dims()
        dim_v2 = v2%dims()

        shape_v1 = v1%shapes()
        shape_v2 = v2%shapes()

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
                elseif (dim_v2 == 4) then
                    allocate(res%t, source=v1%t)
                    res%t = v1%t - v2%t
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
            elseif (dim_v2==4) then
                allocate(res%t, source=v2%t)
                res%t = v1%s - v2%t
            end if

        elseif (dim_v2==0) then
            ! v1=vector/matrix/tensor and v2=scalar
            if (dim_v1==1) then
                allocate(res%v, source=v1%v)
                res%v = v1%v - v2%s
            elseif (dim_v1==2) then
                allocate(res%m, source=v1%m)
                res%m = v1%m - v2%s
            elseif (dim_v1==4) then
                allocate(res%t, source=v1%t)
                res%t = v1%t - v2%s
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
    
    function substraction_vv_t(v1, t) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: t(:,:,:,:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(t)
        res = substraction_vv_vv(v1, v2)
    end function substraction_vv_t
    
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
    
    function substraction_t_vv(t, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: t(:,:,:,:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(t)
        res = substraction_vv_vv(v1, v2)
    end function substraction_t_vv
    
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

        dim_v1 = v1%dims()
        dim_v2 = v2%dims()

        shape_v1 = v1%shapes()
        shape_v2 = v2%shapes()

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
                elseif (dim_v2 == 4) then
                    allocate(res%t, source=v1%t)
                    res%t = v1%t * v2%t
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
            elseif (dim_v2==4) then
                allocate(res%t, source=v2%t)
                res%t = v1%s * v2%t
            end if

        elseif (dim_v2==0) then
            ! v1=vector/matrix/tensor and v2=scalar
            if (dim_v1==1) then
                allocate(res%v, source=v1%v)
                res%v = v1%v * v2%s
            elseif (dim_v1==2) then
                allocate(res%m, source=v1%m)
                res%m = v1%m * v2%s
            elseif (dim_v1==4) then
                allocate(res%t, source=v1%t)
                res%t = v1%t * v2%s
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
    
    function multiplication_vv_t(v1, t) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: t(:,:,:,:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(t)
        res = multiplication_vv_vv(v1, v2)
    end function multiplication_vv_t
    
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
    
    function multiplication_t_vv(t, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: t(:,:,:,:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(t)
        res = multiplication_vv_vv(v1, v2)
    end function multiplication_t_vv
    
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

        dim_v1 = v1%dims()
        dim_v2 = v2%dims()

        shape_v1 = v1%shapes()
        shape_v2 = v2%shapes()

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
                elseif (dim_v2 == 4) then
                    allocate(res%t, source=v1%t)
                    res%t = v1%t / v2%t
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
            elseif (dim_v2==4) then
                allocate(res%t, source=v2%t)
                res%t = v1%s / v2%t
            end if

        elseif (dim_v2==0) then
            ! v1=vector/matrix/tensor and v2=scalar
            if (dim_v1==1) then
                allocate(res%v, source=v1%v)
                res%v = v1%v / v2%s
            elseif (dim_v1==2) then
                allocate(res%m, source=v1%m)
                res%m = v1%m / v2%s
            elseif (dim_v1==4) then
                allocate(res%t, source=v1%t)
                res%t = v1%t / v2%s
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
    
    function division_vv_t(v1, t) result(res)
        implicit none
        type(variable_in_variable), intent(in) :: v1
        real(kind=8), intent(in) :: t(:,:,:,:)
        type(variable_in_variable) :: v2
        type(variable_in_variable) :: res
        v2 = variable_in_variable(t)
        res = division_vv_vv(v1, v2)
    end function division_vv_t
    
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
    
    function division_t_vv(t, v2) result(res)
        implicit none
        real(kind=8), intent(in) :: t(:,:,:,:)
        type(variable_in_variable), intent(in) :: v2
        type(variable_in_variable) :: v1
        type(variable_in_variable) :: res
        v1 = variable_in_variable(t)
        res = division_vv_vv(v1, v2)
    end function division_t_vv
    
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


program main_variable
    use mod_variable
    implicit none
    
    real(kind=8) :: scl, vec(5), mat(10,5), mat_orig(10,5)
    type(variable_in_variable) :: v_scl, v_vec, v_mat
    integer(kind=8) :: i

    scl = 10d0
    vec = [1d0, 2d0, 3d0, 4d0, 5d0]
    do i=1, 10, 1
        mat(i,:) = [1d0, 2d0, 3d0, 4d0, 5d0]*i
    end do



    ! scalar + scalar
    print*, '*********************************************************************************************'
    print*, "  scalar + scalar"
    v_scl = scl
    print*, "    v_scl = 10d0:          ", v_scl%s  == 10
    v_scl = v_scl + 10d0
    print*, "    v_scl = v_scl + 10d0:  ", v_scl%s  == 20
    v_scl = 10d0 + v_scl
    print*, "    v_scl = 10d0 + v_scl:  ", v_scl%s  == 30
    v_scl = v_scl + v_scl
    print*, "    v_scl = v_scl + v_scl: ", v_scl%s  == 60
    
    ! vector + scalar
    print*, '*********************************************************************************************'
    print*, "  vector + scalar"
    v_vec = vec
    print*, "    v_vec = vec:           ", all(real(v_vec%v) == vec)
    v_vec = v_vec + 10d0
    print*, "    v_vec = v_vec + 10d0:  ", all(real(v_vec%v) == vec+10d0)
    v_vec = 10d0 + v_vec
    print*, "    v_vec = 10d0 + v_vec:  ", all(real(v_vec%v) == vec+20d0)
    v_vec = v_vec + vec
    print*, "    v_vec = v_vec + vec:   ", all(real(v_vec%v) == 2d0*vec+20d0)
    v_vec = v_vec + v_scl
    print*, "    v_vec = v_vec + v_scl: ", all(real(v_vec%v) == 2d0*vec+80d0)
    v_vec = v_scl + v_vec
    print*, "    v_vec = v_scl + v_vec: ", all(real(v_vec%v) == 2d0*vec+140d0)
    v_vec = v_vec + v_vec
    print*, "    v_vec = v_vec + v_vec: ", all(real(v_vec%v) == 4d0*vec+280d0)

    ! matrix + scalar
    print*, '*********************************************************************************************'
    print*, "  matrix + scalar"
    v_mat = mat
    print*, "    v_mat = mat :              ", all(real(v_mat%m) == mat)
    v_mat = v_mat + 10d0
    print*, "    v_mat = v_mat + 10d0 :     ", all(real(v_mat%m) == mat+10d0)
    v_mat = 10d0 + v_mat
    print*, "    v_mat = 10d0 + v_mat :     ", all(real(v_mat%m) == mat+20d0)
    v_mat = v_mat + vec
    print*, "    v_mat = v_mat + vec :      ", all(real(v_mat%m) == mat+20d0+spread(vec,dim=1,ncopies=10))
    v_mat = vec + v_mat
    print*, "    v_mat = vec + v_mat :      ", all(real(v_mat%m) == mat+20d0+2d0*spread(vec,dim=1,ncopies=10))
    v_mat = v_mat + mat
    print*, "    v_mat = v_mat + mat :      ", all(real(v_mat%m) == 2d0*mat+20d0+2d0*spread(vec,dim=1,ncopies=10))
    v_mat = mat + v_mat
    print*, "    v_mat = mat + v_mat :      ", all(real(v_mat%m) == 3d0*mat+20d0+2d0*spread(vec,dim=1,ncopies=10))
    v_mat = v_mat + v_scl
    print*, "    v_mat = v_mat + v_scl :    ", all(real(v_mat%m) == 3d0*mat+80d0+2d0*spread(vec,dim=1,ncopies=10))
    v_mat = v_mat + v_vec
    print*, "    v_mat = v_mat + v_vec :    ", all(real(v_mat%m) == 3d0*mat+80d0+2d0*spread(vec,dim=1,ncopies=10) &
        + spread(v_vec%v, dim=1, ncopies=10))
    v_mat = v_mat + v_mat
    print*, "    v_mat = v_mat + v_mat :    ", all(real(v_mat%m) == (3d0*mat+80d0+2d0*spread(vec,dim=1,ncopies=10) &
        + spread(v_vec%v, dim=1, ncopies=10))*2d0)

end program main_variable