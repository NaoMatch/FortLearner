!> A function to sum up vector, integer 64bit.
!! Optimization was performed using only Fortran.
!! \param sum_up_02_f_i8 Sum of vector elements
!! \param x vector
!! \param n size of vector
function sum_up_02_f_i8(x,n)
    implicit none
    integer(kind=8), intent(in) :: x(n)
    integer(kind=8), intent(in) :: n
    integer(kind=8)             :: sum_up_02_f_i8

    integer(kind=8) :: num_unroll, i
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    sum_up_02_f_i8=0d0
    num_unroll = n - mod(n, 2)
    include "./include/stats/sum_up/inc_sum_up_02_f.f90"
    sum_up_02_f_i8 = r15 + r14 + r13 + r12
end function sum_up_02_f_i8

!> A function to sum up vector, integer 64bit.
!! Optimization was performed using only Fortran.
!! \param sum_up_02_f_r8 Sum of vector elements
!! \param x vector
!! \param n size of vector
function sum_up_02_f_r8(x,n)
    implicit none
    real(kind=8), intent(in)    :: x(n)
    integer(kind=8), intent(in) :: n
    real(kind=8)                :: sum_up_02_f_r8

    integer(kind=8) :: num_unroll, i
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    sum_up_02_f_r8=0d0
    num_unroll = n - mod(n, 2)
    include "./include/stats/sum_up/inc_sum_up_02_f.f90"
    sum_up_02_f_r8 = r15 + r14 + r13 + r12
end function sum_up_02_f_r8

!> A function to sum up vector, integer 64bit.
!! Optimization was performed using only Fortran.
!! \param sum_up_04_f_i8 Sum of vector elements
!! \param x vector
!! \param n size of vector
function sum_up_04_f_i8(x,n)
    implicit none
    integer(kind=8), intent(in) :: x(n)
    integer(kind=8), intent(in) :: n
    integer(kind=8)             :: sum_up_04_f_i8

    integer(kind=8) :: num_unroll, i
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    sum_up_04_f_i8=0d0
    num_unroll = n - mod(n, 4)
    include "./include/stats/sum_up/inc_sum_up_04_f.f90"
    sum_up_04_f_i8 = r15 + r14 + r13 + r12
end function sum_up_04_f_i8

!> A function to sum up vector, integer 64bit.
!! Optimization was performed using only Fortran.
!! \param sum_up_04_f_r8 Sum of vector elements
!! \param x vector
!! \param n size of vector
function sum_up_04_f_r8(x,n)
    implicit none
    real(kind=8), intent(in)    :: x(n)
    integer(kind=8), intent(in) :: n
    real(kind=8)                :: sum_up_04_f_r8

    integer(kind=8) :: num_unroll, i
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    sum_up_04_f_r8=0d0
    num_unroll = n - mod(n, 4)
    include "./include/stats/sum_up/inc_sum_up_04_f.f90"
    sum_up_04_f_r8 = r15 + r14 + r13 + r12
end function sum_up_04_f_r8

!> A function to sum up vector, integer 64bit.
!! Optimization was performed using only Fortran.
!! \param sum_up_08_f_i8 Sum of vector elements
!! \param x vector
!! \param n size of vector
function sum_up_08_f_i8(x,n)
    implicit none
    integer(kind=8), intent(in) :: x(n)
    integer(kind=8), intent(in) :: n
    integer(kind=8)             :: sum_up_08_f_i8

    integer(kind=8) :: num_unroll, i
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    sum_up_08_f_i8=0d0
    num_unroll = n - mod(n, 8)
    include "./include/stats/sum_up/inc_sum_up_08_f.f90"
    sum_up_08_f_i8 = r15 + r14 + r13 + r12
end function sum_up_08_f_i8

!> A function to sum up vector, integer 64bit.
!! Optimization was performed using only Fortran.
!! \param sum_up_08_f_r8 Sum of vector elements
!! \param x vector
!! \param n size of vector
function sum_up_08_f_r8(x,n)
    implicit none
    real(kind=8), intent(in)    :: x(n)
    integer(kind=8), intent(in) :: n
    real(kind=8)                :: sum_up_08_f_r8

    integer(kind=8) :: num_unroll, i
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    sum_up_08_f_r8=0d0
    num_unroll = n - mod(n, 8)
    include "./include/stats/sum_up/inc_sum_up_08_f.f90"
    sum_up_08_f_r8 = r15 + r14 + r13 + r12
end function sum_up_08_f_r8

!> A function to sum up vector, integer 64bit.
!! Optimization was performed using only Fortran.
!! \param sum_up_15_f_i8 Sum of vector elements
!! \param x vector
!! \param n size of vector
function sum_up_15_f_i8(x,n)
    implicit none
    integer(kind=8), intent(in) :: x(n)
    integer(kind=8), intent(in) :: n
    integer(kind=8)             :: sum_up_15_f_i8

    integer(kind=8) :: num_unroll, i
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    sum_up_15_f_i8=0d0
    num_unroll = n - mod(n, 15)
    include "./include/stats/sum_up/inc_sum_up_15_f.f90"
    sum_up_15_f_i8 = r15
end function sum_up_15_f_i8

!> A function to sum up vector, integer 64bit.
!! Optimization was performed using only Fortran.
!! \param sum_up_15_f_r8 Sum of vector elements
!! \param x vector
!! \param n size of vector
function sum_up_15_f_r8(x,n)
    implicit none
    real(kind=8), intent(in)    :: x(n)
    integer(kind=8), intent(in) :: n
    real(kind=8)                :: sum_up_15_f_r8

    integer(kind=8) :: num_unroll, i
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    sum_up_15_f_r8=0d0
    num_unroll = n - mod(n, 15)
    include "./include/stats/sum_up/inc_sum_up_15_f.f90"
    sum_up_15_f_r8 = r15
end function sum_up_15_f_r8

!> A function to sum up vector, integer 64bit.
!! Optimization was performed using only Fortran.
!! \param sum_up_30_f_i8 Sum of vector elements
!! \param x vector
!! \param n size of vector
function sum_up_30_f_i8(x,n)
    implicit none
    integer(kind=8), intent(in) :: x(n)
    integer(kind=8), intent(in) :: n
    integer(kind=8)             :: sum_up_30_f_i8

    integer(kind=8) :: num_unroll, i
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    sum_up_30_f_i8=0d0
    num_unroll = n - mod(n, 30)
    include "./include/stats/sum_up/inc_sum_up_30_f.f90"
    sum_up_30_f_i8 = r15
end function sum_up_30_f_i8

!> A function to sum up vector, integer 64bit.
!! Optimization was performed using only Fortran.
!! \param sum_up_30_f_r8 Sum of vector elements
!! \param x vector
!! \param n size of vector
function sum_up_30_f_r8(x,n)
    implicit none
    real(kind=8), intent(in)    :: x(n)
    integer(kind=8), intent(in) :: n
    real(kind=8)                :: sum_up_30_f_r8

    integer(kind=8) :: num_unroll, i
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    sum_up_30_f_r8=0d0
    num_unroll = n - mod(n, 30)
    include "./include/stats/sum_up/inc_sum_up_30_f.f90"
    sum_up_30_f_r8 = r15
end function sum_up_30_f_r8