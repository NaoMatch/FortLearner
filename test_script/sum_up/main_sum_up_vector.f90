program main_sum_up_vector
    use mod_timer
    use Iso_C_Binding
    use ISO_FORTRAN_ENV    
    implicit none

    Interface

        ! -------------------------------------------------------------------
        ! -------------------------------------------------------------------
        function sum_naive_i8_c_(x,n) Bind(C,Name='sum_naive_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_naive_i8_c_
            Integer(c_int64_t),Value   :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end function        

        function sum_naive_r8_c_(x,n) Bind(C,Name='sum_naive_r8_c_')
            Import
            Real(c_double) :: sum_naive_r8_c_
            Integer(c_int64_t),Value :: n
            Real(c_double),Intent(In) :: x(n)
        end function    

        ! -------------------------------------------------------------------
        ! -------------------------------------------------------------------
        function sum_unroll_02_r8_c_(x,n) Bind(C,Name='sum_unroll_02_r8_c_')
            Import
            Real(c_double)            :: sum_unroll_02_r8_c_
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end function

        function sum_unroll_02_i8_c_(x,n) Bind(C,Name='sum_unroll_02_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_unroll_02_i8_c_
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end function

        function sum_unroll_04_r8_c_(x,n) Bind(C,Name='sum_unroll_04_r8_c_')
            Import
            Real(c_double)            :: sum_unroll_04_r8_c_
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end function

        function sum_unroll_04_i8_c_(x,n) Bind(C,Name='sum_unroll_04_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_unroll_04_i8_c_
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end function

        function sum_unroll_08_r8_c_(x,n) Bind(C,Name='sum_unroll_08_r8_c_')
            Import
            Real(c_double)            :: sum_unroll_08_r8_c_
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end function

        function sum_unroll_08_i8_c_(x,n) Bind(C,Name='sum_unroll_08_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_unroll_08_i8_c_
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end function

        function sum_unroll_15_r8_c_(x,n) Bind(C,Name='sum_unroll_15_r8_c_')
            Import
            Real(c_double)            :: sum_unroll_15_r8_c_
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end function

        function sum_unroll_15_i8_c_(x,n) Bind(C,Name='sum_unroll_15_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_unroll_15_i8_c_
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end function

        ! -------------------------------------------------------------------
        ! -------------------------------------------------------------------
        function sum_assembl_r8_04_C_(x,n) Bind(C,Name='sum_assembl_r8_04_C_')
            Import
            Real(c_double) :: sum_assembl_r8_04_C_
            Integer(c_int64_t),Value :: n
            Real(c_double),Intent(In) :: x(n)
        end function    

        function sum_assembl_i8_04_C_(x,n) Bind(C,Name='sum_assembl_i8_04_C_')
            Import
            Integer(c_int64_t)            :: sum_assembl_i8_04_C_
            Integer(c_int64_t),Value   :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end function        

        ! -------------------------------------------------------------------
        ! -------------------------------------------------------------------
        function sum_assembl_r8_08_C_(x,n) Bind(C,Name='sum_assembl_r8_08_C_')
            Import
            Real(c_double) :: sum_assembl_r8_08_C_
            Integer(c_int64_t),Value :: n
            Real(c_double),Intent(In) :: x(n)
        end function    

        function sum_assembl_i8_08_C_(x,n) Bind(C,Name='sum_assembl_i8_08_C_')
            Import
            Integer(c_int64_t)            :: sum_assembl_i8_08_C_
            Integer(c_int64_t),Value   :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end function        

        ! -------------------------------------------------------------------
        ! -------------------------------------------------------------------
        function sum_assembl_r8_16_C_(x,n) Bind(C,Name='sum_assembl_r8_16_C_')
            Import
            Real(c_double) :: sum_assembl_r8_16_C_
            Integer(c_int64_t),Value :: n
            Real(c_double),Intent(In) :: x(n)
        end function    

        function sum_assembl_i8_16_C_(x,n) Bind(C,Name='sum_assembl_i8_16_C_')
            Import
            Integer(c_int64_t)            :: sum_assembl_i8_16_C_
            Integer(c_int64_t),Value   :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end function        

        ! -------------------------------------------------------------------
        ! -------------------------------------------------------------------
        function sum_assembl_r8_32_C_(x,n) Bind(C,Name='sum_assembl_r8_32_C_')
            Import
            Real(c_double) :: sum_assembl_r8_32_C_
            Integer(c_int64_t),Value :: n
            Real(c_double),Intent(In) :: x(n)
        end function    

        function sum_assembl_i8_32_C_(x,n) Bind(C,Name='sum_assembl_i8_32_C_')
            Import
            Integer(c_int64_t)            :: sum_assembl_i8_32_C_
            Integer(c_int64_t),Value   :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end function        

    end interface

    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(c_int64_t), allocatable :: indices(:), indices_full(:)
    real(c_double), allocatable :: mask(:), tmp(:), tmp_x(:)
    real(c_float), allocatable :: x_r4(:)
    real(c_double), allocatable :: x_r8(:)
    integer(c_int32_t), allocatable :: x_i4(:)
    integer(c_int64_t), allocatable :: x_i8(:)
    real(c_float) :: res_r4
    real(c_double) :: res_r8
    real(c_double) :: res_min1, res_min2, res_min3, res_min4, res_min5, res_min6, res_min7, res_min8
    real(c_double) :: res_max1, res_max2, res_max3, res_max4, res_max5, res_max6, res_max7, res_max8
    real(c_double) :: x_edge2(2)
    real(kind=8) :: time1, time2, time3, time4, time5, time6, time7, time8, NFLOP
    real(kind=8), ALLOCATABLE :: times(:)
    integer(kind=8) :: iter, n_iter, n_i8, k, n_x, n_i, idx, i, n_types, iter_types, res_i8
    integer(kind=4) :: n_i4
    character(len=30), ALLOCATABLE :: types(:)

    n_types = 15
    allocate(times(n_types))
    allocate(types(n_types))
    types(1)  = "sum_intrinsic_F      :"
    types(2)  = "sum_naive_loop_F     :"
    types(3)  = "sum_unroll_02_F      :"
    types(4)  = "sum_unroll_04_F      :"
    types(5)  = "sum_unroll_08_F      :"
    types(6)  = "sum_unroll_15_F      :"
    types(7)  = "sum_naive_loop_C     :"
    types(8)  = "sum_unroll_02_C      :"
    types(9)  = "sum_unroll_04_C      :"
    types(10) = "sum_unroll_08_C      :"
    types(11) = "sum_unroll_15_C      :"
    types(12) = "sum_assembl_r8_04_C_  :"
    types(13) = "sum_assembl_r8_08_C_  :"
    types(14) = "sum_assembl_r8_16_C_  :"
    types(15) = "sum_assembl_r8_32_C_  :"

    open(10, file="time_sum_r8.csv")
    open(20, file="time_sum_i8.csv")
    do k=20, 200, 1
        n_i8 = maxval((/2**(k/dble(8)), 4d0/))
        n_i4 = n_i8
        allocate(x_r8(n_i8))
        allocate(x_i8(n_i8))
        call random_number(x_r8)
        x_r8 = 10 * x_r8
        x_i8 = x_r8
        n_iter=5000000000_8/n_i8
        ! n_iter=100000000_8/n_i8
        ! n_iter=1

        print*, '============================================================='
        do iter_types=1, n_types, 1
            res_r8=0d0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case (1)
                            res_r8 =  sum(x_r8)
                    case (2)
                            res_r8 =  sum_naive_f_r8_(x_r8, n_i8)
                    case (3)
                            res_r8 =  sum_unroll_02_f_r8_(x_r8, n_i8)
                    case (4)
                            res_r8 =  sum_unroll_04_f_r8_(x_r8, n_i8)
                    case (5)
                            res_r8 =  sum_unroll_08_f_r8_(x_r8, n_i8)
                    case (6)
                            res_r8 =  sum_unroll_15_f_r8_(x_r8, n_i8)
                    case (7)
                            res_r8 =  sum_naive_r8_c_(x_r8, n_i8)
                    case (8)
                            res_r8 =  sum_unroll_02_r8_c_(x_r8, n_i8)
                    case (9)
                            res_r8 =  sum_unroll_04_r8_c_(x_r8, n_i8)
                    case (10)
                            res_r8 =  sum_unroll_08_r8_c_(x_r8, n_i8)
                    case (11)
                            res_r8 =  sum_unroll_15_r8_c_(x_r8, n_i8)
                    case (12)
                            res_r8 =  sum_assembl_r8_04_C_(x_r8, n_i8)
                    case (13)
                            res_r8 =  sum_assembl_r8_08_C_(x_r8, n_i8)
                    case (14)
                            res_r8 =  sum_assembl_r8_16_C_(x_r8, n_i8)
                    case (15)
                            res_r8 =  sum_assembl_r8_32_C_(x_r8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print *, types(iter_types), n_i8, real(times(iter_types)), res_r8
        end do
        write(10,*) k, n_i8, times

        print*, '============================================================='
        do iter_types=1, n_types, 1
            res_i8=0d0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case (1)
                            res_i8 =  sum(x_i8)
                    case (2)
                            res_i8 =  sum_naive_f_i8_(x_i8, n_i8)
                    case (3)
                            res_i8 =  sum_unroll_02_f_i8_(x_i8, n_i8)
                    case (4)
                            res_i8 =  sum_unroll_04_f_i8_(x_i8, n_i8)
                    case (5)
                            res_i8 =  sum_unroll_08_f_i8_(x_i8, n_i8)
                    case (6)
                            res_i8 =  sum_unroll_15_f_i8_(x_i8, n_i8)
                    case (7)
                            res_i8 =  sum_naive_i8_c_(x_i8, n_i8)
                    case (8)
                            res_i8 =  sum_unroll_02_i8_c_(x_i8, n_i8)
                    case (9)
                            res_i8 =  sum_unroll_04_i8_c_(x_i8, n_i8)
                    case (10)
                            res_i8 =  sum_unroll_08_i8_c_(x_i8, n_i8)
                    case (11)
                            res_i8 =  sum_unroll_15_i8_c_(x_i8, n_i8)
                    case (12)
                            res_i8 =  sum_assembl_i8_04_C_(x_i8, n_i8)
                    case (13)
                            res_i8 =  sum_assembl_i8_08_C_(x_i8, n_i8)
                    case (14)
                            res_i8 =  sum_assembl_i8_16_C_(x_i8, n_i8)
                    case (15)
                            res_i8 =  sum_assembl_i8_32_C_(x_i8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print *, types(iter_types), n_i8, real(times(iter_types)), res_i8
        end do
        write(20,*) k, n_i8, times


        deallocate(x_r8, x_i8)
        call sleep(3)
    end do


contains

    function sum_naive_f_i8_(x,n)
        implicit none
        integer(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n
        integer(kind=8)                :: sum_naive_f_i8_

        integer(kind=8) :: i
        integer(kind=8)    :: r00
        include "./inc_sum_naive_f.f90"
        sum_naive_f_i8_ = r00
    end function sum_naive_f_i8_

    function sum_naive_f_r8_(x,n)
        implicit none
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_naive_f_r8_

        integer(kind=8) :: i
        real(kind=8)    :: r00
        include "./inc_sum_naive_f.f90"
        sum_naive_f_r8_ = r00
    end function sum_naive_f_r8_

    function sum_unroll_02_f_i8_(x,n)
        implicit none
        integer(kind=8), intent(in) :: x(n)
        integer(kind=8), intent(in) :: n
        integer(kind=8)             :: sum_unroll_02_f_i8_

        integer(kind=8) :: num_unroll, i
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        sum_unroll_02_f_i8_=0d0
        num_unroll = n - mod(n, 2)
        include "./inc_sum_unroll_02_f.f90"
        sum_unroll_02_f_i8_ = r15 + r14 + r13 + r12
    end function sum_unroll_02_f_i8_

    function sum_unroll_02_f_r8_(x,n)
        implicit none
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_unroll_02_f_r8_

        integer(kind=8) :: num_unroll, i
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        sum_unroll_02_f_r8_=0d0
        num_unroll = n - mod(n, 2)
        include "./inc_sum_unroll_02_f.f90"
        sum_unroll_02_f_r8_ = r15 + r14 + r13 + r12
    end function sum_unroll_02_f_r8_

    function sum_unroll_04_f_i8_(x,n)
        implicit none
        integer(kind=8), intent(in) :: x(n)
        integer(kind=8), intent(in) :: n
        integer(kind=8)             :: sum_unroll_04_f_i8_

        integer(kind=8) :: num_unroll, i
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        sum_unroll_04_f_i8_=0d0
        num_unroll = n - mod(n, 4)
        include "./inc_sum_unroll_04_f.f90"
        sum_unroll_04_f_i8_ = r15 + r14 + r13 + r12
    end function sum_unroll_04_f_i8_

    function sum_unroll_04_f_r8_(x,n)
        implicit none
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_unroll_04_f_r8_

        integer(kind=8) :: num_unroll, i
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        sum_unroll_04_f_r8_=0d0
        num_unroll = n - mod(n, 4)
        include "./inc_sum_unroll_04_f.f90"
        sum_unroll_04_f_r8_ = r15 + r14 + r13 + r12
    end function sum_unroll_04_f_r8_

    function sum_unroll_08_f_i8_(x,n)
        implicit none
        integer(kind=8), intent(in) :: x(n)
        integer(kind=8), intent(in) :: n
        integer(kind=8)             :: sum_unroll_08_f_i8_

        integer(kind=8) :: num_unroll, i
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        sum_unroll_08_f_i8_=0d0
        num_unroll = n - mod(n, 8)
        include "./inc_sum_unroll_08_f.f90"
        sum_unroll_08_f_i8_ = r15 + r14 + r13 + r12
    end function sum_unroll_08_f_i8_

    function sum_unroll_08_f_r8_(x,n)
        implicit none
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_unroll_08_f_r8_

        integer(kind=8) :: num_unroll, i
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        sum_unroll_08_f_r8_=0d0
        num_unroll = n - mod(n, 8)
        include "./inc_sum_unroll_08_f.f90"
        sum_unroll_08_f_r8_ = r15 + r14 + r13 + r12
    end function sum_unroll_08_f_r8_

    function sum_unroll_15_f_i8_(x,n)
        implicit none
        integer(kind=8), intent(in) :: x(n)
        integer(kind=8), intent(in) :: n
        integer(kind=8)             :: sum_unroll_15_f_i8_

        integer(kind=8) :: num_unroll, i
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        sum_unroll_15_f_i8_=0d0
        num_unroll = n - mod(n, 15)
        include "./inc_sum_unroll_15_f.f90"
        sum_unroll_15_f_i8_ = r15
    end function sum_unroll_15_f_i8_

    function sum_unroll_15_f_r8_(x,n)
        implicit none
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_unroll_15_f_r8_

        integer(kind=8) :: num_unroll, i
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        sum_unroll_15_f_r8_=0d0
        num_unroll = n - mod(n, 15)
        include "./inc_sum_unroll_15_f.f90"
        sum_unroll_15_f_r8_ = r15
    end function sum_unroll_15_f_r8_

    function sum_up_hydrid_r8(x,n)
        implicit none
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_up_hydrid_r8

        if (n .le. 256_8) then
            sum_up_hydrid_r8 = sum_unroll_08_f_r8_(x,n)
        elseif(n .le. 1000000) then
            sum_up_hydrid_r8 = sum_assembl_r8_16_C_(x,n)
        else
            sum_up_hydrid_r8 = sum_assembl_r8_08_C_(x,n)
        end if
    end function sum_up_hydrid_r8

    function sum_up_hydrid_i8(x,n)
        implicit none
        integer(kind=8), intent(in) :: x(n)
        integer(kind=8), intent(in) :: n
        integer(kind=8)             :: sum_up_hydrid_i8

        if (n .le. 256_8) then
            sum_up_hydrid_i8 = sum_unroll_08_f_i8_(x,n)
        elseif(n .le. 1000000) then
            sum_up_hydrid_i8 = sum_assembl_i8_16_C_(x,n)
        else
            sum_up_hydrid_i8 = sum_assembl_i8_08_C_(x,n)
        end if
    end function sum_up_hydrid_i8

end program main_sum_up_vector
