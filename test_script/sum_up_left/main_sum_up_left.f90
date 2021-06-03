program main_sum_up_left
    use mod_timer
    use Iso_C_Binding
    use ISO_FORTRAN_ENV    
    implicit none

    Interface
        function sum_up_left_naive_c_i8_i8(x,y,n,v) Bind(C,Name='sum_up_left_naive_c_i8_i8')
            Import
            Integer(c_int64_t)            :: sum_up_left_naive_c_i8_i8
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n), y(n)
            Integer(c_int64_t),Value      :: v
        end function    

        function sum_up_left_naive_branchless_c_i8_i8(x,y,n,v) Bind(C,Name='sum_up_left_naive_branchless_c_i8_i8')
            Import
            Integer(c_int64_t)            :: sum_up_left_naive_branchless_c_i8_i8
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n), y(n)
            Integer(c_int64_t),Value      :: v
        end function    

        function sum_up_left_unroll_c_i8_i8(x,y,n,v) Bind(C,Name='sum_up_left_unroll_c_i8_i8')
            Import
            Integer(c_int64_t)            :: sum_up_left_unroll_c_i8_i8
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n), y(n)
            Integer(c_int64_t),Value      :: v
        end function    

        function sum_up_left_naive_c_r8_r8(x,y,n,v) Bind(C,Name='sum_up_left_naive_c_r8_r8')
            Import
            Real(c_double)            :: sum_up_left_naive_c_r8_r8
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n), y(n)
            Real(c_double),Value  :: v
        end function

        function sum_up_left_naive_branchless_c_r8_r8(x,y,n,v) Bind(C,Name='sum_up_left_naive_branchless_c_r8_r8')
            Import
            Real(c_double)            :: sum_up_left_naive_branchless_c_r8_r8
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n), y(n)
            Real(c_double),Value  :: v
        end function

        function sum_up_left_unroll_c_r8_r8(x,y,n,v) Bind(C,Name='sum_up_left_unroll_c_r8_r8')
            Import
            Real(c_double)            :: sum_up_left_unroll_c_r8_r8
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n), y(n)
            Real(c_double),Value  :: v
        end function

        function sum_up_left_assembler_04_c_i8_i8(x,y,n,v) Bind(C,Name='sum_up_left_assembler_04_c_i8_i8')
            Import
            Integer(c_int64_t)            :: sum_up_left_assembler_04_c_i8_i8
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n), y(n)
            Integer(c_int64_t),Value      :: v
        end function    

        function sum_up_left_assembler_04_c_r8_r8(x,y,n,v) Bind(C,Name='sum_up_left_assembler_04_c_r8_r8')
            Import
            Real(c_double)            :: sum_up_left_assembler_04_c_r8_r8
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n), y(n)
            Real(c_double),Value  :: v
        end function    

        function sum_up_left_assembler_08_c_i8_i8(x,y,n,v) Bind(C,Name='sum_up_left_assembler_08_c_i8_i8')
            Import
            Integer(c_int64_t)            :: sum_up_left_assembler_08_c_i8_i8
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n), y(n)
            Integer(c_int64_t),Value      :: v
        end function    

        function sum_up_left_assembler_08_c_r8_r8(x,y,n,v) Bind(C,Name='sum_up_left_assembler_08_c_r8_r8')
            Import
            Real(c_double)            :: sum_up_left_assembler_08_c_r8_r8
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n), y(n)
            Real(c_double),Value  :: v
        end function    

        function sum_up_left_assembler_16_c_i8_i8(x,y,n,v) Bind(C,Name='sum_up_left_assembler_16_c_i8_i8')
            Import
            Integer(c_int64_t)            :: sum_up_left_assembler_16_c_i8_i8
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n), y(n)
            Integer(c_int64_t),Value      :: v
        end function    

        function sum_up_left_assembler_20_c_i8_i8(x,y,n,v) Bind(C,Name='sum_up_left_assembler_20_c_i8_i8')
            Import
            Integer(c_int64_t)            :: sum_up_left_assembler_20_c_i8_i8
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n), y(n)
            Integer(c_int64_t),Value      :: v
        end function    

        function sum_up_left_assembler_24_c_i8_i8(x,y,n,v) Bind(C,Name='sum_up_left_assembler_24_c_i8_i8')
            Import
            Integer(c_int64_t)            :: sum_up_left_assembler_24_c_i8_i8
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n), y(n)
            Integer(c_int64_t),Value      :: v
        end function    

        function sum_up_left_assembler_16_c_r8_r8(x,y,n,v) Bind(C,Name='sum_up_left_assembler_16_c_r8_r8')
            Import
            Real(c_double)            :: sum_up_left_assembler_16_c_r8_r8
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n), y(n)
            Real(c_double),Value  :: v
        end function    

        function sum_up_left_assembler_20_c_r8_r8(x,y,n,v) Bind(C,Name='sum_up_left_assembler_20_c_r8_r8')
            Import
            Real(c_double)            :: sum_up_left_assembler_20_c_r8_r8
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n), y(n)
            Real(c_double),Value  :: v
        end function    

        function sum_up_left_assembler_24_c_r8_r8(x,y,n,v) Bind(C,Name='sum_up_left_assembler_24_c_r8_r8')
            Import
            Real(c_double)            :: sum_up_left_assembler_24_c_r8_r8
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n), y(n)
            Real(c_double),Value  :: v
        end function    

    end interface

    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(c_int64_t), allocatable :: indices(:), indices_full(:)
    real(c_double), allocatable :: mask(:), tmp(:), tmp_x(:)
    real(c_double), allocatable     :: x_r8(:), y_r8(:)
    integer(c_int64_t), allocatable :: x_i8(:), y_i8(:)
    real(c_float) :: res_r4
    real(c_double) :: res_r8
    real(c_double) :: res_min1, res_min2, res_min3, res_min4, res_min5, res_min6, res_min7, res_min8
    real(c_double) :: res_max1, res_max2, res_max3, res_max4, res_max5, res_max6, res_max7, res_max8
    real(c_double) :: x_edge2(2)
    real(kind=8) :: time1, time2, time3, time4, time5, time6, time7, time8, NFLOP
    real(kind=8), ALLOCATABLE :: times(:)
    integer(kind=8) :: iter, n_iter, n_i8, k, n_x, n_i, idx, i, n_types, iter_types, res_i8
    integer(kind=4) :: n_i4, ini_04, ini_08, ini_16, fin
    character(len=30), ALLOCATABLE :: types(:)

    real(kind=8)    :: thre_r8
    integer(kind=8) :: thre_i8


    n_types = 12
    allocate(times(n_types))
    allocate(types(n_types))
    types(1) = "naive_F                     :"
    types(2) = "sum_naive_loop_F            :"
    types(3) = "sum_naive_loop_branchkess_F :"
    types(4) = "sum_naive_unroll_F          :"
    types(5) = "sum_naive_loop_C            :"
    types(6) = "sum_naive_loop_branchkess_C :"
    types(7) = "sum_up_left_unroll_C        :"
    types(8) = "sum_naive_assember_04_C     :"
    types(9) = "sum_naive_assember_08_C     :"
    types(10) = "sum_naive_assember_16_C    :"
    types(11) = "sum_naive_assember_20_C    :"
    types(12) = "sum_naive_assember_24_C    :"


    open(10, file="time_sum_up_left_r8.csv")
    open(20, file="time_sum_up_left_i8.csv")
    do k=120, 200, 10
        n_i8 = maxval((/2**(k/dble(8)), 4d0/))
        n_i4 = n_i8
        allocate(x_r8(n_i8), y_r8(n_i8))
        allocate(x_i8(n_i8), y_i8(n_i8))
        call random_number(x_r8)
        call random_number(y_r8)
        call random_number(thre_r8)
        x_r8 = 10 * x_r8
        x_i8 = x_r8
        y_r8 = 10 * y_r8
        y_i8 = y_r8
        thre_r8 = 5d0
        thre_i8 = thre_r8

        n_iter=maxval((/1000000000_8/n_i8, 1_8/))
        ! n_iter=1
        ini_04 = n_i8 - mod(n_i8, 4)+1
        ini_08 = n_i8 - mod(n_i8, 8)+1
        ini_16 = n_i8 - mod(n_i8, 16)+1
        fin = n_i8

        print*, '============================================================='
        do iter_types=1, n_types, 1
            res_r8=0d0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case (1)
                            ! res_r8 = sum(pack(y_r8, mask=x_r8<=thre_r8))
                    case (2)
                            res_r8 = sum_up_left_naive_f_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                    case (3)
                            res_r8 = sum_up_left_naive_branchless_f_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                    case (4)
                            res_r8 = sum_up_left_unroll_f_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                    case (5)
                            res_r8 = sum_up_left_naive_c_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                    case (6)
                            res_r8 = sum_up_left_naive_branchless_c_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                    case (7)
                            res_r8 = sum_up_left_unroll_c_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                    case (8)
                            res_r8 = sum_up_left_assembler_04_c_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                    case (9)
                            res_r8 = sum_up_left_assembler_08_c_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                    case (10)
                            res_r8 = sum_up_left_assembler_16_c_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                    case (11)
                            res_r8 = sum_up_left_assembler_20_c_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                    case (12)
                            res_r8 = sum_up_left_assembler_24_c_r8_r8(x_r8, y_r8, n_i8, thre_r8)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print *, types(iter_types), int(n_i8), real(times(iter_types)), res_r8
        end do
        write(10,*) k, n_i8, times

        print*, '============================================================='
        do iter_types=1, n_types, 1
            res_i8=0d0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case (1)
                            ! res_i8 = sum(pack(y_i8, mask=x_i8<=thre_i8))
                    case (2)
                            res_i8 = sum_up_left_naive_f_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                    case (3)
                            res_i8 = sum_up_left_naive_branchless_f_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                    case (4)
                            res_i8 = sum_up_left_unroll_f_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                    case (5)
                            res_i8 = sum_up_left_naive_c_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                    case (6)
                            res_i8 = sum_up_left_naive_branchless_c_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                    case (7)
                            res_i8 = sum_up_left_unroll_c_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                    case (8)
                            res_i8 = sum_up_left_assembler_04_c_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                    case (9)
                            res_i8 = sum_up_left_assembler_08_c_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                    case (10)
                            res_i8 = sum_up_left_assembler_16_c_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                    case (11)
                            res_i8 = sum_up_left_assembler_20_c_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                    case (12)
                            res_i8 = sum_up_left_assembler_24_c_i8_i8(x_i8, y_i8, n_i8, thre_i8)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print *, types(iter_types), int(n_i8), real(times(iter_types)), res_i8
        end do
        write(20,*) k, n_i8, times


        deallocate(x_r8, x_i8)
        deallocate(y_r8, y_i8)
        ! stop
        ! call sleep(5)
    end do


contains

    function sum_up_left_naive_f_i8_i8(x,y,n,v)
        implicit none
        integer(kind=8), intent(in) :: x(n), y(n), v
        integer(kind=8), intent(in) :: n
        integer(kind=8)             :: sum_up_left_naive_f_i8_i8
        integer(kind=8)             :: i
        integer(kind=8)             :: r00
        include "./inc_sum_up_left_naive_f.f90"
        sum_up_left_naive_f_i8_i8 = r00
    end function sum_up_left_naive_f_i8_i8

    function sum_up_left_naive_f_r8_r8(x,y,n,v)
        implicit none
        real(kind=8), intent(in)    :: x(n), y(n), v
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_up_left_naive_f_r8_r8
        integer(kind=8)             :: i
        real(kind=8)                :: r00
        include "./inc_sum_up_left_naive_f.f90"
        sum_up_left_naive_f_r8_r8 = r00
    end function sum_up_left_naive_f_r8_r8

    function sum_up_left_naive_branchless_f_i8_i8(x,y,n,v)
        implicit none
        integer(kind=8), intent(in) :: x(n), y(n), v
        integer(kind=8), intent(in) :: n
        integer(kind=8)             :: sum_up_left_naive_branchless_f_i8_i8
        integer(kind=8)             :: i, factor
        integer(kind=8)             :: r00
        include "./inc_sum_up_left_naive_branchless_f.f90"
        sum_up_left_naive_branchless_f_i8_i8 = r00
    end function sum_up_left_naive_branchless_f_i8_i8

    function sum_up_left_naive_branchless_f_r8_r8(x,y,n,v)
        implicit none
        real(kind=8), intent(in)    :: x(n), y(n), v
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_up_left_naive_branchless_f_r8_r8
        integer(kind=8)             :: i, factor
        real(kind=8)                :: r00
        include "./inc_sum_up_left_naive_branchless_f.f90"
        sum_up_left_naive_branchless_f_r8_r8 = r00
    end function sum_up_left_naive_branchless_f_r8_r8

    function sum_up_left_unroll_f_r8_r8(x, y, n, v)
        implicit none
        real(kind=8), intent(in)    :: x(n), y(n), v
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_up_left_unroll_f_r8_r8
        integer(kind=8)             :: i, factor, num_unroll
        real(kind=8)                :: r00, r01, r02, r03
        real(kind=8)                :: r04, r05, r06, r07
        integer(kind=8)             :: r08, r09, r10, r11
        real(kind=8)                :: r12, r13, r14, r15
        include "./inc_sum_up_left_unroll_f.f90"
        sum_up_left_unroll_f_r8_r8 = r15 + r14 + r13 + r12
    end function sum_up_left_unroll_f_r8_r8

    function sum_up_left_unroll_f_i8_i8(x, y, n, v)
        implicit none
        integer(kind=8), intent(in)    :: x(n), y(n), v
        integer(kind=8), intent(in) :: n
        integer(kind=8)                :: sum_up_left_unroll_f_i8_i8
        integer(kind=8)             :: i, factor, num_unroll
        integer(kind=8)                :: r00, r01, r02, r03
        integer(kind=8)                :: r04, r05, r06, r07
        integer(kind=8)             :: r08, r09, r10, r11
        integer(kind=8)                :: r12, r13, r14, r15
        include "./inc_sum_up_left_unroll_f.f90"
        sum_up_left_unroll_f_i8_i8 = r15 + r14 + r13 + r12
    end function sum_up_left_unroll_f_i8_i8

end program main_sum_up_left
