program main_minmax
    use mod_timer
    use Iso_C_Binding
    use ISO_FORTRAN_ENV    

    implicit none
    
    Interface
        subroutine minmax_loop_c_r8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_c_r8')
            Import
            Real(c_double)            :: x_min, x_max
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_c_i8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_c_i8')
            Import
            Integer(c_int64_t)            :: x_min, x_max
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_04_c_r8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_04_c_r8')
            Import
            Real(c_double)            :: x_min, x_max
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_04_c_i8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_04_c_i8')
            Import
            Integer(c_int64_t)            :: x_min, x_max
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_08_c_r8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_08_c_r8')
            Import
            Real(c_double)            :: x_min, x_max
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_08_c_i8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_08_c_i8')
            Import
            Integer(c_int64_t)            :: x_min, x_max
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_14_c_r8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_14_c_r8')
            Import
            Real(c_double)            :: x_min, x_max
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_14_c_i8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_14_c_i8')
            Import
            Integer(c_int64_t)            :: x_min, x_max
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_02_ams_r8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_02_ams_r8')
            Import
            Real(c_double)            :: x_min, x_max
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_02_ams_i8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_02_ams_i8')
            Import
            Integer(c_int64_t)            :: x_min, x_max
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_04_ams_r8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_04_ams_r8')
            Import
            Real(c_double)            :: x_min, x_max
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_04_ams_i8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_04_ams_i8')
            Import
            Integer(c_int64_t)            :: x_min, x_max
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_08_ams_r8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_08_ams_r8')
            Import
            Real(c_double)            :: x_min, x_max
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_08_ams_i8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_08_ams_i8')
            Import
            Integer(c_int64_t)            :: x_min, x_max
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_16_ams_r8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_16_ams_r8')
            Import
            Real(c_double)            :: x_min, x_max
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_16_ams_i8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_16_ams_i8')
            Import
            Integer(c_int64_t)            :: x_min, x_max
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_32_ams_r8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_32_ams_r8')
            Import
            Real(c_double)            :: x_min, x_max
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_32_ams_i8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_32_ams_i8')
            Import
            Integer(c_int64_t)            :: x_min, x_max
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_64_ams_r8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_64_ams_r8')
            Import
            Real(c_double)            :: x_min, x_max
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end subroutine    

        subroutine minmax_loop_unroll_64_ams_i8(x_min, x_max, x, n) Bind(C,Name='minmax_loop_unroll_64_ams_i8')
            Import
            Integer(c_int64_t)            :: x_min, x_max
            Integer(c_int64_t),Value      :: n
            Integer(c_int64_t),Intent(In) :: x(n)
        end subroutine    
    end interface

    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(c_int64_t), allocatable :: indices(:), indices_full(:)
    real(c_double), allocatable :: mask(:), tmp(:), tmp_x(:)
    real(c_float), allocatable :: x_r4(:)
    real(c_double), allocatable :: x_r8(:)
    integer(c_int32_t), allocatable :: x_i4(:)
    integer(c_int64_t), allocatable :: x_i8(:)
    real(c_float) :: res_r4
    real(c_double) :: min_r8, max_r8
    real(c_double) :: res_min1, res_min2, res_min3, res_min4, res_min5, res_min6, res_min7, res_min8
    real(c_double) :: res_max1, res_max2, res_max3, res_max4, res_max5, res_max6, res_max7, res_max8
    real(c_double) :: x_edge2(2)
    real(kind=8) :: time1, time2, time3, time4, time5, time6, time7, time8, NFLOP
    real(kind=8), ALLOCATABLE :: times(:)
    integer(kind=8) :: iter, n_iter, n_i8, k, n_x, n_i, idx, i, n_types, iter_types, min_i8, max_i8
    integer(kind=4) :: n_i4
    character(len=30), ALLOCATABLE :: types(:)

    n_types = 15
    allocate(times(n_types))
    allocate(types(n_types))
    types(1)  = "minmax_intrinsic_F         :"
    types(2)  = "minmax_loop_F              :"
    types(3)  = "minmax_loop_unroll_04_F    :"
    types(4)  = "minmax_loop_unroll_08_F    :"
    types(5)  = "minmax_loop_unroll_14_F    :"
    types(6)  = "minmax_loop_C              :"
    types(7)  = "minmax_loop_unroll_04      :"
    types(8)  = "minmax_loop_unroll_08      :"
    types(9)  = "minmax_loop_unroll_14      :"
    types(10) = "minmax_loop_unroll_02_ams  :"
    types(11) = "minmax_loop_unroll_04_ams  :"
    types(12) = "minmax_loop_unroll_08_ams  :"
    types(13) = "minmax_loop_unroll_16_ams  :"
    types(14) = "minmax_loop_unroll_32_ams  :"
    types(15) = "minmax_loop_unroll_64_ams  :"

    open(10, file="time_minmax_r8.csv")
    open(20, file="time_minmax_i8.csv")
    do k=20, 200, 1
        n_i8 = maxval((/2**(k/dble(8)), 4d0/))
        n_i4 = n_i8
        allocate(x_r8(n_i8))
        allocate(x_i8(n_i8))
        call random_number(x_r8)
        x_r8 = 10d0 * x_r8-5d0
        x_i8 = x_r8
        n_iter=5000000_8/n_i8
        ! n_iter=1
        x_r8(n_i8) = -1000000
        x_i8(n_i8) = -1000000

        print*, '============================================================='
        do iter_types=1, n_types, 1
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case (1)
                        min_r8 = minval(x_r8)
                        max_r8 = maxval(x_r8)
                    case (2)
                        call minmax_loop_f_r8(min_r8, max_r8, x_r8, n_i8)
                    case (3)
                        call minmax_loop_unroll_04_f_r8(min_r8, max_r8, x_r8, n_i8)
                    case (4)
                        call minmax_loop_unroll_08_f_r8(min_r8, max_r8, x_r8, n_i8)
                    case (5)
                        call minmax_loop_unroll_14_f_r8(min_r8, max_r8, x_r8, n_i8)
                    case (6)
                        call minmax_loop_c_r8(min_r8, max_r8, x_r8, n_i8)
                    case (7)
                        call minmax_loop_unroll_04_c_r8(min_r8, max_r8, x_r8, n_i8)
                    case (8)
                        call minmax_loop_unroll_04_c_r8(min_r8, max_r8, x_r8, n_i8)
                    case (9)
                        call minmax_loop_unroll_04_c_r8(min_r8, max_r8, x_r8, n_i8)
                    case (10)
                        call minmax_loop_unroll_02_ams_r8(min_r8, max_r8, x_r8, n_i8)
                    case (11)
                        call minmax_loop_unroll_04_ams_r8(min_r8, max_r8, x_r8, n_i8)
                    case (12)
                        call minmax_loop_unroll_08_ams_r8(min_r8, max_r8, x_r8, n_i8)
                    case (13)
                        call minmax_loop_unroll_16_ams_r8(min_r8, max_r8, x_r8, n_i8)
                    case (14)
                        call minmax_loop_unroll_32_ams_r8(min_r8, max_r8, x_r8, n_i8)
                    case (15)
                        call minmax_loop_unroll_64_ams_r8(min_r8, max_r8, x_r8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print *, types(iter_types), n_i8, real(times(iter_types)), min_r8, max_r8
        end do
        write(10,*) k, n_i8, times

        print*, '============================================================='
        do iter_types=1, n_types, 1
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case (1)
                        min_i8 = minval(x_i8)
                        max_i8 = maxval(x_i8)
                    case (2)
                        call minmax_loop_f_i8(min_i8, max_i8, x_i8, n_i8)
                    case (3)
                        call minmax_loop_unroll_04_f_i8(min_i8, max_i8, x_i8, n_i8)
                    case (4)
                        call minmax_loop_unroll_08_f_i8(min_i8, max_i8, x_i8, n_i8)
                    case (5)
                        call minmax_loop_unroll_14_f_i8(min_i8, max_i8, x_i8, n_i8)
                    case (6)
                        call minmax_loop_c_i8(min_i8, max_i8, x_i8, n_i8)
                    case (7)
                        call minmax_loop_unroll_04_c_i8(min_i8, max_i8, x_i8, n_i8)
                    case (8)
                        call minmax_loop_unroll_04_c_i8(min_i8, max_i8, x_i8, n_i8)
                    case (9)
                        call minmax_loop_unroll_04_c_i8(min_i8, max_i8, x_i8, n_i8)
                    case (10)
                        call minmax_loop_unroll_02_ams_i8(min_i8, max_i8, x_i8, n_i8)
                    case (11)
                        call minmax_loop_unroll_04_ams_i8(min_i8, max_i8, x_i8, n_i8)
                    case (12)
                        call minmax_loop_unroll_08_ams_i8(min_i8, max_i8, x_i8, n_i8)
                    case (13)
                        call minmax_loop_unroll_16_ams_i8(min_i8, max_i8, x_i8, n_i8)
                    case (14)
                        call minmax_loop_unroll_32_ams_i8(min_i8, max_i8, x_i8, n_i8)
                    case (15)
                        call minmax_loop_unroll_64_ams_i8(min_i8, max_i8, x_i8, n_i8)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print *, types(iter_types), n_i8, real(times(iter_types)), min_i8, max_i8
        end do
        write(20,*) k, n_i8, times


        deallocate(x_r8, x_i8)
        ! stop
    end do


contains

    subroutine minmax_loop_f_r8(min, max, x, n)
        implicit none
        real(kind=8), intent(inout) :: min, max
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n

        real(kind=8)    :: tmp_x
        integer(kind=8) :: i
        include "./inc_minmax_loop_f.f90"
    end subroutine minmax_loop_f_r8

    subroutine minmax_loop_f_i8(min, max, x, n)
        implicit none
        integer(kind=8), intent(inout) :: min, max
        integer(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in)    :: n

        integer(kind=8) :: tmp_x
        integer(kind=8) :: i
        include "./inc_minmax_loop_f.f90"
    end subroutine minmax_loop_f_i8

    subroutine minmax_loop_unroll_04_f_r8(min, max, x, n)
        implicit none
        real(kind=8), intent(inout) :: min, max
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n

        integer(kind=8) :: num_unroll, i
        real(kind=8) :: tmp_x
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        r14 = huge(0d0)
        r15 = - huge(0d0)        
        include "./inc_minmax_loop_unroll_04_f.f90"
    end subroutine minmax_loop_unroll_04_f_r8

    subroutine minmax_loop_unroll_04_f_i8(min, max, x, n)
        implicit none
        integer(kind=8), intent(inout) :: min, max
        integer(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in)    :: n

        integer(kind=8) :: num_unroll, i
        integer(kind=8) :: tmp_x
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        r14 = huge(0_8)
        r15 = - huge(0_8)        
        include "./inc_minmax_loop_unroll_04_f.f90"
    end subroutine minmax_loop_unroll_04_f_i8

    subroutine minmax_loop_unroll_08_f_r8(min, max, x, n)
        implicit none
        real(kind=8), intent(inout) :: min, max
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n

        integer(kind=8) :: num_unroll, i
        real(kind=8) :: tmp_x
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        r14 = huge(0d0)
        r15 = - huge(0d0)        
        include "./inc_minmax_loop_unroll_08_f.f90"
    end subroutine minmax_loop_unroll_08_f_r8

    subroutine minmax_loop_unroll_08_f_i8(min, max, x, n)
        implicit none
        integer(kind=8), intent(inout) :: min, max
        integer(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in)    :: n

        integer(kind=8) :: num_unroll, i
        integer(kind=8) :: tmp_x
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        r14 = huge(0_8)
        r15 = - huge(0_8)        
        include "./inc_minmax_loop_unroll_08_f.f90"
    end subroutine minmax_loop_unroll_08_f_i8

    subroutine minmax_loop_unroll_14_f_r8(min, max, x, n)
        implicit none
        real(kind=8), intent(inout) :: min, max
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n

        integer(kind=8) :: num_unroll, i
        real(kind=8) :: tmp_x
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        r14 = huge(0d0)
        r15 = - huge(0d0)        
        include "./inc_minmax_loop_unroll_14_f.f90"
    end subroutine minmax_loop_unroll_14_f_r8

    subroutine minmax_loop_unroll_14_f_i8(min, max, x, n)
        implicit none
        integer(kind=8), intent(inout) :: min, max
        integer(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in)    :: n

        integer(kind=8) :: num_unroll, i
        integer(kind=8) :: tmp_x
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        r14 = huge(0_8)
        r15 = - huge(0_8)        
        include "./inc_minmax_loop_unroll_14_f.f90"
    end subroutine minmax_loop_unroll_14_f_i8




end program main_minmax
