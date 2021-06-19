program main_sum_up_vector
    use mod_timer
    use Iso_C_Binding
    use ISO_FORTRAN_ENV    
    use mod_stats
    implicit none

    Interface
        ! -------------------------------------------------------------------
        ! -------------------------------------------------------------------
        function sum_with_index_loop_r8_c_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_r8_c_')
            Import
            Real(c_double) :: sum_with_index_loop_r8_c_
            Integer(c_int64_t),Value :: n_x
            Integer(c_int64_t),Value :: n_ids
            Real(c_double),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function    

        function sum_with_index_loop_i8_c_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_with_index_loop_i8_c_
            Integer(c_int64_t),Value   :: n_x
            Integer(c_int64_t),Value   :: n_ids
            Integer(c_int64_t),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function        

        function sum_with_index_loop_02_r8_c_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_02_r8_c_')
            Import
            Real(c_double) :: sum_with_index_loop_02_r8_c_
            Integer(c_int64_t),Value :: n_x
            Integer(c_int64_t),Value :: n_ids
            Real(c_double),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function    

        function sum_with_index_loop_02_i8_c_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_02_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_with_index_loop_02_i8_c_
            Integer(c_int64_t),Value   :: n_x
            Integer(c_int64_t),Value   :: n_ids
            Integer(c_int64_t),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function        

        function sum_with_index_loop_04_r8_c_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_04_r8_c_')
            Import
            Real(c_double) :: sum_with_index_loop_04_r8_c_
            Integer(c_int64_t),Value :: n_x
            Integer(c_int64_t),Value :: n_ids
            Real(c_double),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function    

        function sum_with_index_loop_04_i8_c_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_04_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_with_index_loop_04_i8_c_
            Integer(c_int64_t),Value   :: n_x
            Integer(c_int64_t),Value   :: n_ids
            Integer(c_int64_t),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function        

        function sum_with_index_loop_08_r8_c_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_08_r8_c_')
            Import
            Real(c_double) :: sum_with_index_loop_08_r8_c_
            Integer(c_int64_t),Value :: n_x
            Integer(c_int64_t),Value :: n_ids
            Real(c_double),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function    

        function sum_with_index_loop_08_i8_c_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_08_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_with_index_loop_08_i8_c_
            Integer(c_int64_t),Value   :: n_x
            Integer(c_int64_t),Value   :: n_ids
            Integer(c_int64_t),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function        

        function sum_with_index_loop_16_r8_c_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_16_r8_c_')
            Import
            Real(c_double) :: sum_with_index_loop_16_r8_c_
            Integer(c_int64_t),Value :: n_x
            Integer(c_int64_t),Value :: n_ids
            Real(c_double),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function    

        function sum_with_index_loop_16_i8_c_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_16_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_with_index_loop_16_i8_c_
            Integer(c_int64_t),Value   :: n_x
            Integer(c_int64_t),Value   :: n_ids
            Integer(c_int64_t),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function  

        function sum_with_index_loop_r8_asm_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_r8_asm_')
            Import
            Real(c_double) :: sum_with_index_loop_r8_asm_
            Integer(c_int64_t),Value :: n_x
            Integer(c_int64_t),Value :: n_ids
            Real(c_double),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function    

        function sum_with_index_loop_i8_asm_(x,ids,n_x,n_ids) Bind(C,Name='sum_with_index_loop_i8_asm_')
            Import
            Integer(c_int64_t)            :: sum_with_index_loop_i8_asm_
            Integer(c_int64_t),Value   :: n_x
            Integer(c_int64_t),Value   :: n_ids
            Integer(c_int64_t),Intent(In) :: x(n_x)
            Integer(c_int64_t),Intent(In) :: ids(n_ids)
        end function        
    end Interface



    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(c_int64_t), allocatable :: indices(:), indices_full(:)
    real(c_double), allocatable :: mask(:), tmp(:), tmp_x(:)
    real(c_float), target, allocatable :: x_r4(:)
    real(c_double), target, allocatable :: x_r8(:), x_subset_r8(:)
    integer(c_int32_t), target, allocatable :: x_i4(:)
    integer(c_int64_t), target, allocatable :: x_i8(:), j_i8(:), idx_i8(:), x_subset_i8(:)
    integer(c_int64_t), target :: res_i8
    real(c_double), target     :: res_r8
    real(c_double) :: res_min1, res_min2, res_min3, res_min4, res_min5, res_min6, res_min7, res_min8
    real(c_double) :: res_max1, res_max2, res_max3, res_max4, res_max5, res_max6, res_max7, res_max8
    real(c_double) :: x_edge2(2)
    real(kind=8) :: time1, time2, time3, time4, time5, time6, time7, time8, NFLOP
    real(kind=8), ALLOCATABLE :: times(:)
    integer(kind=8) :: iter, n_iter, n_i8, k, n_x, n_i, idx, i, n_types, iter_types, c_i8, i_i8, iii
    integer(kind=4) :: n_i4
    character(len=30), ALLOCATABLE :: types(:)

    type(c_ptr) :: res_r8_ptr, res_i8_ptr, x_r8_ptr, x_i8_ptr


    n_types = 13
    allocate(times(0:n_types))
    allocate(types(0:n_types))
    types(0)  = "sum_all_loop_F           :"
    types(1)  = "sum_loop_F               :"
    types(2)  = "sum_loop_unroll_02_F     :"
    types(3)  = "sum_loop_unroll_04_F     :"
    types(4)  = "sum_loop_unroll_08_F     :"
    types(5)  = "sum_loop_unroll_15_F     :"
    types(6)  = "sum_loop_unroll_16_F     :"
    types(7)  = "sum_loop_unroll_32_F     :"
    types(8)  = "sum_loop_C               :"
    types(9)  = "sum_loop_unroll_02_C     :"
    types(10) = "sum_loop_unroll_04_C     :"
    types(11) = "sum_loop_unroll_08_C     :"
    types(12) = "sum_loop_unroll_16_C     :"
    types(13) = "sum_loop_unroll_04_ASM   :"

    open(10, file="time_sum_Xx64_r8.csv")
    open(20, file="time_sum_Xx64_i8.csv")
    do k=120, 200, 1
        n_i8 = maxval((/2**(k/dble(8)), 4d0/))
        n_i8 = 9000000
        i_i8 = n_i8/2_8


        allocate(x_r8(n_i8), x_subset_r8(i_i8))
        allocate(x_i8(n_i8), x_subset_i8(i_i8))
        allocate(j_i8(n_i8))
        allocate(idx_i8(i_i8))
        do iii=1, n_i8, 1
            j_i8(iii) = iii
        end do
        call permutation(j_i8, n_i8)
        idx_i8 = j_i8(1:i_i8)
        call quick_sort_i8(idx_i8, i_i8)

        call random_number(x_r8)
        x_r8 = 10 * x_r8
        x_i8 = x_r8
        x_subset_r8 = x_r8(j_i8)
        x_subset_i8 = x_i8(j_i8)
        n_iter=maxval((/10000000000_8/n_i8, 1_8/))
        n_iter=maxval((/500000000_8/n_i8, 1_8/))
        n_iter=maxval((/50000000_8/n_i8, 1_8/))
        n_iter=28

        print*, '============================================================='
        do iter_types=0, n_types, 1
            res_r8=0d0
            call date_and_time(values=date_value1)
            x_subset_r8 = x_r8(j_i8)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (0); res_r8 = sum_up(x_subset_r8, i_i8)
                    case  (1); res_r8 = sum_up_with_index_loop_r8(x_r8, idx_i8, n_i8, i_i8)
                    case  (2); res_r8 = sum_up_with_index_loop_unroll_02_r8(x_r8, idx_i8, n_i8, i_i8)
                    case  (3); res_r8 = sum_up_with_index_loop_unroll_04_r8(x_r8, idx_i8, n_i8, i_i8)
                    case  (4); res_r8 = sum_up_with_index_loop_unroll_08_r8(x_r8, idx_i8, n_i8, i_i8)
                    case  (5); res_r8 = sum_up_with_index_loop_unroll_15_r8(x_r8, idx_i8, n_i8, i_i8)
                    case  (6); res_r8 = sum_up_with_index_loop_unroll_16_r8(x_r8, idx_i8, n_i8, i_i8)
                    case  (7); res_r8 = sum_up_with_index_loop_unroll_32_r8(x_r8, idx_i8, n_i8, i_i8)
                    case  (8); res_r8 = sum_with_index_loop_r8_c_(x_r8, idx_i8, n_i8, i_i8)
                    case  (9); res_r8 = sum_with_index_loop_02_r8_c_(x_r8, idx_i8, n_i8, i_i8)
                    case (10); res_r8 = sum_with_index_loop_04_r8_c_(x_r8, idx_i8, n_i8, i_i8)
                    case (11); res_r8 = sum_with_index_loop_08_r8_c_(x_r8, idx_i8, n_i8, i_i8)
                    case (12); res_r8 = sum_with_index_loop_16_r8_c_(x_r8, idx_i8, n_i8, i_i8)
                    case (13); res_r8 = sum_with_index_loop_r8_asm_(x_r8, idx_i8, n_i8, i_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, f30.15)", types(iter_types), n_i8, real(times(iter_types)), res_r8
        end do
        write(10,*) k, n_i8, times

        print*, '============================================================='
        do iter_types=0, n_types, 1
            res_i8=0d0
            call date_and_time(values=date_value1)
            x_subset_i8 = x_i8(j_i8)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (0); res_i8 = sum_up(x_subset_i8, i_i8)
                    case  (1); res_i8 = sum_up_with_index_loop_i8(x_i8, idx_i8, n_i8, i_i8)
                    case  (2); res_i8 = sum_up_with_index_loop_unroll_02_i8(x_i8, idx_i8, n_i8, i_i8)
                    case  (3); res_i8 = sum_up_with_index_loop_unroll_04_i8(x_i8, idx_i8, n_i8, i_i8)
                    case  (4); res_i8 = sum_up_with_index_loop_unroll_08_i8(x_i8, idx_i8, n_i8, i_i8)
                    case  (5); res_i8 = sum_up_with_index_loop_unroll_15_i8(x_i8, idx_i8, n_i8, i_i8)
                    case  (6); res_i8 = sum_up_with_index_loop_unroll_16_i8(x_i8, idx_i8, n_i8, i_i8)
                    case  (7); res_i8 = sum_up_with_index_loop_unroll_32_i8(x_i8, idx_i8, n_i8, i_i8)
                    case  (8); res_i8 = sum_with_index_loop_i8_c_(x_i8, idx_i8, n_i8, i_i8)
                    case  (9); res_i8 = sum_with_index_loop_02_i8_c_(x_i8, idx_i8, n_i8, i_i8)
                    case (10); res_i8 = sum_with_index_loop_04_i8_c_(x_i8, idx_i8, n_i8, i_i8)
                    case (11); res_i8 = sum_with_index_loop_08_i8_c_(x_i8, idx_i8, n_i8, i_i8)
                    case (12); res_i8 = sum_with_index_loop_16_i8_c_(x_i8, idx_i8, n_i8, i_i8)
                    case (13); res_i8 = sum_with_index_loop_i8_asm_(x_i8, idx_i8, n_i8, i_i8)
                end select
            end do
            call date_and_time(values=date_value2); times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, i15)", types(iter_types), n_i8, real(times(iter_types)), res_i8
        end do
        write(20,*) k, n_i8, times


        deallocate(x_r8, x_i8, j_i8, idx_i8)
        deallocate(x_subset_r8, x_subset_i8)
        call sleep(5)
    end do


contains

    function sum_up_with_index_loop_r8(x, ids, n_x, n_ids)
        implicit none
        real(kind=8), intent(in)    :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        real(kind=8)                :: sum_up_with_index_loop_r8

        integer(kind=8) :: i
        real(kind=8)    :: tmp
        include "./inc_sum_up_vector_with_index_loop.f90"
        sum_up_with_index_loop_r8 = tmp
    end function sum_up_with_index_loop_r8

    function sum_up_with_index_loop_i8(x, ids, n_x, n_ids)
        implicit none
        integer(kind=8), intent(in) :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        integer(kind=8)             :: sum_up_with_index_loop_i8

        integer(kind=8) :: i
        integer(kind=8) :: tmp
        include "./inc_sum_up_vector_with_index_loop.f90"
        sum_up_with_index_loop_i8 = tmp
    end function sum_up_with_index_loop_i8

    function sum_up_with_index_loop_unroll_02_r8(x, ids, n_x, n_ids)
        implicit none
        real(kind=8), intent(in)    :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        real(kind=8)                :: sum_up_with_index_loop_unroll_02_r8

        integer(kind=8) :: i, n_ids_unroll
        real(kind=8)    :: r00, r01
        real(kind=8)    :: r15
        include "./inc_sum_up_vector_with_index_loop_unroll_02.f90"
        sum_up_with_index_loop_unroll_02_r8 = r15
    end function sum_up_with_index_loop_unroll_02_r8

    function sum_up_with_index_loop_unroll_02_i8(x, ids, n_x, n_ids)
        implicit none
        integer(kind=8), intent(in) :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        integer(kind=8)             :: sum_up_with_index_loop_unroll_02_i8

        integer(kind=8) :: i, n_ids_unroll
        integer(kind=8) :: r00, r01
        integer(kind=8) :: r15
        include "./inc_sum_up_vector_with_index_loop_unroll_02.f90"
        sum_up_with_index_loop_unroll_02_i8 = r15
    end function sum_up_with_index_loop_unroll_02_i8

    function sum_up_with_index_loop_unroll_04_r8(x, ids, n_x, n_ids)
        implicit none
        real(kind=8), intent(in)    :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        real(kind=8)                :: sum_up_with_index_loop_unroll_04_r8

        integer(kind=8) :: i, n_ids_unroll
        real(kind=8)    :: r00, r01, r02, r03
        real(kind=8)    :: r15
        include "./inc_sum_up_vector_with_index_loop_unroll_04.f90"
        sum_up_with_index_loop_unroll_04_r8 = r15
    end function sum_up_with_index_loop_unroll_04_r8

    function sum_up_with_index_loop_unroll_04_i8(x, ids, n_x, n_ids)
        implicit none
        integer(kind=8), intent(in) :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        integer(kind=8)             :: sum_up_with_index_loop_unroll_04_i8

        integer(kind=8) :: i, n_ids_unroll
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r15
        include "./inc_sum_up_vector_with_index_loop_unroll_04.f90"
        sum_up_with_index_loop_unroll_04_i8 = r15
    end function sum_up_with_index_loop_unroll_04_i8

    function sum_up_with_index_loop_unroll_08_r8(x, ids, n_x, n_ids)
        implicit none
        real(kind=8), intent(in)    :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        real(kind=8)                :: sum_up_with_index_loop_unroll_08_r8

        integer(kind=8) :: i, n_ids_unroll
        real(kind=8)    :: r00, r01, r02, r03
        real(kind=8)    :: r04, r05, r06, r07
        real(kind=8)    :: r15
        include "./inc_sum_up_vector_with_index_loop_unroll_08.f90"
        sum_up_with_index_loop_unroll_08_r8 = r15
    end function sum_up_with_index_loop_unroll_08_r8

    function sum_up_with_index_loop_unroll_08_i8(x, ids, n_x, n_ids)
        implicit none
        integer(kind=8), intent(in) :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        integer(kind=8)             :: sum_up_with_index_loop_unroll_08_i8

        integer(kind=8) :: i, n_ids_unroll
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r15
        include "./inc_sum_up_vector_with_index_loop_unroll_08.f90"
        sum_up_with_index_loop_unroll_08_i8 = r15
    end function sum_up_with_index_loop_unroll_08_i8

    function sum_up_with_index_loop_unroll_15_r8(x, ids, n_x, n_ids)
        implicit none
        real(kind=8), intent(in)    :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        real(kind=8)                :: sum_up_with_index_loop_unroll_15_r8

        integer(kind=8) :: i, n_ids_unroll
        real(kind=8)    :: tmp
        real(kind=8)    :: r00, r01, r02, r03
        real(kind=8)    :: r04, r05, r06, r07
        real(kind=8)    :: r08, r09, r10, r11
        real(kind=8)    :: r12, r13, r14, r15
        include "./inc_sum_up_vector_with_index_loop_unroll_15.f90"
        sum_up_with_index_loop_unroll_15_r8 = r15
    end function sum_up_with_index_loop_unroll_15_r8

    function sum_up_with_index_loop_unroll_15_i8(x, ids, n_x, n_ids)
        implicit none
        integer(kind=8), intent(in) :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        integer(kind=8)             :: sum_up_with_index_loop_unroll_15_i8

        integer(kind=8) :: i, n_ids_unroll
        integer(kind=8) :: tmp
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_up_vector_with_index_loop_unroll_15.f90"
        sum_up_with_index_loop_unroll_15_i8 = r15
    end function sum_up_with_index_loop_unroll_15_i8

    function sum_up_with_index_loop_unroll_16_r8(x, ids, n_x, n_ids)
        implicit none
        real(kind=8), intent(in)    :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        real(kind=8)                :: sum_up_with_index_loop_unroll_16_r8

        integer(kind=8) :: i, n_ids_unroll
        real(kind=8)    :: tmp
        real(kind=8)    :: r00, r01, r02, r03
        real(kind=8)    :: r04, r05, r06, r07
        real(kind=8)    :: r08, r09, r10, r11
        real(kind=8)    :: r12, r13, r14, r15
        include "./inc_sum_up_vector_with_index_loop_unroll_16.f90"
        sum_up_with_index_loop_unroll_16_r8 = r15
    end function sum_up_with_index_loop_unroll_16_r8

    function sum_up_with_index_loop_unroll_16_i8(x, ids, n_x, n_ids)
        implicit none
        integer(kind=8), intent(in) :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        integer(kind=8)             :: sum_up_with_index_loop_unroll_16_i8

        integer(kind=8) :: i, n_ids_unroll
        integer(kind=8) :: tmp
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_up_vector_with_index_loop_unroll_16.f90"
        sum_up_with_index_loop_unroll_16_i8 = r15
    end function sum_up_with_index_loop_unroll_16_i8

    function sum_up_with_index_loop_unroll_32_r8(x, ids, n_x, n_ids)
        implicit none
        real(kind=8), intent(in)    :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        real(kind=8)                :: sum_up_with_index_loop_unroll_32_r8

        integer(kind=8) :: i, n_ids_unroll
        real(kind=8)    :: tmp
        real(kind=8)    :: r00, r01, r02, r03
        real(kind=8)    :: r04, r05, r06, r07
        real(kind=8)    :: r08, r09, r10, r11
        real(kind=8)    :: r12, r13, r14, r15
        real(kind=8)    :: r16, r17, r18, r19
        real(kind=8)    :: r20, r21, r22, r23
        real(kind=8)    :: r24, r25, r26, r27
        real(kind=8)    :: r28, r29, r31, r32
        include "./inc_sum_up_vector_with_index_loop_unroll_16.f90"
        sum_up_with_index_loop_unroll_32_r8 = r15
    end function sum_up_with_index_loop_unroll_32_r8

    function sum_up_with_index_loop_unroll_32_i8(x, ids, n_x, n_ids)
        implicit none
        integer(kind=8), intent(in) :: x(n_x)
        integer(kind=8), intent(in) :: ids(n_ids)
        integer(kind=8), intent(in) :: n_x, n_ids
        integer(kind=8)             :: sum_up_with_index_loop_unroll_32_i8

        integer(kind=8) :: i, n_ids_unroll
        integer(kind=8) :: tmp
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15
        integer(kind=8) :: r16, r17, r18, r19
        integer(kind=8) :: r20, r21, r22, r23
        integer(kind=8) :: r24, r25, r26, r27
        integer(kind=8) :: r28, r29, r31, r32
        include "./inc_sum_up_vector_with_index_loop_unroll_16.f90"
        sum_up_with_index_loop_unroll_32_i8 = r15
    end function sum_up_with_index_loop_unroll_32_i8

end program main_sum_up_vector
