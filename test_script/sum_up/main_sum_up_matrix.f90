program main_sum_up_vector
    use mod_timer
    use Iso_C_Binding
    use ISO_FORTRAN_ENV    
    use mod_stats
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

        function sum_unroll_30_r8_c_(x,n) Bind(C,Name='sum_unroll_30_r8_c_')
            Import
            Real(c_double)            :: sum_unroll_30_r8_c_
            Integer(c_int64_t),Value  :: n
            Real(c_double),Intent(In) :: x(n)
        end function

        function sum_unroll_30_i8_c_(x,n) Bind(C,Name='sum_unroll_30_i8_c_')
            Import
            Integer(c_int64_t)            :: sum_unroll_30_i8_c_
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

        function sum_up_04_i8(x,n) Bind(C,Name='sum_up_04_i8')
            Import
            Integer(c_int64_t)            :: sum_up_04_i8
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

        function sum_assembl_r8_16_C_ver02_(x,n) Bind(C,Name='sum_assembl_r8_16_C_ver02_')
            Import
            Real(c_double) :: sum_assembl_r8_16_C_ver02_
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

        subroutine sum_up_matrix_naive_r8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_naive_r8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_naive_r8_C

        subroutine sum_up_matrix_naive_i8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_naive_i8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_naive_i8_C

        subroutine sum_up_matrix_unroll_02_r8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_02_r8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_02_r8_C

        subroutine sum_up_matrix_unroll_02_i8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_02_i8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_02_i8_C

        subroutine sum_up_matrix_unroll_04_r8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_04_r8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_r8_C

        subroutine sum_up_matrix_unroll_04_i8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_04_i8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_i8_C

        subroutine sum_up_matrix_unroll_08_r8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_08_r8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_r8_C

        subroutine sum_up_matrix_unroll_08_i8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_08_i8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_i8_C

        subroutine sum_up_matrix_unroll_15_r8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_15_r8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_15_r8_C

        subroutine sum_up_matrix_unroll_15_i8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_15_i8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_15_i8_C

        subroutine sum_up_matrix_unroll_04_r8_ASM(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_04_r8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_r8_ASM

        subroutine sum_up_matrix_unroll_04_i8_ASM(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_04_i8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_i8_ASM

        subroutine sum_up_matrix_unroll_08_r8_ASM(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_08_r8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_r8_ASM

        subroutine sum_up_matrix_unroll_08_i8_ASM(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_08_i8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_i8_ASM

        subroutine sum_up_matrix_unroll_16_r8_ASM(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_16_r8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_16_r8_ASM

        subroutine sum_up_matrix_unroll_16_i8_ASM(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_16_i8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_16_i8_ASM

        subroutine sum_up_matrix_unroll_32_r8_ASM(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_32_r8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_32_r8_ASM

        subroutine sum_up_matrix_unroll_32_i8_ASM(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_32_i8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_32_i8_ASM

        subroutine sum_up_matrix_unroll_04_02_r8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_04_02_r8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_02_r8_C

        subroutine sum_up_matrix_unroll_04_02_i8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_04_02_i8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_02_i8_C

        subroutine sum_up_matrix_unroll_08_02_r8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_08_02_r8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_02_r8_C

        subroutine sum_up_matrix_unroll_08_02_i8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_08_02_i8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_02_i8_C

        subroutine sum_up_matrix_unroll_02_08_r8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_02_08_r8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_02_08_r8_C

        subroutine sum_up_matrix_unroll_02_08_i8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_02_08_i8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_02_08_i8_C

        subroutine sum_up_matrix_unroll_04_04_r8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_04_04_r8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_04_r8_C

        subroutine sum_up_matrix_unroll_04_04_i8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_04_04_i8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_04_i8_C

        subroutine sum_up_matrix_unroll_02_04_r8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_02_04_r8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_02_04_r8_C

        subroutine sum_up_matrix_unroll_02_04_i8_C(x_sum, x, n, c) bind(c, name='sum_up_matrix_unroll_02_04_i8_C')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_02_04_i8_C

        subroutine sum_up_matrix_unroll_04_04_r8_ASM(x_sum, x, n, c) &
            bind(c, name='sum_up_matrix_unroll_04_04_r8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_04_r8_ASM

        subroutine sum_up_matrix_unroll_04_04_i8_ASM(x_sum, x, n, c) &
            bind(c, name='sum_up_matrix_unroll_04_04_i8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_04_i8_ASM

        subroutine sum_up_matrix_unroll_08_04_r8_ASM(x_sum, x, n, c) &
            bind(c, name='sum_up_matrix_unroll_08_04_r8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_04_r8_ASM

        subroutine sum_up_matrix_unroll_08_04_i8_ASM(x_sum, x, n, c) &
            bind(c, name='sum_up_matrix_unroll_08_04_i8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_04_i8_ASM

        subroutine sum_up_matrix_unroll_08_04_r8_ASM_Parallel(x_sum, x, n, c) & 
            bind(c, name='sum_up_matrix_unroll_08_04_r8_ASM_Parallel')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_04_r8_ASM_Parallel

        subroutine sum_up_matrix_unroll_08_04_i8_ASM_Parallel(x_sum, x, n, c) & 
            bind(c, name='sum_up_matrix_unroll_08_04_i8_ASM_Parallel')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_04_i8_ASM_Parallel

        subroutine sum_up_matrix_unroll_04_02_r8_ASM(x_sum, x, n, c) & 
            bind(c, name='sum_up_matrix_unroll_04_02_r8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_02_r8_ASM

        subroutine sum_up_matrix_unroll_04_02_i8_ASM(x_sum, x, n, c) & 
            bind(c, name='sum_up_matrix_unroll_04_02_i8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_02_i8_ASM

        subroutine sum_up_matrix_unroll_04_08_r8_ASM(x_sum, x, n, c) & 
            bind(c, name='sum_up_matrix_unroll_04_08_r8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_08_r8_ASM

        subroutine sum_up_matrix_unroll_04_08_i8_ASM(x_sum, x, n, c) & 
            bind(c, name='sum_up_matrix_unroll_04_08_i8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_04_08_i8_ASM

        subroutine sum_up_matrix_unroll_08_02_r8_ASM(x_sum, x, n, c) & 
            bind(c, name='sum_up_matrix_unroll_08_02_r8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_02_r8_ASM

        subroutine sum_up_matrix_unroll_08_02_i8_ASM(x_sum, x, n, c) & 
            bind(c, name='sum_up_matrix_unroll_08_02_i8_ASM')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_08_02_i8_ASM

        subroutine sum_up_matrix_unroll_02_08_r8_C_P(x_sum, x, n, c) & 
            bind(c, name='sum_up_matrix_unroll_02_08_r8_C_P')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_02_08_r8_C_P

        subroutine sum_up_matrix_unroll_02_08_i8_C_P(x_sum, x, n, c) & 
            bind(c, name='sum_up_matrix_unroll_02_08_i8_C_P')
            import :: c_ptr, c_int64_t
            type(c_ptr), value    :: x_sum, x
            integer(c_int64_t), value :: n, c
        end subroutine sum_up_matrix_unroll_02_08_i8_C_P

    end interface

    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(c_int64_t), allocatable :: indices(:), indices_full(:)
    real(c_double), allocatable :: mask(:), tmp(:), tmp_x(:)
    real(c_float), target, allocatable :: x_r4(:,:)
    real(c_double), target, allocatable :: x_r8(:,:)
    integer(c_int32_t), target, allocatable :: x_i4(:,:)
    integer(c_int64_t), target, allocatable :: x_i8(:,:)
    integer(c_int64_t), target, ALLOCATABLE :: res_i8(:)
    real(c_double), target, ALLOCATABLE     :: res_r8(:)
    real(c_double) :: res_min1, res_min2, res_min3, res_min4, res_min5, res_min6, res_min7, res_min8
    real(c_double) :: res_max1, res_max2, res_max3, res_max4, res_max5, res_max6, res_max7, res_max8
    real(c_double) :: x_edge2(2)
    real(kind=8) :: time1, time2, time3, time4, time5, time6, time7, time8, NFLOP
    real(kind=8), ALLOCATABLE :: times(:)
    integer(kind=8) :: iter, n_iter, n_i8, k, n_x, n_i, idx, i, n_types, iter_types, c_i8
    integer(kind=4) :: n_i4
    character(len=30), ALLOCATABLE :: types(:)

    type(c_ptr) :: res_r8_ptr, res_i8_ptr, x_r8_ptr, x_i8_ptr


    n_types = 37
    allocate(times(n_types))
    allocate(types(n_types))
    types(1)  = "sum_intrinsic_F      :"
    types(2)  = "sum_naive_F          :"
    types(3)  = "sum_unroll_02_F      :"
    types(4)  = "sum_unroll_04_F      :"
    types(5)  = "sum_unroll_08_F      :"
    types(6)  = "sum_unroll_15_F      :"
    types(7)  = "sum_unroll_30_F      :"
    types(8)  = "sum_unroll_04_04_F   :" !
    types(9)  = "sum_unroll_08_02_F   :" !
    types(10) = "sum_unroll_02_08_F   :" !x
    types(11) = "sum_unroll_04_02_F   :" !
    types(12) = "sum_unroll_02_04_F   :" !x
    types(13) = "sum_naive_C          :"
    types(14) = "sum_unroll_02_C      :"
    types(15) = "sum_unroll_04_C      :"
    types(16) = "sum_unroll_08_C      :"
    types(17) = "sum_unroll_15_C      :"
    types(18) = "sum_unroll_04_ASM    :"
    types(19) = "sum_unroll_08_ASM    :"
    types(20) = "sum_unroll_16_ASM    :"
    types(21) = "sum_unroll_32_ASM    :"
    types(22) = "sum_unroll_04_04_C   :"
    types(23) = "sum_unroll_08_02_C   :"
    types(24) = "sum_unroll_02_08_C   :"
    types(25) = "sum_unroll_04_02_C   :"
    types(26) = "sum_unroll_02_04_C   :"
    types(27) = "sum_unroll_04_04_ASM :" !
    types(28) = "sum_unroll_08_02_ASM :" !
    types(29) = "sum_unroll_04_08_ASM :"
    types(30) = "sum_unroll_04_02_ASM :" !
    types(31) = "sum_vector_loop      :" !
    types(32) = "sum_vector_loop_para2:" !
    types(33) = "sum_vector_loop_para4:" !
    types(34) = "sum_vector_loop_para8:" !
    types(35) = "sum_of_matrix        :" !
    types(36) = "sum_unroll_02_08_C_P :"
    types(37) = "sum_unroll_04_04_F_P :"

    c_i8 = 64
    allocate(res_r8(c_i8))
    allocate(res_i8(c_i8))

    open(10, file="time_sum_Xx64_r8.csv")
    open(20, file="time_sum_Xx64_i8.csv")
    do k=120, 200, 1
        n_i8 = maxval((/2**(k/dble(8)), 4d0/))
        n_i4 = n_i8
        allocate(x_r8(n_i8,c_i8))
        allocate(x_i8(n_i8,c_i8))
        call random_number(x_r8)
        x_r8 = 10 * x_r8
        x_i8 = x_r8
        n_iter=maxval((/10000000000_8/n_i8/c_i8, 1_8/))
        n_iter=maxval((/500000000_8/n_i8/c_i8, 1_8/))
        ! n_iter=maxval((/50000000_8/n_i8/c_i8, 1_8/))
        ! n_iter=1

        res_r8_ptr = c_loc(res_r8)
        res_i8_ptr = c_loc(res_i8)
        x_r8_ptr = c_loc(x_r8)
        x_i8_ptr = c_loc(x_i8)

        print*, '============================================================='
        do iter_types=1, n_types, 1
            res_r8=0d0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case (1)
                            res_r8 =  sum(x_r8, dim=1)
                    case (2)
                            call sum_up_matrix_naive_r8(res_r8, x_r8, n_i8, c_i8)
                    case (3)
                            call sum_up_matrix_unroll_02_r8(res_r8, x_r8, n_i8, c_i8)
                    case (4)
                            call sum_up_matrix_unroll_04_r8(res_r8, x_r8, n_i8, c_i8)
                    case (5)
                            call sum_up_matrix_unroll_08_r8(res_r8, x_r8, n_i8, c_i8)
                    case (6)
                            call sum_up_matrix_unroll_15_r8(res_r8, x_r8, n_i8, c_i8)
                    case (7)
                            call sum_up_matrix_unroll_30_r8(res_r8, x_r8, n_i8, c_i8)
                    case (8)
                            call sum_up_matrix_unroll_04_04_r8(res_r8, x_r8, n_i8, c_i8)
                    case (9)
                            call sum_up_matrix_unroll_08_02_r8(res_r8, x_r8, n_i8, c_i8)
                    case (10)
                            call sum_up_matrix_unroll_02_08_r8(res_r8, x_r8, n_i8, c_i8)
                    case (11)
                            call sum_up_matrix_unroll_04_02_r8(res_r8, x_r8, n_i8, c_i8)
                    case (12)
                            call sum_up_matrix_unroll_02_04_r8(res_r8, x_r8, n_i8, c_i8)
                    case (13)
                            call sum_up_matrix_naive_r8_C(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (14)
                            call sum_up_matrix_unroll_02_r8_C(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (15)
                            call sum_up_matrix_unroll_04_r8_C(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (16)
                            call sum_up_matrix_unroll_08_r8_C(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (17)
                            call sum_up_matrix_unroll_15_r8_C(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (18)
                            call sum_up_matrix_unroll_04_r8_ASM(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (19)
                            call sum_up_matrix_unroll_08_r8_ASM(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (20)
                            call sum_up_matrix_unroll_16_r8_ASM(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (21)
                            call sum_up_matrix_unroll_32_r8_ASM(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (22)
                            call sum_up_matrix_unroll_04_04_r8_C(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (23)
                            call sum_up_matrix_unroll_08_02_r8_C(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (24)
                            call sum_up_matrix_unroll_02_08_r8_C(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (25)
                            call sum_up_matrix_unroll_04_02_r8_C(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (26)
                            call sum_up_matrix_unroll_02_04_r8_C(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (27)
                            call sum_up_matrix_unroll_04_04_r8_ASM(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (28)
                            call sum_up_matrix_unroll_08_02_r8_ASM(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (29)
                            call sum_up_matrix_unroll_04_08_r8_ASM(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (30)
                            call sum_up_matrix_unroll_04_02_r8_ASM(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (31)
                            call sum_up_matrix_r8(res_r8, x_r8, n_i8, c_i8)
                    case (32)
                            call sum_up_matrix_parallel_02_r8(res_r8, x_r8, n_i8, c_i8)
                    case (33)
                            call sum_up_matrix_parallel_04_r8(res_r8, x_r8, n_i8, c_i8)
                    case (34)
                            call sum_up_matrix_parallel_08_r8(res_r8, x_r8, n_i8, c_i8)
                    case (35)
                            call sum_of_matrix(res_r8, x_r8, n_i8, c_i8, 1_8)
                    case (36)
                            call sum_up_matrix_unroll_02_08_r8_C_P(res_r8_ptr, x_r8_ptr, n_i8, c_i8)
                    case (37)
                            call sum_up_matrix_unroll_04_04_P_r8(res_r8, x_r8, n_i8, c_i8)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            ! print *, "R: ", types(iter_types), n_i8, real(times(iter_types)), sum(res_r8)
            ! print "(A, i15, f30.15, f30.15)", types(iter_types), n_i8, real(times(iter_types)), sum(res_r8)
            print "(A, i15, f30.15, f30.15)", types(iter_types), n_i8, real(times(iter_types)), sum(res_r8)
        end do
        write(10,*) k, n_i8, times

        print*, '============================================================='
        do iter_types=1, n_types, 1
            res_i8=0d0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case (1)
                            res_i8 =  sum(x_i8, dim=1)
                    case (2)
                            call sum_up_matrix_naive_i8(res_i8, x_i8, n_i8, c_i8)
                    case (3)
                            call sum_up_matrix_unroll_02_i8(res_i8, x_i8, n_i8, c_i8)
                    case (4)
                            call sum_up_matrix_unroll_04_i8(res_i8, x_i8, n_i8, c_i8)
                    case (5)
                            call sum_up_matrix_unroll_08_i8(res_i8, x_i8, n_i8, c_i8)
                    case (6)
                            call sum_up_matrix_unroll_15_i8(res_i8, x_i8, n_i8, c_i8)
                    case (7)
                            call sum_up_matrix_unroll_30_i8(res_i8, x_i8, n_i8, c_i8)
                    case (8)
                            call sum_up_matrix_unroll_04_04_i8(res_i8, x_i8, n_i8, c_i8)
                    case (9)
                            call sum_up_matrix_unroll_08_02_i8(res_i8, x_i8, n_i8, c_i8)
                    case (10)
                            call sum_up_matrix_unroll_02_08_i8(res_i8, x_i8, n_i8, c_i8)
                    case (11)
                            call sum_up_matrix_unroll_04_02_i8(res_i8, x_i8, n_i8, c_i8)
                    case (12)
                            call sum_up_matrix_unroll_02_04_i8(res_i8, x_i8, n_i8, c_i8)
                    case (13)
                            call sum_up_matrix_naive_i8_C(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (14)
                            call sum_up_matrix_unroll_02_i8_C(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (15)
                            call sum_up_matrix_unroll_04_i8_C(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (16)
                            call sum_up_matrix_unroll_08_i8_C(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (17)
                            call sum_up_matrix_unroll_15_i8_C(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (18)
                            call sum_up_matrix_unroll_04_i8_ASM(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (19)
                            call sum_up_matrix_unroll_08_i8_ASM(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (20)
                            call sum_up_matrix_unroll_16_i8_ASM(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (21)
                            call sum_up_matrix_unroll_32_i8_ASM(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (22)
                            call sum_up_matrix_unroll_04_04_i8_C(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (23)
                            call sum_up_matrix_unroll_08_02_i8_C(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (24)
                            call sum_up_matrix_unroll_02_08_i8_C(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (25)
                            call sum_up_matrix_unroll_04_02_i8_C(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (26)
                            call sum_up_matrix_unroll_02_04_i8_C(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (27)
                            call sum_up_matrix_unroll_04_04_i8_ASM(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (28)
                            call sum_up_matrix_unroll_08_02_i8_ASM(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (29)
                            call sum_up_matrix_unroll_04_08_i8_ASM(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (30)
                            call sum_up_matrix_unroll_04_02_i8_ASM(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (31)
                            call sum_up_matrix_i8(res_i8, x_i8, n_i8, c_i8)
                    case (32)
                            call sum_up_matrix_parallel_02_i8(res_i8, x_i8, n_i8, c_i8)
                    case (33)
                            call sum_up_matrix_parallel_04_i8(res_i8, x_i8, n_i8, c_i8)
                    case (34)
                            call sum_up_matrix_parallel_08_i8(res_i8, x_i8, n_i8, c_i8)
                    case (35)
                            call sum_of_matrix(res_i8, x_i8, n_i8, c_i8, 1_8)
                    case (36)
                            call sum_up_matrix_unroll_02_08_i8_C_P(res_i8_ptr, x_i8_ptr, n_i8, c_i8)
                    case (37)
                            call sum_up_matrix_unroll_04_04_P_i8(res_i8, x_i8, n_i8, c_i8)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, i15)", types(iter_types), n_i8, real(times(iter_types)), sum(res_i8)
        end do
        write(20,*) k, n_i8, times


        deallocate(x_r8, x_i8)
        call sleep(5)
    end do


contains

    subroutine sum_up_matrix_r8(r, x, n, c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i
        real(kind=8) :: tmp_sum

        do i=1, c, 1
            tmp_sum = sum_up(x(:,i),n)
            r(i) = tmp_sum
        end do
    end subroutine sum_up_matrix_r8

    subroutine sum_up_matrix_i8(r, x, n, c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i
        integer(kind=8) :: tmp_sum

        do i=1, c, 1
            tmp_sum = sum_up(x(:,i),n)
            r(i) = tmp_sum
        end do
    end subroutine sum_up_matrix_i8

    subroutine sum_up_matrix_parallel_02_r8(r, x, n, c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i
        real(kind=8) :: tmp_sum

        !$omp parallel num_threads(2)
        !$omp do
        do i=1, c, 1
            tmp_sum = sum_up(x(:,i),n)
            r(i) = tmp_sum
        end do
        !$omp end do
        !$omp end parallel
    end subroutine sum_up_matrix_parallel_02_r8

    subroutine sum_up_matrix_parallel_02_i8(r, x, n, c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i
        integer(kind=8) :: tmp_sum

        !$omp parallel num_threads(2)
        !$omp do
        do i=1, c, 1
            tmp_sum = sum_up(x(:,i),n)
            r(i) = tmp_sum
        end do
        !$omp end do
        !$omp end parallel
    end subroutine sum_up_matrix_parallel_02_i8

    subroutine sum_up_matrix_parallel_04_r8(r, x, n, c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i
        real(kind=8) :: tmp_sum

        !$omp parallel num_threads(4)
        !$omp do
        do i=1, c, 1
            tmp_sum = sum_up(x(:,i),n)
            r(i) = tmp_sum
        end do
        !$omp end do
        !$omp end parallel
    end subroutine sum_up_matrix_parallel_04_r8

    subroutine sum_up_matrix_parallel_04_i8(r, x, n, c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i
        integer(kind=8) :: tmp_sum

        !$omp parallel num_threads(4)
        !$omp do
        do i=1, c, 1
            tmp_sum = sum_up(x(:,i),n)
            r(i) = tmp_sum
        end do
        !$omp end do
        !$omp end parallel
    end subroutine sum_up_matrix_parallel_04_i8

    subroutine sum_up_matrix_parallel_08_r8(r, x, n, c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i
        real(kind=8) :: tmp_sum

        !$omp parallel num_threads(8)
        !$omp do
        do i=1, c, 1
            tmp_sum = sum_up(x(:,i),n)
            r(i) = tmp_sum
        end do
        !$omp end do
        !$omp end parallel
    end subroutine sum_up_matrix_parallel_08_r8

    subroutine sum_up_matrix_parallel_08_i8(r, x, n, c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i
        integer(kind=8) :: tmp_sum

        !$omp parallel num_threads(8)
        !$omp do
        do i=1, c, 1
            tmp_sum = sum_up(x(:,i),n)
            r(i) = tmp_sum
        end do
        !$omp end do
        !$omp end parallel
    end subroutine sum_up_matrix_parallel_08_i8


    subroutine sum_up_matrix_naive_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i, j
        real(kind=8)    :: tmp
        include "./inc_sum_matrix_naive_f.f90"
    end subroutine sum_up_matrix_naive_r8

    subroutine sum_up_matrix_naive_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: i, j
        integer(kind=8) :: tmp
        include "./inc_sum_matrix_naive_f.f90"
    end subroutine sum_up_matrix_naive_i8


    subroutine sum_up_matrix_unroll_02_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: num_unroll, i, j
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        num_unroll = n - mod(n, 2)
        include "./inc_sum_matrix_unroll_02_f.f90"
    end subroutine sum_up_matrix_unroll_02_r8

    subroutine sum_up_matrix_unroll_02_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: num_unroll, i, j
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        num_unroll = n - mod(n, 2)
        include "./inc_sum_matrix_unroll_02_f.f90"
    end subroutine sum_up_matrix_unroll_02_i8


    subroutine sum_up_matrix_unroll_04_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: num_unroll, i, j
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        num_unroll = n - mod(n, 4)
        include "./inc_sum_matrix_unroll_04_f.f90"
    end subroutine sum_up_matrix_unroll_04_r8

    subroutine sum_up_matrix_unroll_04_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: num_unroll, i, j
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        num_unroll = n - mod(n, 4)
        include "./inc_sum_matrix_unroll_04_f.f90"
    end subroutine sum_up_matrix_unroll_04_i8


    subroutine sum_up_matrix_unroll_08_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: num_unroll, i, j
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        num_unroll = n - mod(n, 8)
        include "./inc_sum_matrix_unroll_08_f.f90"
    end subroutine sum_up_matrix_unroll_08_r8

    subroutine sum_up_matrix_unroll_08_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: num_unroll, i, j
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        num_unroll = n - mod(n, 8)
        include "./inc_sum_matrix_unroll_08_f.f90"
    end subroutine sum_up_matrix_unroll_08_i8


    subroutine sum_up_matrix_unroll_15_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: num_unroll, i, j
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        num_unroll = n - mod(n, 15)
        include "./inc_sum_matrix_unroll_15_f.f90"
    end subroutine sum_up_matrix_unroll_15_r8

    subroutine sum_up_matrix_unroll_15_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: num_unroll, i, j
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        num_unroll = n - mod(n, 15)
        include "./inc_sum_matrix_unroll_15_f.f90"
    end subroutine sum_up_matrix_unroll_15_i8



    subroutine sum_up_matrix_unroll_30_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: num_unroll, i, j
        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15

        num_unroll = n - mod(n, 30)
        include "./inc_sum_matrix_unroll_30_f.f90"
    end subroutine sum_up_matrix_unroll_30_r8

    subroutine sum_up_matrix_unroll_30_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: num_unroll, i, j
        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15

        num_unroll = n - mod(n, 30)
        include "./inc_sum_matrix_unroll_30_f.f90"
    end subroutine sum_up_matrix_unroll_30_i8


    subroutine sum_up_matrix_unroll_04_04_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_04_04_f.f90"
    end subroutine sum_up_matrix_unroll_04_04_r8

    subroutine sum_up_matrix_unroll_04_04_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_04_04_f.f90"
    end subroutine sum_up_matrix_unroll_04_04_i8


    subroutine sum_up_matrix_unroll_08_02_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_08_02_f.f90"
    end subroutine sum_up_matrix_unroll_08_02_r8

    subroutine sum_up_matrix_unroll_08_02_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_08_02_f.f90"
    end subroutine sum_up_matrix_unroll_08_02_i8


    subroutine sum_up_matrix_unroll_02_08_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_02_08_f.f90"
    end subroutine sum_up_matrix_unroll_02_08_r8

    subroutine sum_up_matrix_unroll_02_08_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_02_08_f.f90"
    end subroutine sum_up_matrix_unroll_02_08_i8


    subroutine sum_up_matrix_unroll_04_02_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_04_02_f.f90"
    end subroutine sum_up_matrix_unroll_04_02_r8

    subroutine sum_up_matrix_unroll_04_02_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_04_02_f.f90"
    end subroutine sum_up_matrix_unroll_04_02_i8


    subroutine sum_up_matrix_unroll_02_04_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_02_04_f.f90"
    end subroutine sum_up_matrix_unroll_02_04_r8

    subroutine sum_up_matrix_unroll_02_04_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_02_04_f.f90"
    end subroutine sum_up_matrix_unroll_02_04_i8


    subroutine sum_up_matrix_unroll_04_04_P_r8(r,x,n,c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        real(kind=8) :: r00, r01, r02, r03
        real(kind=8) :: r04, r05, r06, r07
        real(kind=8) :: r08, r09, r10, r11
        real(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_04_04_f_P.f90"
    end subroutine sum_up_matrix_unroll_04_04_P_r8

    subroutine sum_up_matrix_unroll_04_04_P_i8(r,x,n,c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        integer(kind=8) :: i_unroll, j_unroll, i, j

        integer(kind=8) :: r00, r01, r02, r03
        integer(kind=8) :: r04, r05, r06, r07
        integer(kind=8) :: r08, r09, r10, r11
        integer(kind=8) :: r12, r13, r14, r15
        include "./inc_sum_matrix_unroll_04_04_f_P.f90"
    end subroutine sum_up_matrix_unroll_04_04_P_i8

end program main_sum_up_vector
