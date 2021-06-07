program main_minmax
    use mod_const
    use mod_common
    use mod_timer
    use mod_stats
    implicit none

    integer(kind=8) :: iter, max_iter
    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(kind=8) :: n_samples, n_columns
    real(kind=8), allocatable :: matrix(:,:), vector_r8(:)
    integer(kind=8), allocatable :: vector_i8(:)
    real(kind=8) :: sumval_naive, sumval_branch
    real(kind=8) :: threshold_r8
    real(kind=8) :: min_r8, max_r8
    integer(kind=8) :: min_i8, max_i8
    integer(kind=8) :: threshold_i8

    max_iter = 100
    n_samples = 20000000
    allocate(vector_r8(n_samples))
    allocate(vector_i8(n_samples))
    call RANDOM_NUMBER(vector_r8)
    vector_r8 = int(vector_r8 * 10d0, kind=8)
    vector_i8 = int(vector_r8, kind=8)

    print*, '============================================================='
    print*, "NAIVE"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        min_r8 = minval(vector_r8)
        max_r8 = maxval(vector_r8)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), min_r8, max_r8

    print*, '============================================================='
    print*, "Optimized Fortran OLD"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call get_minmax(min_r8, max_r8, vector_r8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch, min_r8, max_r8

    print*, '============================================================='
    print*, "Optimized Fortran NEW"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call minmax_loop_f_r8(min_r8, max_r8, vector_r8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch, min_r8, max_r8

    print*, '============================================================='
    print*, "with Assembler"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call minmax(min_r8, max_r8, vector_r8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch, min_r8, max_r8


    print*, '============================================================='
    print*, "NAIVE"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        min_i8 = minval(vector_i8)
        max_i8 = maxval(vector_i8)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), min_i8, max_i8

    print*, '============================================================='
    print*, "Optimized Fortran OLD"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call get_minmax(min_i8, max_i8, vector_i8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), min_i8, max_i8

    print*, '============================================================='
    print*, "Optimized Fortran NEW"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call minmax_loop_f_i8(min_i8, max_i8, vector_i8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), min_i8, max_i8

    print*, '============================================================='
    print*, "with Assembler"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call minmax(min_i8, max_i8, vector_i8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), min_i8, max_i8


contains

    subroutine minmax_loop_f_r8(min, max, x, n)
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

        num_unroll = n - mod(n, 8)
        do i=1, num_unroll, 8
            r00 = x(i)
            r01 = x(i+1)
            r02 = x(i+2)
            r03 = x(i+3)
            r04 = x(i+4)
            r05 = x(i+5)
            r06 = x(i+6)
            r07 = x(i+7)

            r14 = minval((/r14, r00/))
            r14 = minval((/r14, r01/))
            r14 = minval((/r14, r02/))
            r14 = minval((/r14, r03/))
            r14 = minval((/r14, r04/))
            r14 = minval((/r14, r05/))
            r14 = minval((/r14, r06/))
            r14 = minval((/r14, r07/))

            r15 = maxval((/r15, r00/))
            r15 = maxval((/r15, r01/))
            r15 = maxval((/r15, r02/))
            r15 = maxval((/r15, r03/))
            r15 = maxval((/r15, r04/))
            r15 = maxval((/r15, r05/))
            r15 = maxval((/r15, r06/))
            r15 = maxval((/r15, r07/))
        end do

        do i=num_unroll+1, n, 1
            tmp_x = x(i)
            r14 = minval((/tmp_x, r14/))
            r15 = maxval((/tmp_x, r15/))
        end do

        min = r14
        max = r15

    end subroutine minmax_loop_f_r8

    subroutine minmax_loop_f_i8(min, max, x, n)
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

        num_unroll = n - mod(n, 8)
        do i=1, num_unroll, 8
            r00 = x(i)
            r01 = x(i+1)
            r02 = x(i+2)
            r03 = x(i+3)
            r04 = x(i+4)
            r05 = x(i+5)
            r06 = x(i+6)
            r07 = x(i+7)

            r14 = minval((/r14, r00/))
            r14 = minval((/r14, r01/))
            r14 = minval((/r14, r02/))
            r14 = minval((/r14, r03/))
            r14 = minval((/r14, r04/))
            r14 = minval((/r14, r05/))
            r14 = minval((/r14, r06/))
            r14 = minval((/r14, r07/))

            r15 = maxval((/r15, r00/))
            r15 = maxval((/r15, r01/))
            r15 = maxval((/r15, r02/))
            r15 = maxval((/r15, r03/))
            r15 = maxval((/r15, r04/))
            r15 = maxval((/r15, r05/))
            r15 = maxval((/r15, r06/))
            r15 = maxval((/r15, r07/))
        end do

        do i=num_unroll+1, n, 1
            tmp_x = x(i)
            r14 = minval((/tmp_x, r14/))
            r15 = maxval((/tmp_x, r15/))
        end do

        min = r14
        max = r15

    end subroutine minmax_loop_f_i8

end program main_minmax
