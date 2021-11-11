program main
    use iso_c_binding
    use mod_dgemm
    use mod_stats
    implicit none

    real(kind=8), allocatable    :: times(:), sums(:)
    integer(kind=8)              :: date_value1(8), date_value2(8)
    integer(kind=8)              :: n, mat_size, max_iter, n_iter, iter, t, n_cases
    integer(kind=8), allocatable :: mat_sizes(:)
    real(kind=8), allocatable, target    :: a(:,:), b(:,:), c(:,:)

    type(c_ptr) :: a_ptr, b_ptr, c0_ptr

    ! mat_sizes = (/16_8, 32_8, 64_8, 128_8, 256_8, 512_8, 1024_8, 2048_8, 4096_8/)
    mat_sizes = (/16_8, 32_8, 64_8, 128_8, 256_8, 512_8, 1024_8, 2048_8/)
    ! mat_sizes = 16
    max_iter = 50000000_8
    ! max_iter = 1_8


    do n=1, size(mat_sizes), 1
        allocate(times(0))
        allocate(sums(0))

        mat_size=mat_sizes(n)
        allocate(a(mat_size, mat_size))
        allocate(b(mat_size, mat_size))
        allocate(c(mat_size, mat_size))
        call random_number(a)
        call random_number(b)

        a_ptr = c_loc(a)
        b_ptr = c_loc(b)
        c0_ptr = c_loc(c)

        n_iter = maxval((/max_iter/mat_size/mat_size/mat_size, 1_8/))

        n_cases = 17
        do t=1, n_cases, 1
            call date_and_time(values=date_value1)
            c(:,:) = 0d0
            do iter=1, n_iter, 1
                    select case (t)

                    ! Intrinsic
                    case  (1); c = matmul(a,b)
                    case  (2); call dgemm('N','N',mat_size,mat_size,mat_size,1.0d0,A,mat_size,B,mat_size,0.d0,C,mat_size)

                    ! Fortran
                    ! case  (3); call my_dgemm_naive00_00(a,b,c,mat_size,mat_size,mat_size)
                    ! case  (4); call my_dgemm_naive00_01(a,b,c,mat_size,mat_size,mat_size)
                    ! case  (5); call my_dgemm_naive00_02(a,b,c,mat_size,mat_size,mat_size)
                    ! case  (6); call my_dgemm_naive00_03(a,b,c,mat_size,mat_size,mat_size)
                    ! case  (7); call my_dgemm_naive00_04(a,b,c,mat_size,mat_size,mat_size)
                    ! case  (8); call my_dgemm_naive00_05(a,b,c,mat_size,mat_size,mat_size)
                    case  (9); call my_dgemm_naive00_06(a,b,c,mat_size,mat_size,mat_size)
                    ! case (10); call my_dgemm_naive00_07(a,b,c,mat_size,mat_size,mat_size)

                    ! C
                    ! case (11); call my_dgemm_000(a_ptr,b_ptr,c0_ptr,mat_size,mat_size,mat_size) ! naive
                    ! case (12); call my_dgemm_001(a_ptr,b_ptr,c0_ptr,mat_size,mat_size,mat_size) ! cache blocking
                    case (13); call my_dgemm_002(a_ptr,b_ptr,c0_ptr,mat_size,mat_size,mat_size) ! cache copy
                    case (14); call my_dgemm_003(a_ptr,b_ptr,c0_ptr,mat_size,mat_size,mat_size) ! loop interchange
                    case (15); call my_dgemm_004(a_ptr,b_ptr,c0_ptr,mat_size,mat_size,mat_size) ! serialize
                    case (16); call my_dgemm_005(a_ptr,b_ptr,c0_ptr,mat_size,mat_size,mat_size) ! kernelize
                    case (17); call my_dgemm_006(a_ptr,b_ptr,c0_ptr,mat_size,mat_size,mat_size) ! loop interchange2
                    end select

            end do
            if (t .ge. 11_8) c=c/dble(iter-1)
            if (sum(c) .eq. 0d0) cycle
            call date_and_time(values=date_value2)
            times = [times, time_diff(date_value1, date_value2)/dble(n_iter)]
            sums  = [sums, sum(c)]
        end do

        print*, mat_size, real(times), real(sqrt(sum(sums**2d0)/n_cases - (sum(sums)/n_cases)**2d0)/sums(1))
        print*, mat_size, real(sums / sums(1))
        print*, ""

        deallocate(times,sums,a,b,c)
        ! stop
    end do
    
contains


    !> A function to get datetime values
    !! \return returns datetime values
    !! \param date_value datetime values
    function datetime2second(date_value)
        implicit none
        integer(kind=8) :: date_value(8)
        real(kind=8)    :: datetime2second
        datetime2second = ( date_value(5) )*60.         ! Hours to minutes
        datetime2second = ( datetime2second + date_value(6) )*60. ! Minutes to seconds
        datetime2second = ( datetime2second + date_value(7) )*1e3 ! Seconds to milliseconds
        datetime2second = datetime2second + date_value(8)         ! Add milliseconds
    end function datetime2second

    !> A function to compute time difference
    !! \return returns time difference
    !! \param date_value1 datetime value at time 1
    !! \param date_value1 datetime value at time 2
    function time_diff(date_value1, date_value2)
        implicit none
        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8) :: time_diff
        time_diff = abs(datetime2second(date_value1) - datetime2second(date_value2))
    end function time_diff

end program main
