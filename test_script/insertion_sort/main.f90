program main
    use Iso_C_Binding
    use mod_sort
    use mod_timer
    implicit none

    interface 
        subroutine inseriont_sort_r8(x, n) Bind(C,Name='inseriont_sort_r8')
            Import
            integer(c_int64_t), Value     :: n
            real(c_double), intent(inout) :: x(n)
        end subroutine inseriont_sort_r8
    end interface

    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(c_int64_t), allocatable :: indices(:), indices_full(:)
    real(c_double), allocatable :: mask(:), tmp(:), tmp_x(:)
    real(c_double), allocatable :: x(:), x_orig(:)
    real(c_double), allocatable :: y(:)
    real(c_double) :: res
    real(c_double) :: res_min1, res_min2, res_min3, res_min4, res_min5, res_min6, res_min7, res_min8
    real(c_double) :: res_max1, res_max2, res_max3, res_max4, res_max5, res_max6, res_max7, res_max8
    real(c_double) :: x_edge2(2)
    real(kind=8) :: time1, time2, time3, time4, time5, time6, time7, time8, NFLOP
    real(kind=8), ALLOCATABLE :: times(:)
    integer(kind=8) :: iter, n_iter, n, k, n_x, n_i, idx, i, n_types, iter_types
    character(len=30), ALLOCATABLE :: types(:)

    open(10, file="time_insertion_sort.csv")

    n_types = 2
    allocate(times(n_types))
    allocate(types(n_types))
    types(1) = "insertion_sort_F       :"
    types(2) = "insertion_sort_C       :"

    do k=16, 32, 1
        n = k
        allocate(x(n), x_orig(n))
        call random_number(x_orig)
        n_iter=3000000000_8/n/n

        res = 0d0
        print*, '============================================================='
        do iter_types=1, n_types, 1
            call date_and_time(values=date_value1)
            x = x_orig
            do iter=1, n_iter, 1
                select case(iter_types)
                    case (1); call insertion_sort(x,n)
                    case (2); call inseriont_sort_r8(x,n)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, f30.15, f30.15)", types(iter_types), n, real(times(iter_types))
        end do
        deallocate(x, x_orig)

        write(10,*) k, n, times
    end do






end program main
