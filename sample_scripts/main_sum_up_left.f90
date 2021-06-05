program main_sum_up_left
    use mod_const
    use mod_common
    use mod_timer
    use mod_stats
    implicit none

    integer(kind=8) :: iter, max_iter
    integer(kind=8) :: date_value1(8), date_value2(8)
    integer(kind=8) :: n_samples, n_columns
    real(kind=8), allocatable :: matrix(:,:), x(:), y(:)
    integer(kind=8), allocatable :: vector_i8(:)
    real(kind=8) :: sumval_naive, sumval_branch
    real(kind=8) :: threshold_r8
    integer(kind=8) :: threshold_i8

    max_iter = 100
    n_samples = 10000000
    threshold_r8 = 5d0
    threshold_i8 = 5_8
    allocate(x(n_samples))
    allocate(y(n_samples))
    allocate(vector_i8(n_samples))
    call RANDOM_NUMBER(x)
    call RANDOM_NUMBER(y)
    x = int(x * 10d0, kind=8)
    vector_i8 = int(x, kind=8)

    print*, '============================================================='
    print*, "NAIVE"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_naive = sum_up_left_naive(x, threshold_r8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_naive


    print*, '============================================================='
    print*, "BRANCH"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_branch = sum_up_left(x, threshold_r8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch


    print*, '============================================================='
    print*, "BRANCH_I8"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_branch = sum_up_left(vector_i8, threshold_i8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch

    print*, '============================================================='
    print*, "BRANCH_I8_R8"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_branch = sum_up_left(vector_i8, threshold_r8, n_samples)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch



    print*, '============================================================='
    print*, "NAIVE"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_naive = sum_up_left_naive2(x, y, n_samples, threshold_r8)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_naive


    print*, '============================================================='
    print*, "BRANCH"
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        sumval_branch = sum_up_left(x, y, n_samples, threshold_r8)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sumval_branch









contains

    function sum_up_left_naive(vector, threshold, n_samples)
        implicit none
        real(kind=8)                :: sum_up_left_naive
        real(kind=8), intent(in)    :: vector(n_samples)
        real(kind=8), intent(in)    :: threshold
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8) :: n
        real(kind=8) :: val

        sum_up_left_naive = 0d0
        do n=1, n_samples, 1
            val = vector(n)
            if ( val .le. threshold ) then
                sum_up_left_naive = sum_up_left_naive + val
            end if
        end do
    end function sum_up_left_naive

    function sum_up_left_naive2(x, y, n_samples, threshold)
        implicit none
        real(kind=8)                :: sum_up_left_naive2
        real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
        real(kind=8), intent(in)    :: threshold
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8) :: n
        real(kind=8) :: val

        sum_up_left_naive2 = 0d0
        do n=1, n_samples, 1
            val = x(n)
            if ( val .le. threshold ) then
                sum_up_left_naive2 = sum_up_left_naive2 + y(n)
            end if
        end do
    end function sum_up_left_naive2

    ! function sum_up_left_branch(vector, threshold, n_samples)
    !     implicit none
    !     real(kind=8)                :: sum_up_left_branch
    !     real(kind=8), intent(in)    :: vector(n_samples)
    !     real(kind=8), intent(in)    :: threshold
    !     integer(kind=8), intent(in) :: n_samples

    !     integer(kind=8) :: n, factor
    !     real(kind=8) :: val, zero=0

    !     sum_up_left_branch = zero
    !     do n=1, n_samples, 1
    !         val = vector(n)
    !         factor = val .le. threshold
    !         sum_up_left_branch = sum_up_left_branch + val * factor
    !     end do
    ! end function sum_up_left_branch

    ! function sum_up_left_branch_cache(vector, threshold, n_samples)
    !     implicit none
    !     real(kind=8)                :: sum_up_left_branch_cache
    !     real(kind=8), intent(in)    :: vector(n_samples)
    !     real(kind=8), intent(in)    :: threshold
    !     integer(kind=8), intent(in) :: n_samples

    !     integer(kind=8) :: n, factor, i
    !     real(kind=8) :: val
    !     real(kind=8) :: buffer(15)
    !     integer(kind=8) :: buffer_len=15, n_samples_unroll

    !     sum_up_left_branch_cache = 0d0
    !     n_samples_unroll = n_samples - mod(n_samples, buffer_len)
    !     do n=1, n_samples_unroll, buffer_len
    !         do i=0, buffer_len-1, 1
    !             val = vector(n+i)
    !             factor = val .le. threshold
    !             sum_up_left_branch_cache = sum_up_left_branch_cache + val * factor
    !         end do
    !     end do

    !     do n=n_samples_unroll+1, n_samples, 1
    !         val = vector(n)
    !         factor = val .le. threshold
    !         sum_up_left_branch_cache = sum_up_left_branch_cache + val * factor
    !     end do
    ! end function sum_up_left_branch_cache


end program main_sum_up_left
