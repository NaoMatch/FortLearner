! gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90  ../math/mod_stats.f90 main_get_minmax.f90 -o main_get_minmax.out
program main_get_minmax
    use mod_timer
    use mod_stats, only: get_minmax
    implicit none
    
    integer(kind=8) :: date_value1(8), date_value2(8)

    integer(kind=8)           :: n_samples
    real(kind=8), allocatable :: vector(:)
    real(kind=8)              :: min_val, max_val

    n_samples = 100000000
    allocate(vector(n_samples))
    call random_number(vector)

    print*, '============================================================='
    print*, "Get Minimum and Maximum, minval(vector) and minval(vector)"
    call date_and_time(values=date_value1)
    min_val = minval(vector)
    max_val = maxval(vector)
    call date_and_time(values=date_value2)
    print*, "minmax:", min_val, max_val
    print*, "Traditional: ", time_diff(date_value1, date_value2)

    print*, '============================================================='
    print*, "Get Minimum and Maximum, get_minmax(min_val, max_val, vector, n_samples)"
    call date_and_time(values=date_value1)
    call get_minmax(min_val, max_val, vector, n_samples)
    call date_and_time(values=date_value2)
    print*, "minmax:", min_val, max_val
    print*, "My Function: ", time_diff(date_value1, date_value2)




end program main_get_minmax
