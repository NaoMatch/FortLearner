program main
    use mod_common
    implicit none

    real(kind=8) :: val, min_val, max_val
    real(kind=8), allocatable :: x(:), x_copy(:)

    min_val = 0d0
    max_val = 10d0 
    val = 11d0
    print*, val, min_val, max_val , " -> ", clipping(val, min_val, max_val)
    val = 10d0
    print*, val, min_val, max_val , " -> ", clipping(val, min_val, max_val)
    val = 9d0
    print*, val, min_val, max_val , " -> ", clipping(val, min_val, max_val)
    val = 1d0
    print*, val, min_val, max_val , " -> ", clipping(val, min_val, max_val)
    val = -1d0
    print*, val, min_val, max_val , " -> ", clipping(val, min_val, max_val)
    val = -10d0
    print*, val, min_val, max_val , " -> ", clipping(val, min_val, max_val)


    allocate(x(10), x_copy(10))
    call random_number(x)
    x = 2d0 * x - 1d0
    min_val = .2d0
    max_val = .5d0

    print*, '*********************************************************************************************'
    print*, "Clip both side"
    x_copy = x
    call clipping_array(x_copy, 10_8, min_val, max_val)
    print*, real(x)
    print*, real(x_copy)

    print*, '*********************************************************************************************'
    print*, "Clip from below"
    x_copy = x
    call clipping_array_lower(x_copy, 10_8, min_val)
    print*, real(x)
    print*, real(x_copy)

    print*, '*********************************************************************************************'
    print*, "Clip from upper"
    x_copy = x
    call clipping_array_upper(x_copy, 10_8, max_val)
    print*, real(x)
    print*, real(x_copy)



contains
    
    



end program main