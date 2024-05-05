program main
    use mod_common
    implicit none

    real(kind=8) :: val, min_val, max_val
    real(kind=8), allocatable :: x(:)

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


    allocate(x(10))
    call random_number(x)
    print*, x
    min_val = 0d0
    max_val = .5d0
    call clipping_array(x, 10_8, min_val, max_val)
    print*, x



contains
    
    



end program main