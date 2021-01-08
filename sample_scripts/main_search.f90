! gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_search.f90 -o main_search.out
program main_search
    use mod_const
    use mod_common, only: linear_search, binary_search_left, binary_search_right
    implicit none
    
    integer(kind=8) :: n_samples
    real(kind=8)    :: val
    real(kind=8), allocatable :: vector(:)

    n_samples = 5
    allocate(vector(n_samples))
    vector = ((/1,2,3,4,5/))

    val = 3.5
    print*, '============================================================='
    print*, "Find: vector(i-1) < val <= vector(i) by linear search"
    print*, "  vector: ", vector
    print*, "  val:    ", val
    print*, linear_search(vector, n_samples, val)


    val = 3.5
    print*, '============================================================='
    print*, "Find: vector(i-1) < val <= vector(i) by binary search"
    print*, "  vector: ", vector
    print*, "  val:    ", val
    print*, binary_search_left(vector, n_samples, val)


    val = 3.5
    print*, '============================================================='
    print*, "Find: vector(i-1) < val <= vector(i) by binary search"
    print*, "  vector: ", vector
    print*, "  val:    ", val
    print*, binary_search_right(vector, n_samples, val)

end program main_search
