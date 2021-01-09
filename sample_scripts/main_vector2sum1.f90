program main_vector2sum1
    use mod_linalg, only: vector2sum1
    implicit none
    
    integer(kind=8)           :: n_samples
    real(kind=8), allocatable :: vector(:)

    n_samples = 5_8
    allocate(vector(n_samples))
    call random_number(vector)


    print*, '============================================================='
    print*, "vector: ", vector
    print*, "   sum: ", sum(vector)

    print*, '============================================================='
    print*, "sum(vector) is 1."
    call vector2sum1(vector, n_samples)
    print*, "vector: ", vector
    print*, "   sum: ", sum(vector)



end program main_vector2sum1
