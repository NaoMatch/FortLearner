program main_math
    use mod_math, only: log2, gini, entropy
    implicit none

    integer(kind=8) :: n_classes, max_val
    real(kind=8), allocatable :: vector_r8(:)
    integer(kind=8), allocatable :: vector_i8(:)

    max_val = 100
    n_classes = 10
    allocate(vector_i8(n_classes))
    allocate(vector_r8(n_classes))

    call random_number(vector_r8)
    vector_i8 = int(vector_r8*max_val, kind=kind(vector_i8))
    vector_i8(5) = 0_8

    print*, '============================================================='
    print*, vector_i8

    print*, '============================================================='
    print*, "GINI: ", gini(vector_i8, n_classes)

    print*, '============================================================='
    print*, "ENTROPY: ", entropy(vector_i8, n_classes)

    print*, '============================================================='
    print*, "From Function:   log2(x)         =", log2(10.0)
    print*, "From Definition: log(x)/log(2.0) =", log(10.0)/log(2.0)
    

end program main_math
