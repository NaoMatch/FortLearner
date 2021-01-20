program main_math
    use mod_const
    use mod_math, only: log2, gini, entropy, gamma
    implicit none

    integer(kind=8) :: n_classes, max_val
    real(kind=8), allocatable :: vector_r8(:)
    integer(kind=8), allocatable :: vector_i8(:)
    real(kind=4) :: x_r4, res_r4
    real(kind=8) :: x_r8, res_r8

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

    print*, '============================================================='
    x_r4 = 0.10
    x_r8 = 0.1d0
    res_r4 = gamma(x_r4)
    res_r8 = gamma(x_r8)
    print*, "Gamma(0.49)", x_r4, res_r4
    print*, "Gamma(0.49)", x_r8, res_r8
    

end program main_math
