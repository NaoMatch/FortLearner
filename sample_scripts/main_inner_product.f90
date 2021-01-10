program main_inner_product
    use mod_linalg, only: inner_product
    implicit none

    integer(kind=8)           :: n_samples
    real(kind=8), allocatable :: vector_x(:), vector_y(:)

    n_samples = 5_8
    allocate(vector_x(n_samples))
    allocate(vector_y(n_samples))

    print*, '============================================================='
    print*, "Randomization"
    call random_number(vector_x)
    call random_number(vector_y)

    print*, '============================================================='
    print*, "vector_x: ", vector_x
    print*, "vector_y: ", vector_y
    print*, inner_product(vector_x, vector_y, n_samples)
    

end program main_inner_product
