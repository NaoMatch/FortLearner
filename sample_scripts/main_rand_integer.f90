! gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_random.f90 main_rand_integer.f90 -o main_rand_integer.out
program main_rand_integer
    use mod_random, only: rand_integer
    implicit none
    integer(kind=8) :: n_samples, lo, hi
    integer(kind=8), allocatable :: vector(:)
    integer(kind=8) :: i

    n_samples = 5
    allocate(vector(n_samples))

    lo = 1
    hi = 10
    print*, '============================================================='
    print*, "Random Uniform Integer"
    print*, "    lo: ", lo
    print*, "    hi: ", hi
    call rand_integer(lo, hi, vector, n_samples)
    print*, vector
    
    lo = 1
    hi = 100000
    print*, '============================================================='
    print*, "Random Uniform Integer"
    print*, "    lo: ", lo
    print*, "    hi: ", hi
    call rand_integer(lo, hi, vector, n_samples)
    print*, vector

end program main_rand_integer
