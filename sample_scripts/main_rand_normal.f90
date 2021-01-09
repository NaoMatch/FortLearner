! gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_random.f90 main_rand_normal.f90 -o main_rand_normal.out
program main_rand_normal
    use mod_random, only: rand_normal
    implicit none
    integer(kind=8) :: n_samples
    real(kind=8), allocatable :: vector(:)
    real(kind=8) :: mean, var
    integer(kind=8) :: i

    n_samples = 5
    allocate(vector(n_samples))

    print*, '============================================================='
    print*, "call random_number(vector)"
    call random_number(vector)
    print*, vector

    print*, '============================================================='
    print*, "call rand_normal(vector, n_samples)"
    call rand_normal(vector, n_samples)
    print*, vector

    print*, '============================================================='
    print*, "CHECK Mean and Variance (Mean and Variance subroutines are implemented in 'math::mod_stats')"
    mean = 0d0
    var = 0d0
    n_samples = 100000
    deallocate(vector)
    allocate(vector(n_samples))
    call rand_normal(vector, n_samples)
    
    do i=1, n_samples, 1
        mean = mean + vector(i)
    end do
    mean = mean / dble(n_samples)
    print*, "Mean: ", mean

    do i=1, n_samples, 1
        var = var + (vector(i)-mean)**2d0
    end do
    var = var / dble(n_samples-1)
    print*, "Variance: ", var





end program main_rand_normal
