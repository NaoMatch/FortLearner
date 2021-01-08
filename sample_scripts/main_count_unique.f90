! gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_count_unique.f90 -o main_count_unique.out
program main_count_unique
    use mod_common, only: count_unique
    implicit none

    integer(kind=4) :: n_samples_i4
    real(kind=4), allocatable :: vector_r4(:)
    
    integer(kind=8) :: n_samples_i8
    real(kind=8), allocatable :: vector_r8(:)

    n_samples_i4 = 5
    n_samples_i8 = 5_8
    allocate(vector_r4(n_samples_i4))
    allocate(vector_r8(n_samples_i8))

    print*, "============================================================"
    print*, "real(kind=4), random"
    call random_number(vector_r4)
    print*, vector_r4
    print*, count_unique(vector_r4, n_samples_i4)
    
    print*, "============================================================"
    print*, "real(kind=8), random"
    call random_number(vector_r8)
    print*, vector_r8
    print*, count_unique(vector_r8, n_samples_i8)
    
    print*, "============================================================"
    print*, "real(kind=4), same value"
    vector_r4 = 1
    print*, vector_r4
    print*, count_unique(vector_r4, n_samples_i4)
    
    print*, "============================================================"
    print*, "real(kind=8), same value"
    vector_r8 = 1
    print*, vector_r8
    print*, count_unique(vector_r8, n_samples_i8)
    

end program main_count_unique
