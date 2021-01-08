! gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_collect_unique_values.f90 -o main_collect_unique_values.out
program main_collect_unique_values
    use mod_common, only: collect_unique_values
    implicit none

    integer(kind=8) :: n_samples
    real(kind=8), allocatable :: uniq_values(:), vector(:)    
    
    n_samples = 5_8
    allocate(vector(n_samples))

    print*, '============================================================='
    print*, "all same"
    vector = 0
    print*, vector
    call collect_unique_values(uniq_values, vector, n_samples)
    print*, uniq_values

    print*, '============================================================='
    print*, "sorted"
    vector = (/1,2,2,4,4/)
    print*, vector
    call collect_unique_values(uniq_values, vector, n_samples)
    print*, uniq_values

    print*, '============================================================='
    print*, "repeat (doesn't work)"
    vector = (/1,2,1,2,1/)
    print*, vector
    call collect_unique_values(uniq_values, vector, n_samples)
    print*, uniq_values


end program main_collect_unique_values
