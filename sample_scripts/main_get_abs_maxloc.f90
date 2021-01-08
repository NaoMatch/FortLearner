! gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_get_abs_maxloc.f90 -o main_get_abs_maxloc.out
program main_get_abs_maxloc
    use mod_common, only: get_abs_maxloc
    implicit none

    integer(kind=4)           :: n_dim
    integer(kind=4)           :: absmax_loc(2)
    real(kind=4)              :: absmax_val
    real(kind=4), allocatable :: squared_matrix(:,:)

    integer(kind=4) :: i

    n_dim = 5
    allocate(squared_matrix(n_dim, n_dim))
    call random_number(squared_matrix)
    squared_matrix = matmul(squared_matrix, transpose(squared_matrix))

    print*, "Symmetric Matrix Only"

    print*, "========================================================="
    do i=1, n_dim, 1
        print*, squared_matrix(i,:)
    end do
    print*, "========================================================="

    call get_abs_maxloc(absmax_loc, absmax_val, squared_matrix, n_dim)
    print*, "Absolute Maximum Value:    ", absmax_val
    print*, "Absolute Maximum Location: ", absmax_loc


    

end program main_get_abs_maxloc
