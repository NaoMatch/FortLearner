! gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_is_sorted.f90 -o main_is_sorted.out
program main_is_sorted
    use mod_const,  only: t_, f_
    use mod_common, only: is_sorted
    implicit none
    
    integer(kind=4) :: n_samples_i4
    integer(kind=8) :: n_samples_i8

    real(kind=4)    :: vector_r4(3)
    real(kind=8)    :: vector_r8(3)
    integer(kind=4) :: vector_i4(3)
    integer(kind=8) :: vector_i8(3)

    n_samples_i4 = 3
    n_samples_i8 = 3_8

    print*, "==============================================================="
    print*, "real(kind=4), random"
    call random_number(vector_r4)
    print*, vector_r4
    print*, "Sorted? ", is_sorted(vector_r4, n_samples_i4)

    print*, "==============================================================="
    print*, "real(kind=4), sorted, ascending order"
    vector_r4 = (/1,2,3/)
    print*, vector_r4
    print*, "Sorted? ", is_sorted(vector_r4, n_samples_i4)

    print*, "==============================================================="
    print*, "real(kind=4), sorted, descending order"
    vector_r4 = (/3,2,1/)
    print*, vector_r4
    print*, "Sorted? ", is_sorted(vector_r4, n_samples_i4)

    print*, "==============================================================="
    print*, "real(kind=4), sorted, descending order, use optional argument"
    vector_r4 = (/3,2,1/)
    print*, vector_r4
    print*, "Sorted? ", is_sorted(vector_r4, n_samples_i4, ascending=f_)

end program main_is_sorted
