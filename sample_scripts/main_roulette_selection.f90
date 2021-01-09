! gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_random.f90 main_roulette_selection.f90 -o main_roulette_selection.out
program main_roulette_selection
    use mod_const,  only: t_, f_
    use mod_random, only: roulette_selection
    implicit none

    integer(kind=8) :: n_samples, selected_index
    real(kind=8), allocatable :: vector(:)
    logical(kind=4) :: reverse

    n_samples = 5_8
    allocate(vector(n_samples))
    call random_number(vector)

    print*, '============================================================='
    print*, "Normalize"
    vector = vector / sum(vector)

    print*, '============================================================='
    print*, "Vector:  ", vector
    print*, "    sum: ", sum(vector)
    reverse = f_
    print*, "    from left:", reverse
    selected_index = roulette_selection(vector, n_samples, reverse=reverse)
    print*, "    index: ", selected_index

end program main_roulette_selection
