! gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90 main_sort.f90 -o main_sort.out
program main_sort
    use mod_common, only: is_sorted
    use mod_timer
    use mod_sort, only: quick_sort, quick_argsort, pbucket_sort, pbucket_argsort
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)

    integer(kind=4) :: n_samples_i4
    integer(kind=8) :: n_samples_i8
    real(kind=8), allocatable :: vector_r8(:), vector_copy_r8(:)
    integer(kind=8), allocatable :: indices_i8(:)

    integer(kind=8) :: i

    n_samples_i4 = 10000000
    n_samples_i8 = 10000000_8
    allocate(vector_r8(n_samples_i8))
    allocate(vector_copy_r8(n_samples_i8))
    allocate(indices_i8(n_samples_i8))
    print*, '============================================================='
    print*, "Randomize vector_r8"
    call random_number(vector_r8)

    print*, '============================================================='
    print*, "Quick Sort, Random Uniform"
    print*, "    number of samples: ", n_samples_i8
    do i=1, n_samples_i8, 1
        vector_copy_r8(i) = vector_r8(i)
    end do
    print*, "    is sorted(before): ", is_sorted(vector_copy_r8, n_samples_i8)
    call date_and_time(values=date_value1)
    call quick_sort(vector_copy_r8, n_samples_i8)
    call date_and_time(values=date_value2)
    print*, "    is sorted(after): ", is_sorted(vector_copy_r8, n_samples_i8)
    print*, "    Time: ", time_diff(date_value1, date_value2), "[msec]"


    print*, '============================================================='
    print*, "Pseudo Bucket Sort, Random Uniform"
    print*, "    number of samples: ", n_samples_i8
    do i=1, n_samples_i8, 1
        vector_copy_r8(i) = vector_r8(i)
    end do
    print*, "    is sorted(before): ", is_sorted(vector_copy_r8, n_samples_i8)
    call date_and_time(values=date_value1)
    call pbucket_sort(vector_copy_r8, n_samples_i8)
    call date_and_time(values=date_value2)
    print*, "    is sorted(after): ", is_sorted(vector_copy_r8, n_samples_i8)
    print*, "    Time: ", time_diff(date_value1, date_value2), "[msec]"


    print*, '============================================================='
    print*, "Quick Sort and indices sorted according to vector, Random Uniform"
    print*, "    number of samples: ", n_samples_i8
    do i=1, n_samples_i8, 1
        vector_copy_r8(i) = vector_r8(i)
        indices_i8(i) = i
    end do
    print*, "    is sorted(before): ", is_sorted(vector_copy_r8, n_samples_i8)
    call date_and_time(values=date_value1)
    call quick_argsort(vector_copy_r8, indices_i8, n_samples_i8)
    call date_and_time(values=date_value2)
    print*, "    is sorted(after): ", is_sorted(vector_copy_r8, n_samples_i8)
    print*, "    Time: ", time_diff(date_value1, date_value2), "[msec]"


    print*, '============================================================='
    print*, "Pseudo Bucket Sort and indices sorted according to vector, Random Uniform"
    print*, "    number of samples: ", n_samples_i8
    do i=1, n_samples_i8, 1
        vector_copy_r8(i) = vector_r8(i)
        indices_i8(i) = i
    end do
    print*, "    is sorted(before): ", is_sorted(vector_copy_r8, n_samples_i8)
    call date_and_time(values=date_value1)
    call pbucket_argsort(vector_copy_r8, indices_i8, n_samples_i8)
    call date_and_time(values=date_value2)
    print*, "    is sorted(after): ", is_sorted(vector_copy_r8, n_samples_i8)
    print*, "    Time: ", time_diff(date_value1, date_value2), "[msec]"




end program main_sort
