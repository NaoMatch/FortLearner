! gfortran ../common/mod_const.f90 ../common/mod_common.f90 ../common/mod_timer.f90 ../common/mod_random.f90 ../common/mod_sort.f90 main_sort.f90 -o main_sort.out
program main_sort
    use mod_common, only: is_sorted
    use mod_timer
    use mod_sort
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8), tot_time, iter, max_iter

    integer(kind=4) :: n_samples_i4
    integer(kind=8) :: n_samples_i8
    real(kind=4), allocatable :: vector_r4(:), vector_copy_r4(:)
    real(kind=8), allocatable :: vector_r8(:), vector_copy_r8(:)
    integer(kind=4), allocatable :: indices_i4(:)
    integer(kind=8), allocatable :: indices_i8(:)

    integer(kind=8) :: i

    max_iter = 1
    n_samples_i4 = 10000000
    n_samples_i8 = 10000000_8
    allocate(vector_r4(n_samples_i4), vector_copy_r4(n_samples_i4))
    allocate(vector_r8(n_samples_i8), vector_copy_r8(n_samples_i8))
    allocate(indices_i4(n_samples_i4))
    allocate(indices_i8(n_samples_i8))
    print*, '============================================================='
    print*, "Randomize vector"
    call random_number(vector_r4)
    call random_number(vector_r8)

    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Quick Sort, Random Uniform KIND=4"
    print*, "    number of samples: ", n_samples_i4
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r4)
        do i=1, n_samples_i4, 1
            vector_copy_r4(i) = vector_r4(i)
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value1)
        call quick_sort(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r4, n_samples_i4)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"

    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Block Quick Sort, Random Uniform KIND=4"
    print*, "    number of samples: ", n_samples_i4
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r4)
        do i=1, n_samples_i4, 1
            vector_copy_r4(i) = vector_r4(i)
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value1)
        call block_quick_sort(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r4, n_samples_i4)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"


    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Pseudo Bucket Sort, Random Uniform KIND=4"
    print*, "    number of samples: ", n_samples_i4
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r4)
        do i=1, n_samples_i4, 1
            vector_copy_r4(i) = vector_r4(i)
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value1)
        call pbucket_sort(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r4, n_samples_i4)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"




    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Quick ArgSort, Random Uniform KIND=4"
    print*, "    number of samples: ", n_samples_i4
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r4)
        do i=1, n_samples_i4, 1
            vector_copy_r4(i) = vector_r4(i)
            indices_i4(i) = i
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value1)
        call quick_argsort(vector_copy_r4, indices_i4, n_samples_i4)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r4, n_samples_i4)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"

    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Block Quick ArgSort, Random Uniform KIND=4"
    print*, "    number of samples: ", n_samples_i4
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r4)
        do i=1, n_samples_i4, 1
            vector_copy_r4(i) = vector_r4(i)
            indices_i4(i) = i
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value1)
        call block_quick_argsort(vector_copy_r4, indices_i4, n_samples_i4)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r4, n_samples_i4)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"


    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, "Pseudo Bucket ArgSort, Random Uniform KIND=4"
    print*, "    number of samples: ", n_samples_i4
    tot_time = 0
    do iter=1, max_iter
        call random_number(vector_r4)
        do i=1, n_samples_i4, 1
            vector_copy_r4(i) = vector_r4(i)
            indices_i4(i) = i
        end do
        ! print*, "    is sorted(before): ", is_sorted(vector_copy_r4, n_samples_i4)
        call date_and_time(values=date_value1)
        call pbucket_argsort(vector_copy_r4, indices_i4, n_samples_i4)
        call date_and_time(values=date_value2)
        tot_time = tot_time + time_diff(date_value1, date_value2)
    end do
    print*, "    is sorted(after): ", is_sorted(vector_copy_r4, n_samples_i4)
    print*, "    Time: ", int(tot_time/dble(max_iter), kind=kind(max_iter)), "[msec]"


end program main_sort
