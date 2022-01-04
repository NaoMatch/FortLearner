program main
    !$ use omp_lib
    use mod_nearest_neighbour
    use mod_exact_duplicate_search
    use mod_linalg
    use mod_timer
    implicit none
    
    type range
        integer(kind=8) :: ini
        integer(kind=8) :: fin
    end type range

    integer(kind=8) :: date_value1(8), date_value2(8)
    type(exact_duplicate_search) :: exdup
    type(duplicate_index), ALLOCATABLE :: dupdata(:)

    integer(kind=8) :: n_samples, n_columns, n, counter
    real(kind=8), ALLOCATABLE    :: x_r8(:,:)
    integer(kind=8), ALLOCATABLE :: x_i8(:,:), y_i8(:,:)
    integer(kind=8), ALLOCATABLE :: m(:), o(:),m2(:,:), o2(:,:)
    integer(kind=8), ALLOCATABLE :: indices(:)
    integer(kind=8) :: n_idx, f, i, j, idx, cnt, iter, n_iter
    type(range), ALLOCATABLE :: range_sets(:)


    n_iter = 10
    n_samples = 100000
    n_columns = 15
    allocate(x_r8(n_samples, n_columns))
    allocate(x_i8(n_samples, n_columns))
    allocate(indices(n_samples))

    call random_number(x_r8)
    x_i8 = 2*x_r8

    exdup = exact_duplicate_search()
    call date_and_time(values=date_value1)
    do iter=1, n_iter
        dupdata = exdup%search(x_i8)
    end do
    call date_and_time(values=date_value2)
    print*, "min_samples_in_leaf= 1: ", time_diff(date_value1, date_value2) / dble(n_iter)

    exdup = exact_duplicate_search(min_samples_in_leaf=32_8)
    call date_and_time(values=date_value1)
    do iter=1, n_iter
        dupdata = exdup%search(x_i8)
    end do
    call date_and_time(values=date_value2)
    print*, "min_samples_in_leaf=32: ", time_diff(date_value1, date_value2) / dble(n_iter)

    print*, size(dupdata)
end program main
