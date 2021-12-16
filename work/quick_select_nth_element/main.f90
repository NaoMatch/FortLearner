program main
    use mod_timer
    use mod_sort
    implicit none
    
    integer(kind=8) :: date_value1(8), date_value2(8)
    real(kind=8) :: med_val
    real(kind=8), ALLOCATABLE :: x(:)
    real(kind=8), ALLOCATABLE :: x_original(:)
    integer(kind=8) :: n_samples, med_idx


    n_samples = 111111
    med_idx = n_samples/2_8 + 1_8

    allocate(x(n_samples))
    allocate(x_original(n_samples))

    call RANDOM_NUMBER(x_original)
    print*, '============================================================='
    print*, "Print: ", med_idx
    x(:) = x_original(:)
    call quick_sort(x, n_samples)
    ! print*, "       ", x(med_idx)

    print*, '============================================================='
    print*, "Quick Select: ", med_idx
    x(:) = x_original(:)
call date_and_time(values=date_value1)
    call quick_select(x, n_samples, med_idx)
call date_and_time(values=date_value2)
print*, time_diff(date_value1, date_value2)
    print*, "       ", x(med_idx)

    print*, '============================================================='
    print*, "Sort: ", med_idx
    x(:) = x_original(:)
call date_and_time(values=date_value1)
    call quick_sort(x, n_samples)
call date_and_time(values=date_value2)
print*, time_diff(date_value1, date_value2)
    med_val = x(med_idx)
    print*, "       ", med_val


end program main
