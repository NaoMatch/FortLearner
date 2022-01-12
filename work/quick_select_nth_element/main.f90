program main
    use mod_timer
    use mod_sort
    use mod_const
    implicit none
    
    integer(kind=8) :: date_value1(8), date_value2(8)
    real(kind=8) :: med_val
    real(kind=8), ALLOCATABLE :: x(:)
    real(kind=8), ALLOCATABLE :: x_original(:)
    integer(kind=8), ALLOCATABLE :: x_int(:)
    integer(kind=8), ALLOCATABLE :: x_copy_0(:), x_copy_1(:)
    integer(kind=8) :: n_samples, med_idx, i
    integer(kind=8) :: depth, max_val
    integer(kind=8) :: iter, max_iter
    character(len=1) :: ast

    depth = 0
    n_samples = 500
    med_idx = n_samples/2_8
    max_val = 10
    max_iter = 1

    allocate(x(n_samples))
    allocate(x_original(n_samples))
    allocate(x_copy_0(n_samples))
    allocate(x_copy_1(n_samples))
    allocate(x_int(n_samples))

    call RANDOM_NUMBER(x_original)
    x_int = int(max_val*x_original)
    print*, '============================================================='
    print*, "Quick Select: ", med_idx
    x_copy_0(:) = x_int(:)

    print*, '============================================================='
    print*, 'NAIVE -------------------------------------------------------'
    print*, '============================================================='
    ! x_copy_0(:) = 10
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        x_copy_0(:) = x_int(:)
        call quick_select(x_copy_0, n_samples, med_idx, left_align=f_)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

    print*, '============================================================='
    print*, 'ALIGNED -----------------------------------------------------'
    print*, '============================================================='
    x_copy_1(:) = 10
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        x_copy_1(:) = x_int(:)
        call quick_select(x_copy_1, n_samples, med_idx, left_align=t_)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

    print*, '============================================================='
    print*, 'ALIGNED -----------------------------------------------------'
    print*, '============================================================='
    x_copy_1(:) = 10
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        x_copy_1(:) = x_int(:)
        call quick_argselect(x_copy_1, x_copy_0, n_samples, med_idx, left_align=t_)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

    do i=1, n_samples, 1
        if (i .eq. med_idx) then
            ast = "*"
        else
            ast = ""
        end if
        print*, i, x_copy_0(i), x_copy_1(i), x_copy_0(i) == x_copy_1(i), x_int(i), ast, x_copy_1(i) <= x_copy_1(med_idx)
    end do
    
    print*, '============================================================='
    print*, '============================================================='
    print*, '============================================================='
    print*, sum(x_copy_0), sum(x_copy_1)


contains

end program main
