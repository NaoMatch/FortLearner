program main_neighbour_exact_duplication_search
    use mod_exact_duplicate_search
    use mod_timer
    implicit none
    integer(kind=8)        :: date_value1(8), date_value2(8)
    CHARACTER(len=256) :: file_name_x_train_bin
    real(kind=8), allocatable :: x(:,:)
    real(kind=8), allocatable :: v1(:), v2(:)
    type(duplicate_index), allocatable :: res(:)
    type(exact_duplication_search) :: ex_dup 

    file_name_x_train_bin = "../sample_data/HIGGS_X.bin"
    ! allocate(x(100000,1))
    ! call random_number(x)
    ! x(21,:) = x(1,:)
    ! x(100,:) = x(2,:)
    call date_and_time(values=date_value1)
    call read_bin_2d(file_name_x_train_bin, x)
    call date_and_time(values=date_value2)
    print*, "Read: ", time_diff(date_value1, date_value2)
    
    ex_dup = exact_duplication_search()
    call date_and_time(values=date_value1)
    res = ex_dup%get_exact_duplication_row_indices(x, check_raw_value=f_)
    call date_and_time(values=date_value2)
    print*, "Duplication Check: ", time_diff(date_value1, date_value2), size(res)

    call date_and_time(values=date_value1)
    res = ex_dup%get_exact_duplication_row_indices(x, check_raw_value=t_)
    call date_and_time(values=date_value2)
    print*, "Duplication Check: ", time_diff(date_value1, date_value2), size(res)
end program main_neighbour_exact_duplication_search