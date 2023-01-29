program main_hash_hash_map
    use mod_hash
    use mod_hash_map
    use mod_hash_table
    use mod_timer
    use mod_random
    implicit none

    integer(kind=8)        :: date_value1(8), date_value2(8)

    integer(kind=8), allocatable :: keys(:,:)

    integer(kind=8) :: key, status
    integer(kind=8) :: hash_val_oath, hash_val_xors, val, val_tot
    real(kind=8), allocatable :: vals(:)
    type(hash_map_keyi8_vali8)  :: hmap
    type(hash_map_keyi8_valsr8)  :: hmap_
    integer(kind=8), allocatable :: counter(:)
    integer(kind=8), allocatable :: rand_indices_1(:), rand_indices_2(:)

    real(kind=8), allocatable :: x(:, :)

    integer(kind=8) :: i, imax=32-1, hash_val, j, jmax=100000
    integer(kind=8) :: idx_1, idx_2

    key = 101_8

    ! hmap = hash_map_keyi8_vali8()

    hmap_ = hash_map_keyi8_valsr8()

    call hmap_%insert(1_8, [1d0, 2d0])
    vals = hmap_%query(1_8, status)
    print*, vals

    call hmap_%upsert(1_8, [2d0])
    vals = hmap_%query(1_8, status)
    print*, vals



    ! call hmap%insert([1_8], 123_8)
    ! call hmap%insert([2_8], 234_8)

    ! val = hmap%query(1_8, status)
    ! print*, val, status

    ! val = hmap%query(2_8, status)
    ! print*, val, status

    ! val = hmap%query(3_8, status)
    ! print*, val, status

    ! stop
    
    ! allocate(x(10, 2))
    ! call random_number(x)
    ! allocate(rand_indices_1(jmax))
    ! allocate(rand_indices_2(jmax))
    ! call rand_integer(1_8, 10_8, rand_indices_1, jmax)
    ! call rand_integer(1_8, 10_8, rand_indices_2, jmax)

    ! call date_and_time(values=date_value1)
    ! do j=1, jmax, 1
    !     idx_1 = rand_indices_1(j)
    !     idx_2 = rand_indices_2(j)
    !     val = 10*exp(- sum(x(idx_1,:)-x(idx_2,:))**2d0 * 10d0)
    !     call hmap%upsert([j, 2*j], val)
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Insertion: ", time_diff(date_value1, date_value2), " [msec]"
    


    ! val_tot = 0
    ! call date_and_time(values=date_value1)
    ! do j=1, jmax, 1
    !     val = hmap%query([i, 2*i], status)
    !     val_tot = val_tot + val
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Query:     ", time_diff(date_value1, date_value2), " [msec]", val_tot



    ! val_tot = 0
    ! call date_and_time(values=date_value1)
    ! do j=1, jmax, 1
    !     idx_1 = rand_indices_1(j)
    !     idx_2 = rand_indices_2(j)
    !     val = 10*exp(- sum(x(idx_1,:)-x(idx_2,:))**2d0 * 10d0)
    !     val_tot = val_tot + val
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Naive:     ", time_diff(date_value1, date_value2), " [msec]", val_tot

    ! ! do i=0, 8-1, 1
    ! !     print*, i, hmap%get_list_size(i)
    ! ! end do

    ! do i=1, 8, 1
    !     val = hmap%query([i, 2*i], status)
    !     print*, [i, 2*i], val, status
    ! end do
    
end program main_hash_hash_map