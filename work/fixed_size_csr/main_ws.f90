program main
    use mod_const
    use mod_random
    use mod_common
    use mod_timer
    use mod_sort
    implicit none
    
    integer(kind=8) :: n_samples, n_weights
    integer(kind=8), allocatable :: idxs(:), counter(:)
    real(kind=8), allocatable :: wght(:)
    integer(kind=8)        :: date_value1(8), date_value2(8)
    integer(kind=8) :: iter, max_iter, idx, i

    real(kind=8), allocatable :: a(:,:), b(:,:), c(:,:)


    call fix_random_seed(42_8)
    n_samples = 16
    n_weights = 128
    max_iter = 10000_8

    allocate(a(n_weights, n_weights), b(n_weights, n_weights))
    allocate(wght(n_weights))
    allocate(counter(n_weights))

    wght(:) = 1d0
    ! wght(3) = -.5d0
    ! wght(5) = -.5d0
    ! wght(3) = 0d0
    wght(:) = (/(i, i=1, n_weights, 1)/)
    ! wght(3) = -0d0
    ! wght(5) = -1d0
    ! wght(7) = -2d0


    print*, '*********************************************************************************************'
    print*, "Original Weights"
    print*, int(wght)
    
    print*, '*********************************************************************************************'
    counter(:) = 0
    call date_and_time(values=date_value1)
    do iter=1, n_weights * max_iter
        call weighted_sampling(idxs, n_samples, wght, n_weights, replace=t_, negative_weights="filter")
        do idx=1, size(idxs), 1
            counter(idxs(idx)) = counter(idxs(idx)) + 1
        end do
    end do
    call date_and_time(values=date_value2)
    print*, "Repacement = True, negative_weights = filter                       : ", time_diff(date_value1, date_value2)
    print*, sum(counter)
    ! print*, int(clipper(wght))
    print*, int(counter(:10)), sum(counter), sum(counter)
    
    ! print*, '*********************************************************************************************'
    ! counter(:) = 0
    ! call date_and_time(values=date_value1)
    ! do iter=1, n_weights * max_iter
    !     call weighted_sampling(idxs, n_samples, wght, n_weights, replace=t_, negative_weights="shift")
    !     do idx=1, size(idxs), 1
    !         counter(idxs(idx)) = counter(idxs(idx)) + 1
    !     end do
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Repacement = True, negative_weights = shift                       : ", time_diff(date_value1, date_value2)
    ! print*, sum(counter)
    ! print*, int(wght-minval(wght))
    ! print*, int(counter(:10)), sum(counter)
    
    ! print*, '*********************************************************************************************'
    ! counter(:) = 0
    ! call date_and_time(values=date_value1)
    ! do iter=1, n_weights * max_iter
    !     call weighted_sampling(idxs, n_samples, wght, n_weights, replace=t_, negative_weights="absolute")
    !     do idx=1, size(idxs), 1
    !         counter(idxs(idx)) = counter(idxs(idx)) + 1
    !     end do
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Repacement = True, negative_weights = absolute                       : ", time_diff(date_value1, date_value2)
    ! print*, sum(counter)
    ! print*, int(abs(wght))
    ! print*, int(counter(:10)), sum(counter)
    



    print*, '*********************************************************************************************'
    counter(:) = 0
    call date_and_time(values=date_value1)
    do iter=1, n_weights * max_iter
        call weighted_sampling(idxs, n_samples, wght, n_weights, replace=f_, negative_weights="filter")
        do idx=1, size(idxs), 1
            counter(idxs(idx)) = counter(idxs(idx)) + 1
        end do
    end do
    call date_and_time(values=date_value2)
    print*, "Repacement = False, negative_weights = filter                       : ", time_diff(date_value1, date_value2)
    print*, sum(counter)
    ! print*, int(clipper(wght))
    print*, int(counter(:10)), sum(counter)

    print*, '*********************************************************************************************'
    counter(:) = 0
    call date_and_time(values=date_value1)
    do iter=1, n_weights * max_iter
        call weighted_sampling_without_replacement_rejction(idxs, n_samples, wght, n_weights, how="filter")
        do idx=1, size(idxs), 1
            counter(idxs(idx)) = counter(idxs(idx)) + 1
        end do
    end do
    call date_and_time(values=date_value2)
    print*, "Repacement = False, negative_weights = filter REJC                 : ", time_diff(date_value1, date_value2)
    print*, sum(counter)
    ! print*, int(clipper(wght))
    print*, int(counter(:10)), sum(counter)
    
    ! print*, '*********************************************************************************************'
    ! counter(:) = 0
    ! call date_and_time(values=date_value1)
    ! do iter=1, n_weights * max_iter
    !     call weighted_sampling(idxs, n_samples, wght, n_weights, replace=f_, negative_weights="shift")
    !     do idx=1, size(idxs), 1
    !         counter(idxs(idx)) = counter(idxs(idx)) + 1
    !     end do
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Repacement = False, negative_weights = shift                       : ", time_diff(date_value1, date_value2)
    ! print*, sum(counter)
    ! print*, int(wght-minval(wght))
    ! print*, int(counter(:10)), sum(counter)
    
    ! print*, '*********************************************************************************************'
    ! counter(:) = 0
    ! call date_and_time(values=date_value1)
    ! do iter=1, n_weights * max_iter
    !     call weighted_sampling(idxs, n_samples, wght, n_weights, replace=f_, negative_weights="absolute")
    !     do idx=1, size(idxs), 1
    !         counter(idxs(idx)) = counter(idxs(idx)) + 1
    !     end do
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Repacement = False, negative_weights = absolute                       : ", time_diff(date_value1, date_value2)
    ! print*, sum(counter)
    ! print*, int(abs(wght))
    ! print*, int(counter(:10)), sum(counter)

contains

    ! Efraimidis and Spirakis
    subroutine weighted_sampling_with_es(indices, n_samples, weights, n_weights)
        implicit none
        integer(kind=8), intent(inout) :: indices(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in) :: weights(n_weights)
        integer(kind=8), intent(in) :: n_weights

        integer(kind=8) :: i, w
        real(kind=8), allocatable :: keys(:)
        integer(kind=8), allocatable :: idxs(:)

        allocate(keys(n_weights), idxs(n_weights))
        call random_number(keys)
        idxs = (/(i, i=1, n_weights, 1)/)


        keys = keys ** (1d0 / weights)

        call quick_argsort(keys, idxs, n_weights)

        do i=1, n_samples, 1
            indices(i) = idxs(n_weights-i+1)
        end do
    end subroutine weighted_sampling_with_es

    subroutine weighted_sampling_cumsum_binary_with_replacement(indices, n_samples, weights, n_weights)
        implicit none
        integer(kind=8), intent(inout) :: indices(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in) :: weights(n_weights)
        integer(kind=8), intent(in) :: n_weights

        integer(kind=8) :: i, idx
        real(kind=8) :: val, max_c
        real(kind=8), allocatable :: c(:)

        allocate(c(n_weights))
        call prefix_sum(weights, c, n_weights)

        max_c = c(n_weights)
        do i=1, n_samples, 1
            call random_number(val)
            val = val * max_c
            idx = binary_search_left_branchless(c, n_weights, val)
            indices(i) = idx
        end do
    end subroutine weighted_sampling_cumsum_binary_with_replacement

    subroutine weighted_sampling_cumsum_binary_without_replacement_2(indices, n_samples, weights, n_weights)
        implicit none
        integer(kind=8), intent(inout) :: indices(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in) :: weights(n_weights)
        integer(kind=8), intent(in) :: n_weights

        integer(kind=8) :: i, idx
        real(kind=8) :: val, max_c
        real(kind=8), allocatable :: c(:)

        allocate(c(n_weights))
        call prefix_sum(weights, c, n_weights)

        do i=1, n_samples, 1
            max_c = c(n_weights)
            call random_number(val)
            val = val * max_c
            idx = binary_search_left_branchless(c, n_weights, val)
            c(idx:) = c(idx:) - weights(idx)
            indices(i) = idx
        end do
    end subroutine weighted_sampling_cumsum_binary_without_replacement_2

    elemental function clipper(x) result(y)
        implicit none
        real(kind=8), intent(in) :: x
        real(kind=8) :: y
        y = maxval([x, 0d0])
    end function 

    subroutine weighted_sampling_resorvor_expj_wo_rplc(indices, n_samples, weights, n_weights, how)
        implicit none
        integer(kind=8), allocatable :: indices(:)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in) :: weights(n_weights)
        integer(kind=8), intent(in) :: n_weights
        character(len=*), intent(in) :: how

        real(kind=8), allocatable :: keys(:), w_copy(:)
        real(kind=8) :: min_w
        integer(kind=8) :: k
        integer(kind=4) :: k_size

        allocate(w_copy, source=weights)
        if (how=="filter") then
            call clipping_array_lower(w_copy, n_weights, x_min=0d0)
        elseif (how=="shift") then
            min_w = minval(w_copy)
            w_copy(:) = w_copy(:) - min_w
        elseif (how=="absolute") then
            w_copy = abs(w_copy)
        else
            if (minval(w_copy)<0d0) then
                stop "argument 'weights' contains negative value(s) in 'weighted_sampling'."
            end if
        end if

        k_size = n_weights/16_8
        allocate(keys(k_size))
        do k=1, 16, 1
            ! call random_number(keys)
            call uni_array(keys, k_size)
        end do
        ! keys(:) = keys(:) ** (1d0 / weights(:))

    end subroutine weighted_sampling_resorvor_expj_wo_rplc


end program main
    
    ! counter(:) = 0
    ! call date_and_time(values=date_value1)
    ! do iter=1, n_weights * max_iter
    !     call weighted_sampling_cumsum_binary_with_replacement(idxs, n_samples, wght, n_weights)
    !     do idx=1, n_samples, 1
    !         counter(idxs(idx)) = counter(idxs(idx)) + 1
    !     end do
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Cumsum and Binary with Replacement         : ", time_diff(date_value1, date_value2)
    ! print*, sum(counter)
    ! print*, int(counter)

    ! counter(:) = 0
    ! call date_and_time(values=date_value1)
    ! do iter=1, n_weights * max_iter
    !     call weighted_sampling_cumsum_binary_without_replacement_2(idxs, n_samples, wght, n_weights)
    !     do idx=1, n_samples, 1
    !         counter(idxs(idx)) = counter(idxs(idx)) + 1
    !     end do
    ! end do
    ! call date_and_time(values=date_value2)
    ! print*, "Cumsum and Binary without Replacement      : ", time_diff(date_value1, date_value2)
    ! print*, sum(counter)
    ! print*, int(counter)