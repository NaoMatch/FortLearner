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


    n_samples = 8
    n_weights = 10
    max_iter = 1000_8


    allocate(idxs(n_samples))
    allocate(wght(n_weights))
    allocate(counter(n_weights))

    wght(:) = 1d0
    wght(:) = (/(i, i=1, n_weights, 1)/)
    ! wght = wght / sum(wght)
    
    counter(:) = 0
    call date_and_time(values=date_value1)
    do iter=1, n_weights * max_iter
        call weighted_sampling_cumsum_binary_with_replacement(idxs, n_samples, wght, n_weights)
        do idx=1, n_samples, 1
            counter(idxs(idx)) = counter(idxs(idx)) + 1
        end do
    end do
    call date_and_time(values=date_value2)
    print*, "Cumsum and Binary with Replacement         : ", time_diff(date_value1, date_value2)
    print*, sum(counter)
    print*, counter

    counter(:) = 0
    call date_and_time(values=date_value1)
    do iter=1, n_weights * max_iter
        call weighted_sampling_cumsum_binary_without_replacement_2(idxs, n_samples, wght, n_weights)
        do idx=1, n_samples, 1
            counter(idxs(idx)) = counter(idxs(idx)) + 1
        end do
    end do
    call date_and_time(values=date_value2)
    print*, "Cumsum and Binary without Replacement      : ", time_diff(date_value1, date_value2)
    print*, sum(counter)
    print*, counter
    
    counter(:) = 0
    call date_and_time(values=date_value1)
    do iter=1, n_weights * max_iter
        call weighted_sampling(idxs, n_samples, wght, n_weights, replace=t_)
        do idx=1, n_samples, 1
            counter(idxs(idx)) = counter(idxs(idx)) + 1
        end do
    end do
    call date_and_time(values=date_value2)
    print*, "Naive Implementation True                       : ", time_diff(date_value1, date_value2)
    print*, sum(counter)
    print*, counter
    
    counter(:) = 0
    call date_and_time(values=date_value1)
    do iter=1, n_weights * max_iter
        call weighted_sampling(idxs, n_samples, wght, n_weights, replace=f_)
        do idx=1, n_samples, 1
            counter(idxs(idx)) = counter(idxs(idx)) + 1
        end do
    end do
    call date_and_time(values=date_value2)
    print*, "Naive Implementation False                       : ", time_diff(date_value1, date_value2)
    print*, sum(counter)
    print*, counter


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

end program main