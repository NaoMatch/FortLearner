program main_common_random_weighted_sampling
    use mod_random
    implicit none

    real(kind=8), allocatable :: weights(:), thresholds_(:)
    real(kind=8) :: w_sum, w_avg
    integer(kind=8), allocatable :: new_indices(:), counter(:), candicates_(:)
    integer(kind=8) :: n_samples, n_weights, n, l, h, i, j, k, idx



    n_samples = 1000000
    n_weights = 5
    allocate(weights(n_weights))
    allocate(counter(n_weights))
    allocate(thresholds_(n_weights))
    allocate(candicates_(n_weights))
    do i=1, n_weights, 1
        weights(i) = i
    end do

    call fix_random_seed(1_8)
    allocate(new_indices(n_samples))
    call weighted_sampling(new_indices, n_samples, weights, n_weights)
    counter(:) = 0
    do i=1, n_samples, 1
        idx = new_indices(i)
        counter(idx) = counter(idx) + 1
    end do

    print*, '*********************************************************************************************'
    print*, real(counter / dble(maxval(counter))*n_weights)

    call release_random_seed()
    call preprocess_for_weighted_sampling(thresholds_, candicates_, weights, n_weights)
    call weighted_sampling(new_indices, n_samples, thresholds_, candicates_, n_weights)
    counter(:) = 0
    do i=1, n_samples, 1
        idx = new_indices(i)
        counter(idx) = counter(idx) + 1
    end do

    print*, '*********************************************************************************************'
    print*, real(counter / dble(maxval(counter))*n_weights)
    

contains


end program main_common_random_weighted_sampling