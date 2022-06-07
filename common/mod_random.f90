!> A module for random operations.
module mod_random
    use mod_const
    use mod_common
    implicit none

    !> Interface to call rand_normal_1d_r4, rand_normal_1d_r8
    interface rand_normal
        module procedure rand_normal_1d_r4
        module procedure rand_normal_r8
        module procedure rand_normal_1d_r8
        module procedure rand_normal_2d_r8
    end interface ! rand_normal

    interface rand_uniform
        module procedure rand_uniform_r4
        module procedure rand_uniform_r8
        module procedure rand_uniform_1d_r4
        module procedure rand_uniform_1d_r8
    end interface rand_uniform

    !> Interface to call permutation_r4, permutation_r8, permutation_i4, permutation_i8
    interface permutation
        module procedure permutation_r4
        module procedure permutation_r8
        module procedure permutation_i4
        module procedure permutation_i8
    end interface ! permutation

    !> Interface to call permutation_head_r4, permutation_head_r8, permutation_head_i4, permutation_head_i8
    interface permutation_head
        module procedure permutation_head_r4
        module procedure permutation_head_r8
        module procedure permutation_head_i4
        module procedure permutation_head_i8
    end interface ! permutation_head

    interface preprocess_for_weighted_sampling
        module procedure preprocess_for_weighted_sampling_r8_i8
    end interface preprocess_for_weighted_sampling

    !> Interface to call rand_integer_i4, rand_integer_i8
    interface rand_integer
        module procedure rand_integer_scl_i4
        module procedure rand_integer_scl_i8
        module procedure rand_integer_i4
        module procedure rand_integer_i8
    end interface ! rand_integer

    !> Interface to call roulette_selection_r4, roulette_selection_r8
    interface roulette_selection
        module procedure roulette_selection_r4
        module procedure roulette_selection_r8
    end interface roulette_selection

    interface weighted_sampling
        module procedure weighted_sampling_with_preprocess_r8
        module procedure weighted_sampling_without_preprocess_r8
    end interface weighted_sampling


contains

    !> A function to roulette selection from 'vector'
    !> 'vector' must be normalized, sum(vector) P== 1.
    !! \return returns selected vector index
    !! \param vector normalized vector
    !! \param n_samples number of samples
    !! \param reverse if true, search from last element.
    function roulette_selection_r4(vector, n_samples, reverse)
        real(kind=4), intent(inout) :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        logical(kind=4), intent(in) :: reverse
        integer(kind=4) :: roulette_selection_r4
        integer(kind=4) :: idx, i
        real(kind=4) :: rand, cumsum
        include "./include/random/roulette_selection/inc_roulette_selection_detail.f90"
        roulette_selection_r4 = idx
    end function roulette_selection_r4
    include "./include/random/roulette_selection/inc_roulette_selection.f90"


    !> A subroutine to generate normally distributed random number of 1-dim.
    !> Subroutines of other data types (rand_normal_1d_real64) are stored in './common/include/rand_normal/'. \n
    !! \return array input 1-dim vector
    !! \param num size of input 1-dim vector
    subroutine rand_normal_1d_r4(array, num)
        implicit none
        real(kind=4), intent(inout) :: array(num)
        integer(kind=4), intent(in) :: num
        real(kind=4), allocatable :: tmp(:)
        real(kind=4) :: two
        allocate(tmp(num))
        include "./include/random/rand_normal/inc_rand_normal_detail.f90"
    end subroutine rand_normal_1d_r4
    include "./include/random/rand_normal/inc_rand_normal.f90"


    !> A subroutine to generate uniformly distributed random number of 1-dim.
    !! \return vector input 1-dim vector
    !! \param num size of input 1-dim vector
    subroutine rand_uniform_1d_r4(vector, min_val, max_val, num)
        implicit none
        real(kind=4), intent(inout) :: vector(num)
        real(kind=4), intent(in)    :: min_val, max_val
        integer(kind=4), intent(in) :: num
        call random_number(vector)
        vector = (max_val-min_val) * vector + min_val
    end subroutine rand_uniform_1d_r4
    include "./include/random/rand_uniform/inc_rand_uniform.f90"


    !> A subroutine to randomly permutate input vector
    !! \return vector input 1-dim vector
    !! \param num size of input 1-dim vector
    subroutine permutation_r4(vector, num)
        implicit none
        real(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in) :: num
        integer(kind=4)             :: tmp, i, j
        integer(kind=8)             :: randpos, one
        real(kind=4)                :: r, rand_vals(num)
        include "./include/random/permutation/inc_permutation_detail.f90"
    end subroutine permutation_r4
    include "./include/random/permutation/inc_permutation.f90"


    !> A subroutine to randomly permutate the first N of input vector
    !! \return vector input 1-dim vector
    !! \param num size of input 1-dim vector
    !! \param n_head number of pieces to be replaced at random
    subroutine permutation_head_r4(vector, num, n_head)
        implicit none
        real(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in) :: num
        integer(kind=4), intent(in) :: n_head
        integer(kind=4)             :: tmp, i, j
        integer(kind=4)             :: randpos, one
        real(kind=4)                :: r, rand_vals(n_head)
        include "./include/random/permutation_head/inc_permutation_head_detail.f90"
    end subroutine permutation_head_r4
    include "./include/random/permutation_head/inc_permutation_head.f90"


    subroutine preprocess_for_weighted_sampling_r8_i8(thresholds, candidates, weights, n_weights)
        implicit none
        real(kind=8), intent(inout) :: thresholds(n_weights)
        integer(kind=8), intent(inout) :: candidates(n_weights)
        real(kind=8), intent(in) :: weights(n_weights)
        integer(kind=8), intent(in) :: n_weights

        real(kind=8)                   :: weight_sum
        integer(kind=8)                :: h, l, i, j, k, idx
        integer(kind=8), allocatable   :: hl(:)

        weight_sum = sum(weights)
        thresholds(:) = weights(:) * n_weights / weight_sum 

        allocate(hl(n_weights))
        candidates(:) = 1_8
        hl(:) = 1_8
        l = 1
        h = n_weights        

        do i=1, n_weights, 1
            if ( thresholds(i) < 1d0 ) then
                hl(l) = i
                l = l + 1
            else
                hl(h) = i
                h = h - 1
            end if
        end do
    
        do while ( (l>1) .and. (h<n_weights) ) 
            j = hl(l-1)
            k = hl(h+1)
    
            candidates(j) = k
            thresholds(k) = thresholds(k) + thresholds(j) - 1d0
            if (thresholds(k)<1d0) then
                hl(l-1) = k
                h = h + 1
            else
                l = l - 1
            end if
        end do 
    end subroutine preprocess_for_weighted_sampling_r8_i8


    !> A subroutine to generate random integer with duplication
    !! \param lo minimum value
    !! \param hi maximum value
    !! \return vector output vector
    !! \param num the size of vector
    subroutine rand_integer_i4(lo, hi, vector, num)
        implicit none
        integer(kind=4), intent(in)    :: lo, hi
        integer(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in)    :: num
        
        real(kind=4), allocatable      :: tmp_array(:)
        integer(kind=4)                :: i, j, factor, min, tmp
        integer(kind=4)                :: unroll, buffer(15)

        include "./include/random/rand_integer/inc_rand_integer_detail.f90"
    end subroutine rand_integer_i4
    include "./include/random/rand_integer/inc_rand_integer.f90"

    subroutine rand_integer_scl_i8(lo, hi, val)
        implicit none
        integer(kind=8), intent(in)    :: lo, hi
        integer(kind=8) :: val
        real(kind=8) :: tmp
        call RANDOM_NUMBER(tmp)
        val = (hi-lo)*tmp + lo + 1_8
    end subroutine rand_integer_scl_i8

    subroutine rand_integer_scl_i4(lo, hi, val)
        implicit none
        integer(kind=4), intent(in)    :: lo, hi
        integer(kind=4) :: val
        real(kind=4) :: tmp
        call RANDOM_NUMBER(tmp)
        val = (hi-lo)*tmp + lo + 1
    end subroutine rand_integer_scl_i4


    subroutine weighted_sampling_with_preprocess_r8(indices, n_samples, weights, n_weights)
        implicit none
        integer(kind=8), intent(inout) :: indices(n_samples)
        integer(kind=8), intent(in)    :: n_samples
        real(kind=8), intent(in)       :: weights(n_weights)
        integer(kind=8), intent(in)    :: n_weights

        real(kind=8)                   :: weight_sum
        integer(kind=8)                :: h, l, i, j, k, idx
        real(kind=8), allocatable      :: thresholds(:), rand_vals(:)
        integer(kind=8), allocatable   :: candidates(:), hl(:), rand_idxs(:)

        allocate(thresholds(n_weights))
        weight_sum = sum(weights)
        thresholds(:) = weights(:) * n_weights / weight_sum 

        allocate(candidates(n_weights), hl(n_weights))
        candidates(:) = 1_8
        hl(:) = 1_8
        l = 1
        h = n_weights        

        do i=1, n_weights, 1
            if ( thresholds(i) < 1d0 ) then
                hl(l) = i
                l = l + 1
            else
                hl(h) = i
                h = h - 1
            end if
        end do
    
        do while ( (l>1) .and. (h<n_weights) ) 
            j = hl(l-1)
            k = hl(h+1)
    
            candidates(j) = k
            thresholds(k) = thresholds(k) + thresholds(j) - 1d0
            if (thresholds(k)<1d0) then
                hl(l-1) = k
                h = h + 1
            else
                l = l - 1
            end if
        end do 
            
        allocate(rand_vals(n_samples), rand_idxs(n_samples))
        call random_number(rand_vals)
        rand_idxs = int(rand_vals*n_weights) + 1
        call random_number(rand_vals)
    
        do i=1, n_samples, 1
            idx = rand_idxs(i)
            if ( rand_vals(i)<=thresholds(idx) ) then
                indices(i) = idx
            else
                indices(i) = candidates(idx)
            end if
        end do
    end subroutine weighted_sampling_with_preprocess_r8

    subroutine weighted_sampling_without_preprocess_r8(indices, n_samples, thresholds, candidates, n_weights)
        implicit none
        integer(kind=8), intent(inout) :: indices(n_samples)
        integer(kind=8), intent(in)    :: n_samples
        real(kind=8), intent(in)       :: thresholds(n_weights)
        integer(kind=8), intent(in)    :: candidates(n_weights)
        integer(kind=8), intent(in)    :: n_weights

        integer(kind=8)                :: idx, i
        real(kind=8), allocatable      :: rand_vals(:)
        integer(kind=8), allocatable   :: rand_idxs(:)

        allocate(rand_vals(n_samples), rand_idxs(n_samples))
        call random_number(rand_vals)
        rand_idxs = int(rand_vals*n_weights) + 1
        call random_number(rand_vals)
    
        do i=1, n_samples, 1
            idx = rand_idxs(i)
            if ( rand_vals(i)<=thresholds(idx) ) then
                indices(i) = idx
            else
                indices(i) = candidates(idx)
            end if
        end do
    end subroutine weighted_sampling_without_preprocess_r8

end module mod_random