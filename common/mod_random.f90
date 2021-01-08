!> A module for random operations.
module mod_random
    use mod_const
    use mod_common
    implicit none

    !> Interface to call rand_normal_real32_1d, rand_normal_real64_1d
    interface rand_normal
        module procedure rand_normal_real32_1d
        module procedure rand_normal_real64_1d
    end interface ! rand_normal

    !> Interface to call permutation_real32, permutation_real64, permutation_int32, permutation_int64
    interface permutation
        module procedure permutation_real32
        module procedure permutation_real64
        module procedure permutation_int32
        module procedure permutation_int64
    end interface ! permutation

    !> Interface to call permutation_head_real32, permutation_head_real64, permutation_head_int32, permutation_head_int64
    interface permutation_head
        module procedure permutation_head_real32
        module procedure permutation_head_real64
        module procedure permutation_head_int32
        module procedure permutation_head_int64
    end interface ! permutation_head

    !> Interface to call rand_integer_int32, rand_integer_int64
    interface rand_integer
        module procedure rand_integer_int32
        module procedure rand_integer_int64
    end interface ! rand_integer

    !> Interface to call roulette_selection_r4, roulette_selection_r8
    interface roulette_selection
        module procedure roulette_selection_r4
        module procedure roulette_selection_r8
    end interface roulette_selection


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
        call random_number(rand)
        cumsum = 0.0
        if (reverse) then
            do i=n_samples, 1, -1
                cumsum = cumsum + vector(i)
                if (cumsum .ge. rand) then
                    idx = i
                    exit
                end if
            end do
        else
            do i=1, n_samples, 1
                cumsum = cumsum + vector(i)
                if (cumsum .ge. rand) then
                    idx = i
                    exit
                end if
            end do
        end if
        roulette_selection_r4 = idx
    end function roulette_selection_r4
    include "./include/random_roulette_selection/inc_roulette_selection.f90"


    !> A subroutine to generate normally distributed random number of 1-dim.
    !> Subroutines of other data types (rand_normal_real64_1d) are stored in './common/include/rand_normal/'. \n
    !! \return vector input 1-dim vector
    !! \param num size of input 1-dim vector
    subroutine rand_normal_real32_1d(vector, num)
        implicit none
        real(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in) :: num
        real(kind=4), allocatable :: tmp(:)
        real(kind=4) :: two
        allocate(tmp(num))
        two = 2.0
        include "./include/random_rand_normal/inc_rand_normal_detail.f90"
    end subroutine rand_normal_real32_1d
    include "./include/random_rand_normal/inc_rand_normal.f90"


    !> A subroutine to randomly permutate input vector
    !! \return vector input 1-dim vector
    !! \param num size of input 1-dim vector
    subroutine permutation_real32(vector, num)
        implicit none
        real(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in) :: num
        integer(kind=4)             :: tmp, i, j
        integer(kind=8)             :: randpos
        real(kind=4)                :: r, rand_vals(num)
        include "./include/random_permutation/inc_permutation_detail.f90"
    end subroutine permutation_real32
    include "./include/random_permutation/inc_permutation.f90"


    !> A subroutine to randomly permutate the first N of input vector
    !! \return vector input 1-dim vector
    !! \param num size of input 1-dim vector
    !! \param n_head number of pieces to be replaced at random
    subroutine permutation_head_real32(vector, num, n_head)
        implicit none
        real(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in) :: num
        integer(kind=4), intent(in) :: n_head
        integer(kind=4)             :: tmp, i, j
        integer(kind=8)             :: randpos
        real(kind=4)                :: r, rand_vals(n_head)
        include "./include/random_permutation_head/inc_permutation_head_detail.f90"
    end subroutine permutation_head_real32
    include "./include/random_permutation_head/inc_permutation_head.f90"


    !> A subroutine to generate random integer with duplication
    !! \param lo minimum value
    !! \param hi maximum value
    !! \return vector output vector
    !! \param num the size of vector
    subroutine rand_integer_int32(lo, hi, vector, num)
        implicit none
        integer(kind=4), intent(in)    :: lo, hi
        integer(kind=4), intent(inout) :: vector(num)
        integer(kind=4), intent(in)    :: num
        
        real(kind=4), allocatable      :: tmp_array(:)
        integer(kind=4)                :: i, j, factor, min, tmp
        integer(kind=4)                :: unroll, buffer(15)

        factor = hi - lo + 1
        allocate(tmp_array(num))
        call random_number(tmp_array)
        unroll = num - mod(num, 15)
        do i=1, unroll, 15
            do j=0, 15-1, 1
                buffer(j+1) = int(factor * tmp_array(i+j), kind=4) + lo
            end do

            do j=0, 15-1, 1
                vector(i+j) = buffer(j+1)
            end do
        end do
        do i=unroll+1, num
            vector(i) = int(factor * tmp_array(i), kind=4) + lo
        end do
        deallocate(tmp_array)
    end subroutine rand_integer_int32
    include "./include/random_rand_integer/inc_rand_integer.f90"

end module mod_random