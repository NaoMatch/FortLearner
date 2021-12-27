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

    !> Interface to call rand_integer_i4, rand_integer_i8
    interface rand_integer
        module procedure rand_integer_i4
        module procedure rand_integer_i8
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

end module mod_random