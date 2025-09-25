module mod_random_permutation
    use :: mod_kinds
    implicit none
    
contains

    subroutine random_permutation_i64(vector, num)
        implicit none
        integer(i64), intent(inout) :: vector(num)
        integer(i64), intent(in)    :: num
        integer(i64)                :: tmp, i, j
        integer(i64)                :: randpos
        real(r64)                   :: r
        real(r64), allocatable      :: rand_vals(:)
        allocate(rand_vals(num))
        call random_number(rand_vals)
        do i = num, 2, -1
            r = rand_vals(i)
            randpos = int(r * i, kind=i64) + 1_i64
            tmp = vector(randpos)
            vector(randpos) = vector(i)
            vector(i) = tmp
        end do
    end subroutine random_permutation_i64


end module mod_random_permutation
