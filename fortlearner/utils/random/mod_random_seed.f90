module mod_random_seed
    use :: mod_kinds
    use :: mod_datetime
    implicit none
    
contains
    !> A function to generate random 32-bit integers.
    !> Marsaglia, G. & Tsang, W.W. (2000) `The ziggurat method for generating random variables', J. Statist. Software, v5(8).
    !> Original implementation taken from https://fortran-lang.discourse.group/t/reproducible-use-of-random-number-with-a-single-integer-seed/3593.
    pure elemental subroutine random_int_32_scalar(jsr,iran)
        integer(i64), intent(in out) :: jsr  ! state of RNG
        integer, intent(out)    :: iran ! random integer
        integer                 :: jz
        jz   = jsr
        jsr  = ieor(jsr, ishft(jsr,  13))
        jsr  = ieor(jsr, ishft(jsr, -17))
        jsr  = ieor(jsr, ishft(jsr,   5))
        iran = jz + jsr
    end subroutine random_int_32_scalar

    !> A function to fix random seed.
    !> given an integer seed, generate the remaining seeds needed for call random_seed(put=seeds)
    !> Original implementation taken from https://fortran-lang.discourse.group/t/reproducible-use-of-random-number-with-a-single-integer-seed/3593.
    subroutine fix_random_seed(iseed)
        integer(i64), intent(in)  :: iseed
        integer                   :: nseeds
        integer(i64)              :: i, jseed
        integer, allocatable      :: seeds(:)
        jseed = iseed
        call random_seed(size=nseeds)
        allocate (seeds(nseeds))
        do i=1,nseeds
            call random_int_32_scalar(jseed,seeds(i))
        end do
        call random_seed(put=seeds)
    end subroutine fix_random_seed

    !> A function to release random seed.
    subroutine release_random_seed()
        integer                   :: nseeds
        integer(i64)              :: i, jseed
        integer, allocatable      :: seeds(:)
        jseed = get_datetime()
        call random_seed(size=nseeds)
        allocate (seeds(nseeds))
        do i=1,nseeds
            call random_int_32_scalar(jseed,seeds(i))
        end do
        call random_seed(put=seeds)
    end subroutine release_random_seed
end module mod_random_seed
