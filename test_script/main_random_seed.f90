module random_init_mod
    implicit none
    private
    public :: fix_random_seed
contains
    pure elemental subroutine random_int_32_scalar(jsr,iran)
        ! Marsaglia, G. & Tsang, W.W. (2000) `The ziggurat method for generating random variables', J. Statist. Software, v5(8).
        ! generate random 32-bit integers
        integer(kind=8), intent(in out) :: jsr  ! state of RNG
        integer, intent(out)    :: iran ! random integer
        integer                 :: jz
        jz   = jsr
        jsr  = ieor(jsr, ishft(jsr,  13))
        jsr  = ieor(jsr, ishft(jsr, -17))
        jsr  = ieor(jsr, ishft(jsr,   5))
        iran = jz + jsr
    end subroutine random_int_32_scalar
    !
    subroutine fix_random_seed(iseed)
        ! given an integer seed, generate the remaining seeds needed for
        ! call random_seed(put=seeds)
        integer(kind=8), intent(in)  :: iseed
        integer              :: nseeds
        integer(kind=8)              :: i, jseed
        integer, allocatable :: seeds(:)
        logical, parameter   :: print_seeds = .false. ! .false. in production
        jseed = iseed
        call random_seed(size=nseeds)
        allocate (seeds(nseeds))
        do i=1,nseeds
            call random_int_32_scalar(jseed,seeds(i))
        end do
        if (print_seeds) print "('seeds =',*(1x,i0))", seeds
        call random_seed(put=seeds)
    end subroutine fix_random_seed
end module random_init_mod
    
program xfix_random_seed
    use random_init_mod, only: fix_random_seed
    implicit none
    integer, parameter :: dp = kind(1.0d0), nran = 5
    real(kind=dp) :: xran(nran)
    integer :: iseed, nseeds
    integer(kind=8) :: time
    call fix_random_seed(huge(0_8))
    do iseed=1,5
       call random_number(xran)
       print "('uniform deviates =',*(f6.3))", xran
    end do
    call random_number(xran)
    print "('uniform deviates =',*(f6.3))", xran

    call get_datetime(time)
contains

end program xfix_random_seed