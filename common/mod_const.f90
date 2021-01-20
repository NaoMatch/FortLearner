!> A module that defines useful constants.
module mod_const
    implicit none

    real(kind=8), parameter :: pi_ = 3.14159265358979323846264338327950288d0
    real(kind=8), parameter :: epsilon_ = 10d-8

    real(kind=8), parameter    :: max_real64 =  huge(0d0)
    real(kind=8), parameter    :: min_real64 = -huge(0d0)
    integer(kind=8), parameter :: max_int64  =  huge(0_8)
    integer(kind=8), parameter :: min_int64  = -huge(0_8)

    real(kind=4), parameter    :: max_real32 =  huge(0.0)
    real(kind=4), parameter    :: min_real32 = -huge(0.0)
    integer(kind=4), parameter :: max_int32  =  huge(0)
    integer(kind=4), parameter :: min_int32  = -huge(0)

    logical(kind=4), parameter :: t_ = .true.
    logical(kind=4), parameter :: f_ = .false.

    integer(kind=8), parameter :: buffer_get_minmax=31
    integer(kind=8), parameter :: max_test_iteration=100000000

    integer(kind=8), parameter :: cache_size_multi_mat_vec=7

    integer(kind=8), parameter :: max_rec_depth = 0

    real(kind=8), parameter :: gamma_coefs_(8) = (/& 
        676.5203681218851d0 &
        ,-1259.1392167224028d0 &
        ,771.32342877765313d0 &
        ,-176.61502916214059d0 &
        ,12.507343278686905d0 &
        ,-0.13857109526572012d0 &
        ,9.9843695780195716d-6 &
        ,1.5056327351493116d-7 &
        /)
    integer(kind=8), parameter :: len_gamma_coefs_ = 8_8

end module mod_const
