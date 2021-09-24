!> A module for simple functions.
module mod_math
    use mod_const
    implicit none
    
    !> An interface to call gini_i4, and gini_i8
    interface gini
        module procedure :: gini_i4
        module procedure :: gini_i8
    end interface gini

    !> An interface to call entropy_i4, and entropy_i8
    interface entropy
        module procedure :: entropy_i4
        module procedure :: entropy_i8
    end interface entropy

    !> An interface to call log2_r4, log2_r8, log2_i4, and log2_i8
    interface log2
        module procedure :: log2_r4
        module procedure :: log2_r8
        module procedure :: log2_i4
        module procedure :: log2_i8
    end interface log2

    interface gamma
        module procedure gamma_r4
        module procedure gamma_r8
    end interface gamma

    interface sigmoid
        module procedure sigmoid_r4
        module procedure sigmoid_r8
    end interface sigmoid

    interface harmonic_number_approx
        module procedure harmonic_number_approx_i8
    end interface harmonic_number_approx

contains

    function harmonic_number_approx_i8(n)
        real(kind=8) :: harmonic_number_approx_i8
        integer(kind=8), intent(in) :: n
        harmonic_number_approx_i8 = log(n+0d0) + euler_gamma
    end function harmonic_number_approx_i8


    !> A function to compute entropy for decision tree classifier.
    !! \return returns entropy
    !! \param class_counts number of samples per class
    !! \param n_classes number of classes
    function gini_i4(class_counts, n_classes)
        implicit none
        integer(kind=4), intent(in) :: class_counts(n_classes)
        integer(kind=4), intent(in) :: n_classes
        real(kind=4)                :: gini_i4
        integer(kind=4)             :: n_samples_tot, c

        include "./include/math/gini/inc_gini_detail.f90"
        gini_i4 = 1.0 - n_samples_tot / n_samples_tot**2.0
    end function gini_i4
    include "./include/math/gini/inc_gini.f90"


    !> An function to compute gini index for decision tree classifier.
    !! \return returns gini index
    !! \param class_counts number of samples per class
    !! \param n_classes number of classes
    function entropy_i4(class_counts, n_classes)
        implicit none
        integer(kind=4), intent(in) :: class_counts(n_classes)
        integer(kind=4), intent(in) :: n_classes
        real(kind=4)                :: entropy_i4
        integer(kind=4)             :: c, zero_i=0
        real(kind=4)                :: zero_r=0.0, n_samples_tot, tmp_result
        real(kind=4), ALLOCATABLE   :: probas(:)
        include "./include/math/entropy/inc_entropy_detail.f90"
        entropy_i4 = tmp_result
    end function entropy_i4
    include "./include/math/entropy/inc_entropy.f90"


    !> An Elemental function to compute log2.
    !! \param x input value
    elemental function log2_r4(x)
        implicit none
        real(kind=4), intent(in) :: x
        real(kind=4) :: log2_r4
        log2_r4 = log(x)/log(2.0)
    end function log2_r4
    include "./include/math/log2/inc_log2.f90"


    !> A function to compute Gamma Function.
    !! \param x input value
    recursive function gamma_r4(x) result(res)
        real(kind=4) :: x
        real(kind=4) :: res
        real(kind=4) :: one_half = .5, one_r=1.0, two_r=2.0
        real(kind=4) :: dummy_x=0.99999999999980993, t
        integer(kind=4) :: n

        if ( x .lt. one_half ) then
            res = pi_ / sin(pi_ * x) / gamma(one_r - x)
        else
            x = x - one_r
            do n=1, len_gamma_coefs_, 1
                dummy_x = dummy_x + gamma_coefs_(n) / (x + n)
            end do
            t = x + len_gamma_coefs_ - one_half
            res = sqrt(two_r * pi_) * t ** (x+one_half) * exp(-t) * dummy_x
        end if
    end function gamma_r4

    recursive function gamma_r8(x) result(res)
        real(kind=8) :: x
        real(kind=8) :: res
        real(kind=8) :: one_half = .5d0, one_r=1.0, two_r=2.0
        real(kind=8) :: dummy_x=0.99999999999980993d0, t
        integer(kind=8) :: n
        integer(kind=8),save :: iter=0

        if ( x .lt. one_half ) then
            res = pi_ / ( sin(pi_ * x) * gamma(one_r - x) )
            iter = iter + 1
        else
            x = x - one_r
            do n=1, len_gamma_coefs_, 1
                dummy_x = dummy_x + gamma_coefs_(n) / (x + n)
            end do
            t = x + len_gamma_coefs_ - one_half
            res = sqrt(two_r * pi_) * t ** (x+one_half) * exp(-t) * dummy_x
        end if
    end function gamma_r8


    !> A function to compute sigmoid
    !! \param x input value
    elemental function sigmoid_r4(x)
        real(kind=4), intent(in) :: x
        real(kind=4)             :: sigmoid_r4
        sigmoid_r4 = 1.0 / (1.0 + exp(-x))
    end function sigmoid_r4
    include "./include/math/sigmoid/inc_sigmoid.f90"

end module mod_math
