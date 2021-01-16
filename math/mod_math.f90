!> A module for simple functions.
module mod_math

    implicit none
    
    !> An interface to call gini_i4, and gini_i8
    interface gini
        module procedure :: gini_i4
        module procedure :: gini_i8
    end interface gini

    !> An interface to call log2_r4, log2_r8, log2_i4, and log2_i8
    interface log2
        module procedure :: log2_r4
        module procedure :: log2_r8
        module procedure :: log2_i4
        module procedure :: log2_i8
    end interface log2

contains
    !> An Elemental function to compute log2.
    !! \param x input value
    elemental function log2_r4(x)
        implicit none
        real(kind=4), intent(in) :: x
        real(kind=4) :: log2_r4
        log2_r4 = log(x)/log(2.0)
    end function log2_r4
    include "./include/math/log2/inc_log2.f90"

    !> An function to compute gini index for decision tree classifier.
    !! \return returns gini index
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


end module mod_math
