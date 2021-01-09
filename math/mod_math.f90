!> A module for simple functions.
module mod_math

    implicit none
    
    !> An interface to call log2_real32, log2_real64, log2_int32, and log2_int64
    interface log2
        module procedure :: log2_real32
        module procedure :: log2_real64
        module procedure :: log2_int32
        module procedure :: log2_int64
    end interface log2

    !> An interface to call gini_int32, and gini_int64
    interface gini
        module procedure :: gini_int32
        module procedure :: gini_int64
    end interface gini

contains
    !> An Elemental function to compute log2.
    !! \param x input value
    elemental function log2_real32(x)
        implicit none
        real(kind=4), intent(in) :: x
        real(kind=4) :: log2_real32
        log2_real32 = log(x)/log(2.0)
    end function log2_real32
    include "./include/math_log2/inc_log2.f90"

    !> An function to compute gini index for decision tree classifier.
    !! \return returns gini index
    !! \param class_counts number of samples per class
    !! \param n_classes number of classes
    function gini_int32(class_counts, n_classes)
        implicit none
        integer(kind=4), intent(in) :: class_counts(n_classes)
        integer(kind=4), intent(in) :: n_classes
        real(kind=4)                :: gini_int32
        integer(kind=4)             :: n_samples_tot, c

        include "./include/gini/inc_gini_detail.f90"
        gini_int32 = 1.0 - n_samples_tot / n_samples_tot**2.0
    end function gini_int32
    include "./include/gini/inc_gini.f90"


end module mod_math
