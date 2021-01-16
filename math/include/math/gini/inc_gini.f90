function gini_i8(class_counts, n_classes)
    implicit none
    integer(kind=8), intent(in) :: class_counts(n_classes)
    integer(kind=8), intent(in) :: n_classes
    real(kind=8)                :: gini_i8
    integer(kind=8)             :: n_samples_tot, c

    include "./include/math/gini/inc_gini_detail.f90"
    gini_i8 = 1d0 - n_samples_tot / n_samples_tot**2d0
end function gini_i8
