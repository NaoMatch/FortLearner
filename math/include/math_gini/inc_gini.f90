function gini_int64(class_counts, n_classes)
    implicit none
    integer(kind=8), intent(in) :: class_counts(n_classes)
    integer(kind=8), intent(in) :: n_classes
    real(kind=8)                :: gini_int64
    integer(kind=8)             :: n_samples_tot, c

    include "./include/gini/inc_gini_detail.f90"
    gini_int64 = 1d0 - n_samples_tot / n_samples_tot**2d0
end function gini_int64
