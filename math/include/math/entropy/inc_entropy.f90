function entropy_i8(class_counts, n_classes)
    implicit none
    integer(kind=8), intent(in) :: class_counts(n_classes)
    integer(kind=8), intent(in) :: n_classes
    real(kind=8)                :: entropy_i8
    integer(kind=8)             :: c, zero_i=0
    real(kind=8)                :: zero_r=0.0, n_samples_tot, tmp_result
    real(kind=8), ALLOCATABLE   :: probas(:)
    include "./include/math/entropy/inc_entropy_detail.f90"
    entropy_i8 = tmp_result
end function entropy_i8
