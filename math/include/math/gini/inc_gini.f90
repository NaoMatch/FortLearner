function gini_i8(class_counts, n_classes)
    implicit none
    integer(kind=8), intent(in) :: class_counts(n_classes)
    integer(kind=8), intent(in) :: n_classes
    real(kind=8)                :: gini_i8
    integer(kind=8)             :: n_samples_tot, c, tot_num
    real(kind=8)                :: tmp_gini

    ! include "./include/math/gini/inc_gini_detail.f90"
    ! gini_i8 = 1d0 - n_samples_tot / n_samples_tot**2d0
    tot_num = sum(class_counts(:))
    tmp_gini = 0d0
    do c=1, n_classes, 1
        if (class_counts(c) .ne. 0_8) then
            tmp_gini = tmp_gini + ( class_counts(c) / dble(tot_num) )**2d0
        end if
    end do
    gini_i8 = 1d0 - tmp_gini
end function gini_i8
