function count_unique_r8(vector, n_samples)
    implicit none
    real(kind=8), intent(in)    :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8)             :: count_unique_r8

    integer(kind=8) :: i, factor, tmp_count
    include "./include/common/count_unique/inc_count_unique_detail.f90"
    count_unique_r8 = tmp_count
end function count_unique_r8

function count_unique_i4(vector, n_samples)
    implicit none
    integer(kind=4), intent(in) :: vector(n_samples)
    integer(kind=4), intent(in) :: n_samples
    integer(kind=4)             :: count_unique_i4

    integer(kind=4) :: i, factor, tmp_count
    tmp_count = 1
    include "./include/common/count_unique/inc_count_unique_detail.f90"
    count_unique_i4 = tmp_count
end function count_unique_i4

function count_unique_i8(vector, n_samples)
    implicit none
    integer(kind=8), intent(in) :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8)             :: count_unique_i8

    integer(kind=8) :: i, factor, tmp_count
    tmp_count = 1
    include "./include/common/count_unique/inc_count_unique_detail.f90"
    count_unique_i8 = tmp_count
end function count_unique_i8
