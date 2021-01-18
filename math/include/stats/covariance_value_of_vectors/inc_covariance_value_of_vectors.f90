function covariance_value_of_vectors_r8(vector1, vector2, num, mean1, mean2)
    implicit none
    real(kind=8), intent(in)      :: vector1(num), vector2(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: covariance_value_of_vectors_r8
    real(kind=8), optional      :: mean1, mean2
    real(kind=8)                :: mean1_opt, mean2_opt, tmp_sum
    integer(kind=8)             :: i, j
    include "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors_detail.f90"
    covariance_value_of_vectors_r8 = tmp_sum / real(num, kind=kind(tmp_sum))
end function covariance_value_of_vectors_r8

function covariance_value_of_vectors_i4(vector1, vector2, num, mean1, mean2)
    implicit none
    integer(kind=4), intent(in)      :: vector1(num), vector2(num)
    integer(kind=4), intent(in) :: num
    real(kind=4)                :: covariance_value_of_vectors_i4
    real(kind=4), optional      :: mean1, mean2
    real(kind=4)                :: mean1_opt, mean2_opt, tmp_sum
    integer(kind=4)             :: i, j
    include "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors_detail.f90"
    covariance_value_of_vectors_i4 = tmp_sum / real(num, kind=kind(tmp_sum))
end function covariance_value_of_vectors_i4

function covariance_value_of_vectors_i8(vector1, vector2, num, mean1, mean2)
    implicit none
    integer(kind=8), intent(in)      :: vector1(num), vector2(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: covariance_value_of_vectors_i8
    real(kind=8), optional      :: mean1, mean2
    real(kind=8)                :: mean1_opt, mean2_opt, tmp_sum
    integer(kind=8)             :: i, j
    include "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors_detail.f90"
    covariance_value_of_vectors_i8 = tmp_sum / real(num, kind=kind(tmp_sum))
end function covariance_value_of_vectors_i8
