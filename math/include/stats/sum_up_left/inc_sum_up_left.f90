function sum_up_left_r8(vector, threshold, n_samples)
    implicit none
    real(kind=8)                :: sum_up_left_r8
    real(kind=8), intent(in)    :: vector(n_samples)
    real(kind=8), intent(in)    :: threshold
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8)             :: n, factor
    real(kind=8)                :: val, tmp_sum, zero=0
    include "./include/stats/sum_up_left/inc_sum_up_left_detail.f90"
    sum_up_left_r8 = tmp_sum
end function sum_up_left_r8

function sum_up_left_i4(vector, threshold, n_samples)
    implicit none
    integer(kind=4)             :: sum_up_left_i4
    integer(kind=4), intent(in) :: vector(n_samples)
    integer(kind=4), intent(in) :: threshold
    integer(kind=4), intent(in) :: n_samples
    integer(kind=4)             :: n, factor
    integer(kind=4)             :: val, tmp_sum, zero=0
    include "./include/stats/sum_up_left/inc_sum_up_left_detail.f90"
    sum_up_left_i4 = tmp_sum
end function sum_up_left_i4

function sum_up_left_i4_r4_threshold(vector, threshold, n_samples)
    implicit none
    integer(kind=4)             :: sum_up_left_i4_r4_threshold
    integer(kind=4), intent(in) :: vector(n_samples)
    real(kind=4), intent(in)    :: threshold
    integer(kind=4), intent(in) :: n_samples
    integer(kind=4)             :: n, factor
    integer(kind=4)             :: val, tmp_sum, zero=0
    include "./include/stats/sum_up_left/inc_sum_up_left_detail.f90"
    sum_up_left_i4_r4_threshold = tmp_sum
end function sum_up_left_i4_r4_threshold

function sum_up_left_i8(vector, threshold, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_left_i8
    integer(kind=8), intent(in) :: vector(n_samples)
    integer(kind=8), intent(in) :: threshold
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8)             :: n, factor
    integer(kind=8)             :: val, tmp_sum, zero=0
    include "./include/stats/sum_up_left/inc_sum_up_left_detail.f90"
    sum_up_left_i8 = tmp_sum
end function sum_up_left_i8

function sum_up_left_i8_r8_threshold(vector, threshold, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_left_i8_r8_threshold
    integer(kind=8), intent(in) :: vector(n_samples)
    real(kind=8), intent(in)    :: threshold
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8)             :: n, factor
    integer(kind=8)             :: val, tmp_sum, zero=0
    include "./include/stats/sum_up_left/inc_sum_up_left_detail.f90"
    sum_up_left_i8_r8_threshold = tmp_sum
end function sum_up_left_i8_r8_threshold
