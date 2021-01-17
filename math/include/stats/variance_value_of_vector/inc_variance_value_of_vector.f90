function variance_value_of_vector_r8(vector, n_samples, mean_of_vector)
    implicit none
    real(kind=8), intent(in)    :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    real(kind=8)                :: variance_value_of_vector_r8
    real(kind=8), optional      :: mean_of_vector
    real(kind=8)                :: mean_of_vector_opt, tmp_sq_sum, tmp
    real(kind=8)                :: buffer(31), val
    integer(kind=8)             :: i, j, n_samples_unroll, len_buffer

    len_buffer = 31
    include "./include/stats/variance_value_of_vector/inc_variance_value_of_vector_detail.f90"  
    variance_value_of_vector_r8 = tmp_sq_sum / real(n_samples, kind=kind(n_samples))
end function variance_value_of_vector_r8

function variance_value_of_vector_i4(vector, n_samples, mean_of_vector)
    implicit none
    integer(kind=4), intent(in) :: vector(n_samples)
    integer(kind=4), intent(in) :: n_samples
    real(kind=4)                :: variance_value_of_vector_i4
    real(kind=4), optional      :: mean_of_vector
    real(kind=4)                :: mean_of_vector_opt, tmp_sq_sum, tmp
    real(kind=4)                :: buffer(63), val
    integer(kind=4)             :: i, j, n_samples_unroll, len_buffer

    len_buffer = 63
    include "./include/stats/variance_value_of_vector/inc_variance_value_of_vector_detail.f90"  
    variance_value_of_vector_i4 = tmp_sq_sum / real(n_samples, kind=kind(n_samples))
end function variance_value_of_vector_i4

function variance_value_of_vector_i8(vector, n_samples, mean_of_vector)
    implicit none
    integer(kind=8), intent(in) :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    real(kind=8)                :: variance_value_of_vector_i8
    real(kind=8), optional      :: mean_of_vector
    real(kind=8)                :: mean_of_vector_opt, tmp_sq_sum, tmp
    real(kind=8)                :: buffer(7), val
    integer(kind=8)             :: i, j, n_samples_unroll, len_buffer

    len_buffer = 7
    include "./include/stats/variance_value_of_vector/inc_variance_value_of_vector_detail.f90"  
    variance_value_of_vector_i8 = tmp_sq_sum / real(n_samples, kind=kind(n_samples))
end function variance_value_of_vector_i8
