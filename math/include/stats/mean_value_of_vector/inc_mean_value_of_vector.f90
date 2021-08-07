function mean_value_of_vector_r8(vector, n_samples)
    implicit none
    real(kind=8), intent(in)      :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    real(kind=8)                :: mean_value_of_vector_r8

    real(kind=8)    :: tmp_sum
    real(kind=8)    :: buffer(15)
    integer(kind=8) :: i, i_unroll, j, buffer_size=15

    tmp_sum = sum_up(vector, n_samples)
    mean_value_of_vector_r8 = tmp_sum / dble(n_samples)
end function mean_value_of_vector_r8

function mean_value_of_vector_i4(vector, n_samples)
    implicit none
    integer(kind=4), intent(in) :: vector(n_samples)
    integer(kind=4), intent(in) :: n_samples
    real(kind=4)                :: mean_value_of_vector_i4
    mean_value_of_vector_i4 = float(sum(vector)) / float(n_samples)
end function mean_value_of_vector_i4

function mean_value_of_vector_i8(vector, n_samples)
    implicit none
    integer(kind=8), intent(in) :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    real(kind=8)                :: mean_value_of_vector_i8
    mean_value_of_vector_i8 = dble(sum(vector)) / dble(n_samples)
end function mean_value_of_vector_i8

