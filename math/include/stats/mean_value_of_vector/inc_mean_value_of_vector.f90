function mean_value_of_vector_real64(vector, num)
    implicit none
    real(kind=8), intent(in)      :: vector(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: mean_value_of_vector_real64

    real(kind=8)    :: tmp_sum
    real(kind=8)    :: buffer(15)
    integer(kind=8) :: i, i_unroll, j
    
    tmp_sum = 0d0
    i_unroll = num - mod(num, 15)
    do i=1, i_unroll, 15
        do j=0, 15-1, 1
            buffer(j+1) = vector(i+j)
        end do

        do j=0, 15-1, 1
            tmp_sum = tmp_sum + buffer(j+1)
        end do
    end do

    do i=i_unroll+1, num, 1
        tmp_sum = tmp_sum + vector(i)
    end do
    mean_value_of_vector_real64 = tmp_sum / dble(num)
end function mean_value_of_vector_real64

function mean_value_of_vector_int32(vector, num)
    implicit none
    integer(kind=4), intent(in) :: vector(num)
    integer(kind=4), intent(in) :: num
    real(kind=4)                :: mean_value_of_vector_int32
    mean_value_of_vector_int32 = float(sum(vector)) / float(num)
end function mean_value_of_vector_int32

function mean_value_of_vector_int64(vector, num)
    implicit none
    integer(kind=8), intent(in) :: vector(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: mean_value_of_vector_int64
    mean_value_of_vector_int64 = dble(sum(vector)) / dble(num)
end function mean_value_of_vector_int64

