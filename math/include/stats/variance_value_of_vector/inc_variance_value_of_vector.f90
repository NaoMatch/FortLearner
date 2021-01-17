function variance_value_of_vector_r8(vector, num, mean_of_vector)
    implicit none
    real(kind=8), intent(in)      :: vector(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: variance_value_of_vector_r8
    real(kind=8), optional      :: mean_of_vector
    real(kind=8)                :: mean_of_vector_opt, tmp_sq_sum, tmp
    real(kind=8)                :: buffer(31)
    integer(kind=8)             :: i, j, unroll, len_buffer

    len_buffer = 31

    if ( present(mean_of_vector) ) then
        mean_of_vector_opt = mean_of_vector
    else
        mean_of_vector_opt = mean(vector, num)
    end if
    tmp_sq_sum = 0
    unroll = num - mod(num, len_buffer)

    do i=1, unroll, len_buffer
        do j=0, len_buffer-1, 1
            buffer(j+1) = vector(i+j)-mean_of_vector_opt
        end do
        
        do j=0, len_buffer-1, 1
            tmp_sq_sum = tmp_sq_sum + buffer(j+1) ** 2
        end do
    end do

    do i=unroll+1, num
        tmp_sq_sum = tmp_sq_sum + vector(i) ** 2
    end do
    variance_value_of_vector_r8 = tmp_sq_sum / dble(num)
end function variance_value_of_vector_r8

function variance_value_of_vector_i4(vector, num, mean_of_vector)
    implicit none
    integer(kind=4), intent(in)      :: vector(num)
    integer(kind=4), intent(in) :: num
    real(kind=4)                :: variance_value_of_vector_i4
    real(kind=4), optional      :: mean_of_vector
    real(kind=4)                :: mean_of_vector_opt, tmp_sq_sum, tmp
    real(kind=4)                :: buffer(63)
    integer(kind=4)             :: i, j, unroll, len_buffer

    len_buffer = 63

    if ( present(mean_of_vector) ) then
        mean_of_vector_opt = mean_of_vector
    else
        mean_of_vector_opt = mean(vector, num)
    end if
    tmp_sq_sum = 0
    unroll = num - mod(num, len_buffer)

    do i=1, unroll, len_buffer
        do j=0, len_buffer-1, 1
            buffer(j+1) = float(vector(i+j))
        end do
        
        do j=0, len_buffer-1, 1
            tmp_sq_sum = tmp_sq_sum + buffer(j+1) ** 2
        end do
    end do

    do i=unroll+1, num
        tmp_sq_sum = tmp_sq_sum + (float(vector(i)) - mean_of_vector_opt) ** 2
    end do
    variance_value_of_vector_i4 = tmp_sq_sum / float(num-1)
end function variance_value_of_vector_i4

function variance_value_of_vector_i8(vector, num, mean_of_vector)
    implicit none
    integer(kind=8), intent(in)      :: vector(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: variance_value_of_vector_i8
    real(kind=8), optional      :: mean_of_vector
    real(kind=8)                :: mean_of_vector_opt, tmp_sq_sum, tmp
    real(kind=8)                :: buffer(7)
    integer(kind=8)             :: i, j, unroll, len_buffer

    len_buffer = 7

    if ( present(mean_of_vector) ) then
        mean_of_vector_opt = mean_of_vector
    else
        mean_of_vector_opt = mean(vector, num)
    end if
    tmp_sq_sum = 0
    unroll = num - mod(num, len_buffer)

    do i=1, unroll, len_buffer
        do j=0, len_buffer-1, 1
            buffer(j+1) = dble(vector(i+j)) - mean_of_vector_opt
        end do
        
        do j=0, len_buffer-1, 1
            tmp_sq_sum = tmp_sq_sum + buffer(j+1) ** 2
        end do
    end do

    do i=unroll+1, num
        tmp_sq_sum = tmp_sq_sum + (dble(vector(i)) - mean_of_vector_opt) ** 2
    end do
    variance_value_of_vector_i8 = tmp_sq_sum / dble(num-1)
end function variance_value_of_vector_i8
