function covariance_value_of_vectors_r8(vector1, vector2, num, mean1, mean2)
    implicit none
    real(kind=8), intent(in)      :: vector1(num), vector2(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: covariance_value_of_vectors_r8
    real(kind=8), optional      :: mean1, mean2
    real(kind=8)                :: mean1_opt, mean2_opt, tmp_sum, buffer1(7), buffer2(7)
    integer(kind=8)             :: i, j, unroll

    tmp_sum = 0
    if ( present(mean1) ) then
        mean1_opt = mean1
    else
        mean1 = mean(vector1, num)
    end if
    if ( present(mean2) ) then
        mean2_opt = mean2
    else
        mean2 = mean(vector2, num)
    end if

    unroll = num - mod(num, 7)
    do i=1, unroll, 7
        do j=0, 7-1, 1
            buffer1(j+1) = vector1(i+j) - mean1_opt
            buffer2(j+1) = vector2(i+j) - mean2_opt
        end do

        do j=0, 7-1, 1
            tmp_sum = tmp_sum + buffer1(j+1) * buffer2(j+1)
        end do
    end do

    do i=unroll+1, num, 1
        tmp_sum = tmp_sum + (vector1(i) - mean1_opt) * (vector2(i) - mean2_opt)
    end do
    covariance_value_of_vectors_r8 = tmp_sum / dble(num-1)
end function covariance_value_of_vectors_r8

function covariance_value_of_vectors_i4(vector1, vector2, num, mean1, mean2)
    implicit none
    integer(kind=4), intent(in)      :: vector1(num), vector2(num)
    integer(kind=4), intent(in) :: num
    real(kind=4)                :: covariance_value_of_vectors_i4
    real(kind=4), optional      :: mean1, mean2
    real(kind=4)                :: mean1_opt, mean2_opt, tmp_sum, buffer1(7), buffer2(7)
    integer(kind=4)             :: i, j, unroll

    tmp_sum = 0
    if ( present(mean1) ) then
        mean1_opt = mean1
    else
        mean1 = mean(vector1, num)
    end if
    if ( present(mean2) ) then
        mean2_opt = mean2
    else
        mean2 = mean(vector2, num)
    end if

    unroll = num - mod(num, 31)
    do i=1, unroll, 31
        do j=0, 31-1, 1
            buffer1(j+1) = real(vector1(i+j), kind=4) - mean1_opt
            buffer2(j+1) = real(vector2(i+j), kind=4) - mean2_opt
        end do

        do j=0, 31-1, 1
            tmp_sum = tmp_sum + buffer1(j+1) * buffer2(j+1)
        end do
    end do

    do i=unroll+1, num, 1
        tmp_sum = tmp_sum + (vector1(i) - mean1_opt) * (vector2(i) - mean2_opt)
    end do
    covariance_value_of_vectors_i4 = tmp_sum / float(num-1)
end function covariance_value_of_vectors_i4

function covariance_value_of_vectors_i8(vector1, vector2, num, mean1, mean2)
    implicit none
    integer(kind=8), intent(in)      :: vector1(num), vector2(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: covariance_value_of_vectors_i8
    real(kind=8), optional      :: mean1, mean2
    real(kind=8)                :: mean1_opt, mean2_opt, tmp_sum, buffer1(7), buffer2(7)
    integer(kind=8)             :: i, j, unroll

    tmp_sum = 0
    if ( present(mean1) ) then
        mean1_opt = mean1
    else
        mean1 = mean(vector1, num)
    end if
    if ( present(mean2) ) then
        mean2_opt = mean2
    else
        mean2 = mean(vector2, num)
    end if

    unroll = num - mod(num, 15)
    do i=1, unroll, 15
        do j=0, 15-1, 1
            buffer1(j+1) = real(vector1(i+j), kind=8) - mean1_opt
            buffer2(j+1) = real(vector2(i+j), kind=8) - mean2_opt
        end do

        do j=0, 15-1, 1
            tmp_sum = tmp_sum + buffer1(j+1) * buffer2(j+1)
        end do
    end do

    do i=unroll+1, num, 1
        tmp_sum = tmp_sum & 
                + (real(vector1(i), kind=8) - mean1_opt) & 
                * (real(vector2(i), kind=8) - mean2_opt)
    end do
    covariance_value_of_vectors_i8 = tmp_sum / dble(num-1)
end function covariance_value_of_vectors_i8


