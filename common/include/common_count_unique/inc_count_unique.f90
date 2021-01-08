function count_unique_real64(vector, num)
    implicit none
    real(kind=8), intent(in)    :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8)             :: count_unique_real64

    integer(kind=8) :: i, factor
    integer(kind=8) :: tmp_count
    if (num .eq. 1_8) then
        count_unique_real64 = 0
        return
    end if

    tmp_count = 1
    include "./include/common_count_unique/inc_count_unique_detail.f90"
    count_unique_real64 = tmp_count
end function count_unique_real64

function count_unique_int32(vector, num)
    implicit none
    integer(kind=4), intent(in)    :: vector(num)
    integer(kind=4), intent(in) :: num
    integer(kind=4)             :: count_unique_int32

    integer(kind=4) :: i, factor
    integer(kind=4) :: tmp_count
    if (num .eq. 1) then
        count_unique_int32 = 0
        return
    end if

    tmp_count = 1
    include "./include/common_count_unique/inc_count_unique_detail.f90"
    count_unique_int32 = tmp_count
end function count_unique_int32

function count_unique_int64(vector, num)
    implicit none
    integer(kind=8), intent(in)    :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8)             :: count_unique_int64

    integer(kind=8) :: i, factor
    integer(kind=8) :: tmp_count
    if (num .eq. 1_8) then
        count_unique_int64 = 0
        return
    end if

    tmp_count = 1
    include "./include/common_count_unique/inc_count_unique_detail.f90"
    count_unique_int64 = tmp_count
end function count_unique_int64
