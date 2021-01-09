function inner_product_real64(vector_1, vector_2, num)
    real(kind=8), intent(in)    :: vector_1(num), vector_2(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: inner_product_real64
    real(kind=8)                :: tmp_sum
    include "./include/linalg_inner_product/inc_inner_product_detail.f90"
    inner_product_real64 = tmp_sum
end function inner_product_real64

function inner_product_int32(vector_1, vector_2, num)
    integer(kind=4), intent(in) :: vector_1(num), vector_2(num)
    integer(kind=4), intent(in) :: num
    integer(kind=4)             :: inner_product_int32
    integer(kind=4)             :: tmp_sum
    include "./include/linalg_inner_product/inc_inner_product_detail.f90"
    inner_product_int32 = tmp_sum
end function inner_product_int32

function inner_product_int64(vector_1, vector_2, num)
    integer(kind=8), intent(in) :: vector_1(num), vector_2(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8)             :: inner_product_int64
    integer(kind=8)             :: tmp_sum
    include "./include/linalg_inner_product/inc_inner_product_detail.f90"
    inner_product_int64 = tmp_sum
end function inner_product_int64
