function inner_product_r8(vector_1, vector_2, num)
    real(kind=8), intent(in)    :: vector_1(num), vector_2(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: inner_product_r8
    real(kind=8)                :: tmp_sum
    include "./include/linalg_inner_product/inc_inner_product_detail.f90"
    inner_product_r8 = tmp_sum
end function inner_product_r8

function inner_product_i4(vector_1, vector_2, num)
    integer(kind=4), intent(in) :: vector_1(num), vector_2(num)
    integer(kind=4), intent(in) :: num
    integer(kind=4)             :: inner_product_i4
    integer(kind=4)             :: tmp_sum
    include "./include/linalg_inner_product/inc_inner_product_detail.f90"
    inner_product_i4 = tmp_sum
end function inner_product_i4

function inner_product_i8(vector_1, vector_2, num)
    integer(kind=8), intent(in) :: vector_1(num), vector_2(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8)             :: inner_product_i8
    integer(kind=8)             :: tmp_sum
    include "./include/linalg_inner_product/inc_inner_product_detail.f90"
    inner_product_i8 = tmp_sum
end function inner_product_i8
