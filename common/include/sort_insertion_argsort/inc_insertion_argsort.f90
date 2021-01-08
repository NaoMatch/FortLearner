subroutine insertion_argsort_r4_r4(vector1, vector2, num)
    implicit none
    real(kind=4), intent(inout) :: vector1(num)
    real(kind=4), intent(inout) :: vector2(num)
    integer(kind=4), intent(in) :: num
    real(kind=4)                :: tmp1_i, tmp1_j
    real(kind=4)                :: tmp2_i, tmp2_j
    integer(kind=4)             :: i, j
    include "./include/sort_insertion_argsort/inc_insertion_argsort_detail.f90"
end subroutine insertion_argsort_r4_r4

subroutine insertion_argsort_r8_i8(vector1, vector2, num)
    implicit none
    real(kind=8), intent(inout)    :: vector1(num)
    integer(kind=8), intent(inout) :: vector2(num)
    integer(kind=8), intent(in)    :: num
    real(kind=8)                   :: tmp1_i, tmp1_j
    integer(kind=8)                :: tmp2_i, tmp2_j
    integer(kind=8)                :: i, j
    include "./include/sort_insertion_argsort/inc_insertion_argsort_detail.f90"
end subroutine insertion_argsort_r8_i8

subroutine insertion_argsort_r8_r8(vector1, vector2, num)
    implicit none
    real(kind=8), intent(inout) :: vector1(num)
    real(kind=8), intent(inout) :: vector2(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: tmp1_i, tmp1_j
    real(kind=8)                :: tmp2_i, tmp2_j
    integer(kind=8)             :: i, j
    include "./include/sort_insertion_argsort/inc_insertion_argsort_detail.f90"
end subroutine insertion_argsort_r8_r8

subroutine insertion_argsort_i4_i4(vector1, vector2, num)
    implicit none
    integer(kind=4), intent(inout) :: vector1(num)
    integer(kind=4), intent(inout) :: vector2(num)
    integer(kind=4), intent(in)    :: num
    integer(kind=4)                :: tmp1_i, tmp1_j
    integer(kind=4)                :: tmp2_i, tmp2_j
    integer(kind=4)                :: i, j
    include "./include/sort_insertion_argsort/inc_insertion_argsort_detail.f90"
end subroutine insertion_argsort_i4_i4

subroutine insertion_argsort_i4_r4(vector1, vector2, num)
    implicit none
    integer(kind=4), intent(inout) :: vector1(num)
    real(kind=4), intent(inout)    :: vector2(num)
    integer(kind=4), intent(in)    :: num
    integer(kind=4)                :: tmp1_i, tmp1_j
    real(kind=4)                   :: tmp2_i, tmp2_j
    integer(kind=4)                :: i, j
    include "./include/sort_insertion_argsort/inc_insertion_argsort_detail.f90"
end subroutine insertion_argsort_i4_r4

subroutine insertion_argsort_i8_i8(vector1, vector2, num)
    implicit none
    integer(kind=8), intent(inout) :: vector1(num)
    integer(kind=8), intent(inout) :: vector2(num)
    integer(kind=8), intent(in)    :: num
    integer(kind=8)                :: tmp1_i, tmp1_j
    integer(kind=8)                :: tmp2_i, tmp2_j
    integer(kind=8)                :: i, j
    include "./include/sort_insertion_argsort/inc_insertion_argsort_detail.f90"
end subroutine insertion_argsort_i8_i8

subroutine insertion_argsort_i8_r8(vector1, vector2, num)
    implicit none
    integer(kind=8), intent(inout) :: vector1(num)
    real(kind=8), intent(inout)    :: vector2(num)
    integer(kind=8), intent(in)    :: num
    integer(kind=8)                :: tmp1_i, tmp1_j
    real(kind=8)                   :: tmp2_i, tmp2_j
    integer(kind=8)                :: i, j
    include "./include/sort_insertion_argsort/inc_insertion_argsort_detail.f90"
end subroutine insertion_argsort_i8_r8

