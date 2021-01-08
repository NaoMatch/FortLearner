subroutine insertion_sort_r8(vector, num)
    implicit none
    real(kind=8), intent(inout)   :: vector(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                  :: tmp_i, tmp_j
    integer(kind=8)             :: i, j
    include "./include/sort_insertion_sort/inc_insertion_sort_detail.f90"
end subroutine insertion_sort_r8

subroutine insertion_sort_i4(vector, num)
    implicit none
    integer(kind=4), intent(inout)   :: vector(num)
    integer(kind=4), intent(in) :: num
    integer(kind=4)                  :: tmp_i, tmp_j
    integer(kind=4)             :: i, j
    include "./include/sort_insertion_sort/inc_insertion_sort_detail.f90"
end subroutine insertion_sort_i4

subroutine insertion_sort_i8(vector, num)
    implicit none
    integer(kind=8), intent(inout)   :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8)                  :: tmp_i, tmp_j
    integer(kind=8)             :: i, j
    include "./include/sort_insertion_sort/inc_insertion_sort_detail.f90"
end subroutine insertion_sort_i8
