! subroutine rand_integer_int64(lo, hi, vector, num)
!     implicit none
!     integer(kind=8), intent(in)    :: lo, hi
!     integer(kind=8), intent(inout) :: vector(num)
!     integer(kind=8), intent(in)    :: num
    
!     real(kind=8), allocatable      :: tmp_array(:)
!     integer(kind=8)                :: i, j, factor, min, tmp
!     integer(kind=8)                :: unroll, buffer(15)

!     factor = hi - lo + 1
!     allocate(tmp_array(num))
!     call random_number(tmp_array)
!     unroll = num - mod(num, 15)
!     do i=1, unroll, 15
!         do j=0, 15-1, 1
!             buffer(j+1) = int(factor * tmp_array(i+j), kind=8) + lo
!         end do

!         do j=0, 15-1, 1
!             vector(i+j) = buffer(j+1)
!         end do
!     end do
!     do i=unroll+1, num
!         vector(i) = int(factor * tmp_array(i), kind=4) + lo
!     end do
!     deallocate(tmp_array)
! end subroutine rand_integer_int64
subroutine rand_integer_int64(lo, hi, vector, num)
    implicit none
    integer(kind=8), intent(in)    :: lo, hi
    integer(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in)    :: num
    
    real(kind=8), allocatable      :: tmp_array(:)
    integer(kind=8)                :: i, j, factor, min, tmp
    integer(kind=8)                :: unroll, buffer(15)

    factor = hi - lo + 1
    allocate(tmp_array(num))
    call random_number(tmp_array)
    tmp_array = (hi-lo) * tmp_array + lo
    vector = int(tmp_array, kind=8)    
    deallocate(tmp_array)
end subroutine rand_integer_int64