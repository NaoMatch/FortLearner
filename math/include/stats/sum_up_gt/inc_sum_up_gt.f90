! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
function sum_up_gt_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8)             :: n, factor
    real(kind=8)                :: val_x, val_y, tmp_sum, zero=0
    include "./include/stats/sum_up_gt/inc_sum_up_gt_detail.f90"
    sum_up_gt_r8 = tmp_sum
end function sum_up_gt_r8

function sum_up_gt_i4(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=4)             :: sum_up_gt_i4
    integer(kind=4), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=4), intent(in) :: threshold_y
    integer(kind=4), intent(in) :: n_samples
    integer(kind=4)             :: n, factor
    integer(kind=4)             :: val_x, val_y, tmp_sum, zero=0
    include "./include/stats/sum_up_gt/inc_sum_up_gt_detail.f90"
    sum_up_gt_i4 = tmp_sum
end function sum_up_gt_i4

function sum_up_gt_i4_r4_threshold(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=4)             :: sum_up_gt_i4_r4_threshold
    integer(kind=4), intent(in) :: x(n_samples), y(n_samples)
    real(kind=4), intent(in)    :: threshold_y
    integer(kind=4), intent(in) :: n_samples
    integer(kind=4)             :: n, factor
    integer(kind=4)             :: val_x, val_y, tmp_sum, zero=0
    include "./include/stats/sum_up_gt/inc_sum_up_gt_detail.f90"
    sum_up_gt_i4_r4_threshold = tmp_sum
end function sum_up_gt_i4_r4_threshold

function sum_up_gt_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8)             :: n, factor
    integer(kind=8)             :: val_x, val_y, tmp_sum, zero=0
    include "./include/stats/sum_up_gt/inc_sum_up_gt_detail.f90"
    sum_up_gt_i8 = tmp_sum
end function sum_up_gt_i8

function sum_up_gt_i8_r8_threshold(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_i8_r8_threshold
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8)             :: n, factor
    integer(kind=8)             :: val_x, val_y, tmp_sum, zero=0
    include "./include/stats/sum_up_gt/inc_sum_up_gt_detail.f90"
    sum_up_gt_i8_r8_threshold = tmp_sum
end function sum_up_gt_i8_r8_threshold

! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
subroutine count_and_sum_up_gt_loop_if_F_r8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    real(kind=8), intent(inout)    :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    real(kind=8), intent(in)       :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)       :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    real(kind=8)                   :: tmp_sum
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_if_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_if_F_r8

subroutine count_and_sum_up_gt_loop_if_F_i8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    integer(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8)                :: tmp_sum
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_if_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_if_F_i8

subroutine count_and_sum_up_gt_loop_02_if_F_r8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    real(kind=8), intent(inout)    :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    real(kind=8), intent(in)       :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)       :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    real(kind=8)                   :: tmp_sum
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_02_if_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_02_if_F_r8

subroutine count_and_sum_up_gt_loop_02_if_F_i8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    integer(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8)                :: tmp_sum
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_02_if_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_02_if_F_i8

subroutine count_and_sum_up_gt_loop_04_if_F_r8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    real(kind=8), intent(inout)    :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    real(kind=8), intent(in)       :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)       :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    real(kind=8)                   :: tmp_sum
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_04_if_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_04_if_F_r8

subroutine count_and_sum_up_gt_loop_04_if_F_i8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    integer(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8)                :: tmp_sum
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_04_if_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_04_if_F_i8

subroutine count_and_sum_up_gt_loop_08_if_F_r8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    real(kind=8), intent(inout)    :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    real(kind=8), intent(in)       :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)       :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    real(kind=8)                   :: tmp_sum
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_08_if_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_08_if_F_r8

subroutine count_and_sum_up_gt_loop_08_if_F_i8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    integer(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8)                :: tmp_sum
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_08_if_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_08_if_F_i8

subroutine count_and_sum_up_gt_loop_16_if_F_r8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    real(kind=8), intent(inout)    :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    real(kind=8), intent(in)       :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)       :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    real(kind=8)                   :: tmp_sum
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_16_if_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_16_if_F_r8

subroutine count_and_sum_up_gt_loop_16_if_F_i8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    integer(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8)                :: tmp_sum
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_16_if_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_16_if_F_i8



! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
subroutine count_and_sum_up_gt_loop_branchless_F_r8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    real(kind=8), intent(inout)    :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    real(kind=8), intent(in)       :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)       :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    real(kind=8)                   :: tmp_sum
    real(kind=8)                   :: r00, r01, r03
    integer(kind=8)                :: r02, r04
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_branchless_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_branchless_F_r8

subroutine count_and_sum_up_gt_loop_branchless_F_i8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    integer(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8)                :: tmp_sum
    integer(kind=8)                :: r00, r01, r03
    integer(kind=8)                :: r02, r04
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_branchless_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_branchless_F_i8

subroutine count_and_sum_up_gt_loop_branchless_02_F_r8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    real(kind=8), intent(inout)    :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    real(kind=8), intent(in)       :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)       :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    real(kind=8)                   :: tmp_sum
    real(kind=8)                   :: r00, r01, r03
    real(kind=8)                   :: r05, r06, r08
    integer(kind=8)                :: r02, r04, r07, r09
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_branchless_02_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_branchless_02_F_r8

subroutine count_and_sum_up_gt_loop_branchless_02_F_i8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    integer(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8)                :: tmp_sum
    integer(kind=8)                :: r00, r01, r03
    integer(kind=8)                :: r05, r06, r08
    integer(kind=8)                :: r02, r04, r07, r09
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_branchless_02_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_branchless_02_F_i8

subroutine count_and_sum_up_gt_loop_branchless_04_F_r8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    real(kind=8), intent(inout)    :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    real(kind=8), intent(in)       :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)       :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    real(kind=8)                   :: tmp_sum
    real(kind=8)                   :: r00, r01, r03
    real(kind=8)                   :: r05, r06, r08
    real(kind=8)                   :: r10, r11, r13
    real(kind=8)                   :: r14
    integer(kind=8)                :: r02, r04, r07, r09, r12
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_branchless_04_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_branchless_04_F_r8

subroutine count_and_sum_up_gt_loop_branchless_04_F_i8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    integer(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8)                :: tmp_sum
    integer(kind=8)                :: r00, r01, r03
    integer(kind=8)                :: r05, r06, r08
    integer(kind=8)                :: r10, r11, r13
    integer(kind=8)                :: r14
    integer(kind=8)                :: r02, r04, r07, r09, r12
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_branchless_04_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_branchless_04_F_i8

subroutine count_and_sum_up_gt_loop_branchless_08_F_r8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    real(kind=8), intent(inout)    :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    real(kind=8), intent(in)       :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)       :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    real(kind=8)                   :: tmp_sum
    real(kind=8)                   :: r00, r01, r03
    real(kind=8)                   :: r05, r06, r08
    real(kind=8)                   :: r10, r11, r13
    real(kind=8)                   :: r14
    integer(kind=8)                :: r02, r04, r07, r09, r12
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_branchless_08_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_branchless_08_F_r8

subroutine count_and_sum_up_gt_loop_branchless_08_F_i8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    integer(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8)                :: tmp_sum
    integer(kind=8)                :: r00, r01, r03
    integer(kind=8)                :: r05, r06, r08
    integer(kind=8)                :: r10, r11, r13
    integer(kind=8)                :: r14
    integer(kind=8)                :: r02, r04, r07, r09, r12
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_branchless_08_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_branchless_08_F_i8

subroutine count_and_sum_up_gt_loop_branchless_16_F_r8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    real(kind=8), intent(inout)    :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    real(kind=8), intent(in)       :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)       :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    real(kind=8)                   :: tmp_sum
    real(kind=8)                   :: r00, r01, r03
    real(kind=8)                   :: r05, r06, r08
    real(kind=8)                   :: r10, r11, r13
    real(kind=8)                   :: r14
    integer(kind=8)                :: r02, r04, r07, r09, r12
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_branchless_16_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_branchless_16_F_r8

subroutine count_and_sum_up_gt_loop_branchless_16_F_i8(sum_up_gt, count_left, x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: sum_up_gt
    integer(kind=8), intent(inout) :: count_left
    integer(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8)                :: tmp_sum
    integer(kind=8)                :: r00, r01, r03
    integer(kind=8)                :: r05, r06, r08
    integer(kind=8)                :: r10, r11, r13
    integer(kind=8)                :: r14
    integer(kind=8)                :: r02, r04, r07, r09, r12
    integer(kind=8)                :: tmp_cnt
    integer(kind=8)                :: i, unroll_size, n_remain, n_unroll
    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_loop_branchless_16_F.f90"
    sum_up_gt = tmp_sum
    count_left  = tmp_cnt
end subroutine count_and_sum_up_gt_loop_branchless_16_F_i8









































! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
function sum_up_gt_loop_if_F_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_loop_if_F_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    real(kind=8)                :: tmp_sum, tmp_x, tmp_y
    integer(kind=8)             :: i, factor
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_if_F.f90"
    sum_up_gt_loop_if_F_r8 = tmp_sum
end function sum_up_gt_loop_if_F_r8

function sum_up_gt_loop_if_F_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_loop_if_F_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8)             :: tmp_sum, tmp_x, tmp_y
    integer(kind=8)             :: i, factor
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_if_F.f90"
    sum_up_gt_loop_if_F_i8 = tmp_sum
end function sum_up_gt_loop_if_F_i8

function sum_up_gt_loop_if_02_F_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_loop_if_02_F_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    real(kind=8)                :: tmp_sum
    real(kind=8)                :: r00, r01
    integer(kind=8)             :: r12, r13
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_if_02_F.f90"
    sum_up_gt_loop_if_02_F_r8 = tmp_sum
end function sum_up_gt_loop_if_02_F_r8

function sum_up_gt_loop_if_02_F_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_loop_if_02_F_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8)             :: tmp_sum
    integer(kind=8)             :: r00, r01
    integer(kind=8)             :: r12, r13
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_if_02_F.f90"
    sum_up_gt_loop_if_02_F_i8 = tmp_sum
end function sum_up_gt_loop_if_02_F_i8

function sum_up_gt_loop_if_04_F_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_loop_if_04_F_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    real(kind=8)                :: tmp_sum
    real(kind=8)                :: r00, r01, r02, r03
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_if_04_F.f90"
    sum_up_gt_loop_if_04_F_r8 = tmp_sum
end function sum_up_gt_loop_if_04_F_r8

function sum_up_gt_loop_if_04_F_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_loop_if_04_F_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8)             :: tmp_sum
    integer(kind=8)             :: r00, r01, r02, r03
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_if_04_F.f90"
    sum_up_gt_loop_if_04_F_i8 = tmp_sum
end function sum_up_gt_loop_if_04_F_i8

function sum_up_gt_loop_if_08_F_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_loop_if_08_F_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    real(kind=8)                :: tmp_sum
    real(kind=8)                :: r00, r01, r02, r03
    real(kind=8)                :: r04, r05, r06, r07
    integer(kind=8)             :: r08, r09, r10, r11
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_if_08_F.f90"
    sum_up_gt_loop_if_08_F_r8 = tmp_sum
end function sum_up_gt_loop_if_08_F_r8

function sum_up_gt_loop_if_08_F_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_loop_if_08_F_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8)             :: tmp_sum
    integer(kind=8)             :: r00, r01, r02, r03
    integer(kind=8)             :: r04, r05, r06, r07
    integer(kind=8)             :: r08, r09, r10, r11
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_if_08_F.f90"
    sum_up_gt_loop_if_08_F_i8 = tmp_sum
end function sum_up_gt_loop_if_08_F_i8

function sum_up_gt_loop_if_16_F_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_loop_if_16_F_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    real(kind=8)                :: tmp_sum
    real(kind=8)                :: r00, r01, r02, r03
    real(kind=8)                :: r04, r05, r06, r07
    real(kind=8)                :: r08, r09, r10, r11
    real(kind=8)                :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_if_16_F.f90"
    sum_up_gt_loop_if_16_F_r8 = tmp_sum
end function sum_up_gt_loop_if_16_F_r8

function sum_up_gt_loop_if_16_F_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_loop_if_16_F_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8)             :: tmp_sum
    integer(kind=8)             :: r00, r01, r02, r03
    integer(kind=8)             :: r04, r05, r06, r07
    integer(kind=8)             :: r08, r09, r10, r11
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_if_16_F.f90"
    sum_up_gt_loop_if_16_F_i8 = tmp_sum
end function sum_up_gt_loop_if_16_F_i8

! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
! ---------------------------------------------------------------------------------------
function sum_up_gt_loop_branchless_F_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_loop_branchless_F_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    real(kind=8)                :: tmp_sum, tmp_val
    integer(kind=8)             :: i, factor
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_branchless_F.f90"
    sum_up_gt_loop_branchless_F_r8 = tmp_sum
end function sum_up_gt_loop_branchless_F_r8

function sum_up_gt_loop_branchless_F_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_loop_branchless_F_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8)             :: tmp_sum, tmp_val
    integer(kind=8)             :: i, factor
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_branchless_F.f90"
    sum_up_gt_loop_branchless_F_i8 = tmp_sum
end function sum_up_gt_loop_branchless_F_i8

function sum_up_gt_loop_branchless_02_F_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_loop_branchless_02_F_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    real(kind=8)                :: tmp_sum
    real(kind=8)                :: r00, r01
    integer(kind=8)             :: r12, r13
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_branchless_02_F.f90"
    sum_up_gt_loop_branchless_02_F_r8 = tmp_sum
end function sum_up_gt_loop_branchless_02_F_r8

function sum_up_gt_loop_branchless_02_F_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_loop_branchless_02_F_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8)             :: tmp_sum
    integer(kind=8)             :: r00, r01
    integer(kind=8)             :: r12, r13
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_branchless_02_F.f90"
    sum_up_gt_loop_branchless_02_F_i8 = tmp_sum
end function sum_up_gt_loop_branchless_02_F_i8

function sum_up_gt_loop_branchless_04_F_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_loop_branchless_04_F_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    real(kind=8)                :: tmp_sum
    real(kind=8)                :: r00, r01, r02, r03
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_branchless_04_F.f90"
    sum_up_gt_loop_branchless_04_F_r8 = tmp_sum
end function sum_up_gt_loop_branchless_04_F_r8

function sum_up_gt_loop_branchless_04_F_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_loop_branchless_04_F_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8)             :: tmp_sum
    integer(kind=8)             :: r00, r01, r02, r03
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_branchless_04_F.f90"
    sum_up_gt_loop_branchless_04_F_i8 = tmp_sum
end function sum_up_gt_loop_branchless_04_F_i8

function sum_up_gt_loop_branchless_08_F_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_loop_branchless_08_F_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    real(kind=8)                :: tmp_sum
    real(kind=8)                :: r00, r01, r02, r03
    real(kind=8)                :: r04, r05, r06, r07
    integer(kind=8)             :: r08, r09, r10, r11
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_branchless_08_F.f90"
    sum_up_gt_loop_branchless_08_F_r8 = tmp_sum
end function sum_up_gt_loop_branchless_08_F_r8

function sum_up_gt_loop_branchless_08_F_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_loop_branchless_08_F_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8)             :: tmp_sum
    integer(kind=8)             :: r00, r01, r02, r03
    integer(kind=8)             :: r04, r05, r06, r07
    integer(kind=8)             :: r08, r09, r10, r11
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_branchless_08_F.f90"
    sum_up_gt_loop_branchless_08_F_i8 = tmp_sum
end function sum_up_gt_loop_branchless_08_F_i8

function sum_up_gt_loop_branchless_16_F_r8(x, y, threshold_y, n_samples)
    implicit none
    real(kind=8)                :: sum_up_gt_loop_branchless_16_F_r8
    real(kind=8), intent(in)    :: x(n_samples), y(n_samples)
    real(kind=8), intent(in)    :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    real(kind=8)                :: tmp_sum
    real(kind=8)                :: r00, r01, r02, r03
    real(kind=8)                :: r04, r05, r06, r07
    integer(kind=8)             :: r08, r09, r10, r11
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_branchless_16_F.f90"
    sum_up_gt_loop_branchless_16_F_r8 = tmp_sum
end function sum_up_gt_loop_branchless_16_F_r8

function sum_up_gt_loop_branchless_16_F_i8(x, y, threshold_y, n_samples)
    implicit none
    integer(kind=8)             :: sum_up_gt_loop_branchless_16_F_i8
    integer(kind=8), intent(in) :: x(n_samples), y(n_samples)
    integer(kind=8), intent(in) :: threshold_y
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8)             :: tmp_sum
    integer(kind=8)             :: r00, r01, r02, r03
    integer(kind=8)             :: r04, r05, r06, r07
    integer(kind=8)             :: r08, r09, r10, r11
    integer(kind=8)             :: r12, r13, r14, r15
    integer(kind=8)             :: i, factor, unroll_size, n_unroll, n_remain
    include "./include/stats/sum_up_gt/inc_sum_up_gt_loop_branchless_16_F.f90"
    sum_up_gt_loop_branchless_16_F_i8 = tmp_sum
end function sum_up_gt_loop_branchless_16_F_i8