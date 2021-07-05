subroutine get_minmax_r8(min_val, max_val, vector, n_samples)
    implicit none
    real(kind=8), intent(out)   :: min_val
    real(kind=8), intent(out)   :: max_val
    real(kind=8), intent(in)    :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    real(kind=8)    :: tmp_val, tmp_min_val, tmp_max_val
    real(kind=8)    :: buffer(buffer_get_minmax)
    integer(kind=8) :: n, n_samples_unroll, j
    include "./include/stats/get_minmax/inc_get_minmax_detail.f90"
end subroutine get_minmax_r8

subroutine get_minmax_i4(min_val, max_val, vector, n_samples)
    implicit none
    integer(kind=4), intent(out)   :: min_val
    integer(kind=4), intent(out)   :: max_val
    integer(kind=4), intent(in)    :: vector(n_samples)
    integer(kind=4), intent(in) :: n_samples
    integer(kind=4)    :: tmp_val, tmp_min_val, tmp_max_val
    integer(kind=4)    :: buffer(buffer_get_minmax)
    integer(kind=4) :: n, n_samples_unroll, j
    include "./include/stats/get_minmax/inc_get_minmax_detail.f90"
end subroutine get_minmax_i4

subroutine get_minmax_i8(min_val, max_val, vector, n_samples)
    implicit none
    integer(kind=8), intent(out)   :: min_val
    integer(kind=8), intent(out)   :: max_val
    integer(kind=8), intent(in)    :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8)    :: tmp_val, tmp_min_val, tmp_max_val
    integer(kind=8)    :: buffer(buffer_get_minmax)
    integer(kind=8) :: n, n_samples_unroll, j
    include "./include/stats/get_minmax/inc_get_minmax_detail.f90"
end subroutine get_minmax_i8

subroutine get_minmax_loop_F_r8(min, max, vec, n)
    implicit none
    real(kind=8), intent(inout) :: min, max
    real(kind=8), intent(in)    :: vec(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8) :: i
    real(kind=8)    :: r00
    include "./include/stats/get_minmax/inc_get_minmax_loop.f90"
end subroutine get_minmax_loop_F_r8

subroutine get_minmax_loop_F_i8(min, max, vec, n)
    implicit none
    integer(kind=8), intent(inout) :: min, max
    integer(kind=8), intent(in)    :: vec(n)
    integer(kind=8), intent(in)    :: n

    integer(kind=8) :: i
    integer(kind=8) :: r00
    include "./include/stats/get_minmax/inc_get_minmax_loop.f90"
end subroutine get_minmax_loop_F_i8

subroutine get_minmax_02_F_r8(min, max, vec, n)
    implicit none
    real(kind=8), intent(inout) :: min, max
    real(kind=8), intent(in)    :: vec(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8) :: i, n_unroll, n_remain, unroll_size
    real(kind=8)    :: r00, r01
    include "./include/stats/get_minmax/inc_get_minmax_02.f90"
end subroutine get_minmax_02_F_r8

subroutine get_minmax_02_F_i8(min, max, vec, n)
    implicit none
    integer(kind=8), intent(inout) :: min, max
    integer(kind=8), intent(in)    :: vec(n)
    integer(kind=8), intent(in)    :: n

    integer(kind=8) :: i, n_unroll, n_remain, unroll_size
    integer(kind=8) :: r00, r01
    include "./include/stats/get_minmax/inc_get_minmax_02.f90"
end subroutine get_minmax_02_F_i8

subroutine get_minmax_04_F_r8(min, max, vec, n)
    implicit none
    real(kind=8), intent(inout) :: min, max
    real(kind=8), intent(in)    :: vec(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8) :: i, n_unroll, n_remain, unroll_size
    real(kind=8)    :: r00, r01, r02, r03
    include "./include/stats/get_minmax/inc_get_minmax_04.f90"
end subroutine get_minmax_04_F_r8

subroutine get_minmax_04_F_i8(min, max, vec, n)
    implicit none
    integer(kind=8), intent(inout) :: min, max
    integer(kind=8), intent(in)    :: vec(n)
    integer(kind=8), intent(in)    :: n

    integer(kind=8) :: i, n_unroll, n_remain, unroll_size
    integer(kind=8) :: r00, r01, r02, r03
    include "./include/stats/get_minmax/inc_get_minmax_04.f90"
end subroutine get_minmax_04_F_i8

subroutine get_minmax_08_F_r8(min, max, vec, n)
    implicit none
    real(kind=8), intent(inout) :: min, max
    real(kind=8), intent(in)    :: vec(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8) :: i, n_unroll, n_remain, unroll_size
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    include "./include/stats/get_minmax/inc_get_minmax_08.f90"
end subroutine get_minmax_08_F_r8

subroutine get_minmax_08_F_i8(min, max, vec, n)
    implicit none
    integer(kind=8), intent(inout) :: min, max
    integer(kind=8), intent(in)    :: vec(n)
    integer(kind=8), intent(in)    :: n

    integer(kind=8) :: i, n_unroll, n_remain, unroll_size
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    include "./include/stats/get_minmax/inc_get_minmax_08.f90"
end subroutine get_minmax_08_F_i8

subroutine get_minmax_16_F_r8(min, max, vec, n)
    implicit none
    real(kind=8), intent(inout) :: min, max
    real(kind=8), intent(in)    :: vec(n)
    integer(kind=8), intent(in) :: n

    integer(kind=8) :: i, n_unroll, n_remain, unroll_size
    real(kind=8)    :: r00, r01, r02, r03
    real(kind=8)    :: r04, r05, r06, r07
    real(kind=8)    :: r08, r09, r10, r11
    real(kind=8)    :: r12, r13, r14, r15
    include "./include/stats/get_minmax/inc_get_minmax_16.f90"
end subroutine get_minmax_16_F_r8

subroutine get_minmax_16_F_i8(min, max, vec, n)
    implicit none
    integer(kind=8), intent(inout) :: min, max
    integer(kind=8), intent(in)    :: vec(n)
    integer(kind=8), intent(in)    :: n

    integer(kind=8) :: i, n_unroll, n_remain, unroll_size
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/get_minmax/inc_get_minmax_16.f90"
end subroutine get_minmax_16_F_i8






