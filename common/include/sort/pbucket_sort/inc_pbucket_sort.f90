recursive subroutine pbucket_sort_r8(vector, n_samples, rec_depth)
    implicit none
    real(kind=8), intent(inout)           :: vector(n_samples)
    integer(kind=8), intent(in)           :: n_samples
    integer(kind=8), optional, intent(in) :: rec_depth

    integer(kind=8)                :: rec_depth_opt
    real(kind=8)                   :: vector_min, vector_max
    integer(kind=8)                :: n_sampling
    integer(kind=8), allocatable   :: indices(:)
    real(kind=8), allocatable      :: values(:)
    integer(kind=8)                :: two=2, one=1, zero=0
    integer(kind=8)                :: i, idx
    integer(kind=8)                :: n_unique, n_buckets
    integer(kind=8)                :: left_posi
    ! =============================================================
    type(tmp_work_r8), allocatable :: works(:)
    integer(kind=8), allocatable :: counters(:), sizes(:)
    integer(kind=8) :: b, min_bucket_size
    integer(kind=8) :: thousand=1000
    real(kind=8)    :: diff, max_val, factor
    real(kind=8)    :: tmp_v, cnt_b, size_b
    integer(kind=8) :: bucket_idx
    real(kind=8), allocatable :: tmp_x(:)
    integer(kind=8) :: k, j, ini, fin
    integer(kind=8) :: n_samples_unroll
    real(kind=8) :: buffer_v(63)
    integer(kind=8) :: buffer_b(63)
    integer(kind=8) :: min_remain
    include "./include/sort/pbucket_sort/inc_pbucket_sort_detail.f90"
end subroutine pbucket_sort_r8

recursive subroutine pbucket_sort_i4(vector, n_samples, rec_depth)
    implicit none
    integer(kind=4), intent(inout)        :: vector(n_samples)
    integer(kind=4), intent(in)           :: n_samples
    integer(kind=4), optional, intent(in) :: rec_depth

    integer(kind=4)                :: rec_depth_opt
    integer(kind=4)                :: vector_min, vector_max
    integer(kind=4)                :: n_sampling
    integer(kind=4), allocatable   :: indices(:)
    integer(kind=4), allocatable   :: values(:)
    integer(kind=4)                :: two=2, one=1, zero=0
    integer(kind=4)                :: i, idx
    integer(kind=4)                :: n_unique, n_buckets
    integer(kind=4)                :: left_posi
    ! =============================================================
    type(tmp_work_i4), allocatable :: works(:)
    integer(kind=4), allocatable :: counters(:), sizes(:)
    integer(kind=4) :: b, min_bucket_size
    integer(kind=4) :: thousand=1000
    real(kind=4)    :: diff, max_val, factor
    integer(kind=4)    :: tmp_v
    integer(kind=4) :: cnt_b, size_b
    integer(kind=4) :: bucket_idx
    integer(kind=4), allocatable :: tmp_x(:)
    integer(kind=4) :: k, j, ini, fin
    integer(kind=4) :: n_samples_unroll
    integer(kind=4) :: buffer_v(63)
    integer(kind=4) :: buffer_b(63)
    integer(kind=4) :: min_remain
    include "./include/sort/pbucket_sort/inc_pbucket_sort_detail.f90"
end subroutine pbucket_sort_i4

recursive subroutine pbucket_sort_i8(vector, n_samples, rec_depth)
    implicit none
    integer(kind=8), intent(inout)        :: vector(n_samples)
    integer(kind=8), intent(in)           :: n_samples
    integer(kind=8), optional, intent(in) :: rec_depth

    integer(kind=8)                :: rec_depth_opt
    integer(kind=8)                :: vector_min, vector_max
    integer(kind=8)                :: n_sampling
    integer(kind=8), allocatable   :: indices(:)
    integer(kind=8), allocatable   :: values(:)
    integer(kind=8)                :: two=2, one=1, zero=0
    integer(kind=8)                :: i, idx
    integer(kind=8)                :: n_unique, n_buckets
    integer(kind=8)                :: left_posi
    ! =============================================================
    type(tmp_work_i8), allocatable :: works(:)
    integer(kind=8), allocatable :: counters(:), sizes(:)
    integer(kind=8) :: b, min_bucket_size
    integer(kind=8) :: thousand=1000
    real(kind=8)    :: diff, max_val, factor
    integer(kind=8)    :: tmp_v
    integer(kind=8) :: cnt_b, size_b
    integer(kind=8) :: bucket_idx
    integer(kind=8), allocatable :: tmp_x(:)
    integer(kind=8) :: k, j, ini, fin
    integer(kind=8) :: n_samples_unroll
    integer(kind=8) :: buffer_v(63)
    integer(kind=8) :: buffer_b(63)
    integer(kind=8) :: min_remain
    include "./include/sort/pbucket_sort/inc_pbucket_sort_detail.f90"
end subroutine pbucket_sort_i8
