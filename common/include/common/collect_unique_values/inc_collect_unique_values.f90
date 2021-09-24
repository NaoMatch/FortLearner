
subroutine collect_unique_values_r8(uniq_vals, vector, n_samples)
    implicit none
    real(kind=8), allocatable   :: uniq_vals(:)
    real(kind=8), intent(inout) :: vector(n_samples)
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8) :: n_unique, n, idx
    include "./include/common/collect_unique_values/inc_collect_unique_values_detail.f90"
end subroutine collect_unique_values_r8

subroutine collect_unique_values_i4(uniq_vals, vector, n_samples)
    implicit none
    integer(kind=4), allocatable   :: uniq_vals(:)
    integer(kind=4), intent(inout) :: vector(n_samples)
    integer(kind=4), intent(in)    :: n_samples

    integer(kind=4) :: n_unique, n, idx
    include "./include/common/collect_unique_values/inc_collect_unique_values_detail.f90"
end subroutine collect_unique_values_i4

subroutine collect_unique_values_i8(uniq_vals, vector, n_samples)
    implicit none
    integer(kind=8), allocatable   :: uniq_vals(:)
    integer(kind=8), intent(inout) :: vector(n_samples)
    integer(kind=8), intent(in)    :: n_samples

    integer(kind=8) :: n_unique, n, idx

    if (allocated(uniq_vals)) deallocate(uniq_vals)
    include "./include/common/collect_unique_values/inc_collect_unique_values_detail.f90"
end subroutine collect_unique_values_i8
