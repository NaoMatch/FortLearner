function roulette_selection_r8(vector, n_samples, reverse)
    real(kind=8), intent(inout) :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    logical(kind=4), intent(in) :: reverse
    integer(kind=8) :: roulette_selection_r8
    integer(kind=8) :: idx, i
    real(kind=8) :: rand, cumsum
    include "./include/random/roulette_selection/inc_roulette_selection_detail.f90"
    roulette_selection_r8 = idx
end function roulette_selection_r8
