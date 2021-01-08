    function roulette_selection_r8(vector, num, reverse)
        real(kind=8), intent(inout) :: vector(num)
        integer(kind=8), intent(in) :: num
        logical(kind=4), intent(in) :: reverse
        integer(kind=8) :: roulette_selection_r8
        integer(kind=8) :: idx, i
        real(kind=8) :: rand, cumsum
        call random_number(rand)
        cumsum = 0.0d0
        if (reverse) then
            do i=num, 1, -1
                cumsum = cumsum + vector(i)
                if (cumsum .ge. rand) then
                    idx = i
                    exit
                end if
            end do
        else
            do i=1, num, 1
                cumsum = cumsum + vector(i)
                if (cumsum .ge. rand) then
                    idx = i
                    exit
                end if
            end do
        end if
        roulette_selection_r8 = idx
    end function roulette_selection_r8
