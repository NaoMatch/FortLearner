call random_number(rand)
cumsum = real(0.0, kind=kind(cumsum))
if (reverse) then
    do i=n_samples, 1, -1
        cumsum = cumsum + vector(i)
        if (cumsum .ge. rand) then
            idx = i
            exit
        end if
    end do
else
    do i=1, n_samples, 1
        cumsum = cumsum + vector(i)
        if (cumsum .ge. rand) then
            idx = i
            exit
        end if
    end do
end if
