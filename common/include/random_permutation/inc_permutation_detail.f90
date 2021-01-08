call random_number(rand_vals)
do i = num, 2, -1
    r = rand_vals(i)
    randpos = int(r * i, kind=8) + 1_8
    tmp = vector(randpos)
    vector(randpos) = vector(i)
    vector(i) = tmp
end do
