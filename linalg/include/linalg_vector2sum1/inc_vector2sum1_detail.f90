sum_vec = 0.0

if (minval(vector) .lt. sum_vec) then
    print*, "Warning: minimum value of vector is negative."
end if

do n=1, n_samples, 1
    sum_vec = sum_vec + vector(n)
end do

if (sum_vec .eq. real(0.0, kind=kind(sum_vec))) then
    print*, "Warning: minimum value of vector is negative."
end if

sum_vec = sum_vec
sum_vec = 1.0 / sum_vec
do n=1, n_samples, 1
    vector(n) = vector(n) * sum_vec
end do
