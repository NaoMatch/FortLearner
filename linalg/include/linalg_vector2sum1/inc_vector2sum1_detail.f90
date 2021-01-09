sum_vec = 0.0
do n=1, n_samples, 1
    sum_vec = sum_vec + vector(n)
end do

sum_vec = sum_vec
sum_vec = 1.0 / sum_vec
do n=1, n_samples, 1
    vector(n) = vector(n) * sum_vec
end do
