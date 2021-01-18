n_samples_tot = zero_r
do c=1, n_classes, 1
    n_samples_tot = n_samples_tot + real(class_counts(c), kind=kind(zero_r))
end do

allocate(probas(n_classes))
do c=1, n_classes, 1
    if ( class_counts(c) .eq. zero_i ) then
        probas(c) = zero_r
    else
        probas(c) = real(class_counts(c), kind=kind(zero_r)) / n_samples_tot
    end if
end do

tmp_result = zero_r
do c=1, n_classes, 1
    if ( probas(c) .eq. zero_r ) cycle
    tmp_result = tmp_result - probas(c) * log2(probas(c))
end do
