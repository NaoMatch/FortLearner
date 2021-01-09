n_samples_tot = 0
do c=1, n_classes, 1
    n_samples_tot = n_samples_tot + class_counts(c)
end do
n_samples_tot = 0
do c=1, n_classes, 1
    n_samples_tot = n_samples_tot + class_counts(c)**2.0
end do
