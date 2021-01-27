n_columns = data_holder_ptr%n_columns
allocate(feature_indices_(n_columns))
allocate(feature_indices_scanning_range_(2))
if ( present(feature_indices) ) then
    is_permute_per_node = f_
    do n=1, n_columns, 1
        feature_indices_(n) = feature_indices(n)
    end do
    feature_indices_scanning_range_(1) = feature_indices_scanning_range(1)
    feature_indices_scanning_range_(2) = feature_indices_scanning_range(2)
else
    is_permute_per_node = t_
    do n=1, n_columns, 1
        feature_indices_(n) = n
    end do
    feature_indices_scanning_range_ = (/1_8, n_columns/)
end if
