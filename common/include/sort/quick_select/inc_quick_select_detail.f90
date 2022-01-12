call quick_select_rec(vector, n_samples, n_th)
allocate( is_same(n_samples-n_th+1) )
if (left_align) call align_left(vector(n_th:), is_same, n_samples-n_th+1)
