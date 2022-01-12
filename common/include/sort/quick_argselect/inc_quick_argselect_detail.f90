call quick_argselect_rec(vector1, vector2, n_samples, n_th)
allocate( is_same(n_samples-n_th+1) )
if (left_align) call align_left_arg(vector1(n_th:), vector2(n_th:), is_same, n_samples-n_th+1)
