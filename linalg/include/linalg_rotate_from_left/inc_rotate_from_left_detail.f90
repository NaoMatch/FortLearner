p = loc(1)
q = loc(2)
do i=1, n_dim
    mat_p = mat(i,p)
    mat_q = mat(i,q)
    mat(i,p) =   mat_p * c_theta + mat_q * s_theta
    mat(i,q) = - mat_p * s_theta + mat_q * c_theta
end do
