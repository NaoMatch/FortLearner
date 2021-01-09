call identity(rotation_matrix, n_dim)
call random_number(x); x = two * x - one
call random_number(y); y = two * y - one
x = x / sqrt(sum(x**two))
y = y / sqrt(sum(y**two))

! rotate random generated vector x to (r,0,....,0) with r=|x|.
do i=n_dim, 2, -1
    x_norm = one / sqrt(x(i)**two + x(i-1)**two)
    c_theta = x(i-1) * x_norm
    s_theta = -x(i) * x_norm
    loc = (/i-1, i/)
    call rotate_from_right(rotation_matrix, n_dim, loc, c_theta, s_theta)
    x(i) = 0
    x(i-1) = one / x_norm
end do

! rotate vector (r,0,....,0) to y.
y_pre = one
do i=2, n_dim, 1
    y_s = sqrt(sum(y(i:)**two))
    y_c = sqrt(y_pre**two-y_s**two)
    y_norm = one / y_pre
    c_theta = y_c * y_norm
    s_theta = -y_s * y_norm
    loc = (/i-1, i/)
    call rotate_from_right(rotation_matrix, n_dim, loc, c_theta, -s_theta)
    y(i) = y_s
    y(i-1) = y_c
    y_pre = y_s
end do

