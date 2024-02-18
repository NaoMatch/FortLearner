step = find_nearest_power_of_two_below(n_samples)

idx = 0
step_ = n_samples - step
flg = value .lt. vector(idx+step_)
idx = (idx+step_)*(1-flg) + idx*(flg)

step_ = ishft(step,-1)
do while (step_ > 0_8)
    flg = value .lt. vector(idx+step_)
    idx = (idx+step_)*(1-flg) + idx*(flg)
    step_ = ishft(step_,-1)
end do   
idx = idx + 1  