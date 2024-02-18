step = find_nearest_power_of_two_below(n_samples)

idx = 0
step_ = n_samples - step
flg = vector(idx+step_) .lt. value
idx = (idx+step_)*flg + idx*(1-flg)

step_ = ishft(step,-1)
do while (step_ > 0_8)
    flg = vector(idx+step_) .lt. value
    idx = (idx+step_)*flg + idx*(1-flg)
    step_ = ishft(step_,-1)
end do   
idx = idx + 1  