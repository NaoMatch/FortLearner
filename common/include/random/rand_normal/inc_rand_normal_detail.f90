two = real(2.0, kind=kind(two))
call random_number(tmp)
array = sqrt(-two * log(tmp))

call random_number(tmp)
array = array * cos(two*pi_*tmp)
