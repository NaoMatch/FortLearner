two = real(2.0, kind=kind(two))
call random_number(tmp)
vector = sqrt(-two * log(tmp))

call random_number(tmp)
vector = vector * cos(two*pi_*tmp)
