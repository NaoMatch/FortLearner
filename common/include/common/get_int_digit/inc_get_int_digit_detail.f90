if (num .lt. int(0, kind=kind(num))) then
    num_digit = int(log10(dble(abs(num)))) + 1
    num_digit = num_digit + 1
else
    num_digit = int(log10(dble(num))) + 1
end if
