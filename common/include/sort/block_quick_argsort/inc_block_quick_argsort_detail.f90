l     = 1
r     = num
if ( num .le. 2 * 15 ) then
    call quick_argsort(vector1, vector2, num)
    return
end if

one = 1
three = 3
pivots(1) = vector1(1)
pivots(2) = vector1((1+num)/2)
pivots(3) = vector1(num)
call insertion_sort(pivots, three)
pivot = pivots(3)

start_l = 0
start_r = 0
num_l   = 1
num_r   = 1

do while ( r-l+1 .gt. 2 * 15 )
    if ( num_l .eq. 1 ) then
        start_l = 0
        do i=1, 15
            offset_l(num_l) = i
            one = pivot .lt. vector1(l+i-1)
            num_l = num_l + one
        end do
    end if

    if ( num_r .eq. 1 ) then
        start_r = 0
        do i=1, 15
            offset_r(num_r) = i
            one = pivot .ge. vector1(r-i+1)
            num_r = num_r + one
        end do
    end if

    num_l = num_l - 1
    num_r = num_r - 1
    num_m = minval((/num_l, num_r/))

    do i=1, num_m, 1
        idx_l         = l+offset_l(start_l+i)-1
        idx_r         = r-offset_r(start_r+i)+1
        tmp1           = vector1(idx_l)
        vector1(idx_l) = vector1(idx_r)
        vector1(idx_r) = tmp1
        tmp2           = vector2(idx_l)
        vector2(idx_l) = vector2(idx_r)
        vector2(idx_r) = tmp2
    end do

    num_l = num_l - num_m + 1
    num_r = num_r - num_m + 1
    start_l = start_l + num_m
    start_r = start_r + num_m

    if ( num_l .eq. 1 ) l = l + 15-1
    if ( num_r .eq. 1 ) r = r - 15+1
end do

call quick_argsort(vector1(l:r), vector2(l:r), r-l+1)

next_l = 0
next_r = 0
do i=l, r
    if (vector1(i) .le. pivot) cycle
    next_l = i-1
    next_r = i
    exit
end do

if ( next_l .eq. next_r .and. next_l .eq. 0 ) then
    call quick_argsort(vector1, vector2, num)
    return
end if
