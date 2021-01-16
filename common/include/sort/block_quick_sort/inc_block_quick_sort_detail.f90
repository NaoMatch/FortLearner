l     = 1
r     = num
if ( num .le. 2 * 31 ) then
    call quick_sort(vector, num)
    return
end if

one = 1
three = 3
pivots(1) = vector(1)
pivots(2) = vector((1+num)/2)
pivots(3) = vector(num)
call insertion_sort(pivots, three)
pivot = pivots(2)

start_l = 0
start_r = 0
num_l   = 1
num_r   = 1

do while ( r-l+1 .gt. 2 * 31 )
    if ( num_l .eq. 1 ) then
        start_l = 0
        do i=1, 31
            offset_l(num_l) = i
            one = pivot .lt. vector(l+i-1)
            num_l = num_l + one
        end do
    end if

    if ( num_r .eq. 1 ) then
        start_r = 0
        do i=1, 31
            offset_r(num_r) = i
            one = pivot .ge. vector(r-i+1)
            num_r = num_r + one
        end do
    end if

    num_l = num_l - 1
    num_r = num_r - 1
    num_m = minval((/num_l, num_r/))

    do i=1, num_m, 1
        idx_l         = l+offset_l(start_l+i)-1
        idx_r         = r-offset_r(start_r+i)+1
        tmp           = vector(idx_l)
        vector(idx_l) = vector(idx_r)
        vector(idx_r) = tmp
    end do

    num_l = num_l - num_m + 1
    num_r = num_r - num_m + 1
    start_l = start_l + num_m
    start_r = start_r + num_m

    if ( num_l .eq. 1 ) l = l + 31-1
    if ( num_r .eq. 1 ) r = r - 31+1
end do

call quick_sort(vector(l:r), r-l+1)

next_l = 0
next_r = 0
do i=l, r
    if (vector(i) .le. pivot) cycle
    next_l = i-1
    next_r = i
    exit
end do

if ( next_l .eq. next_r .and. next_l .eq. 0 ) then
    call quick_sort(vector, num)
    return
end if