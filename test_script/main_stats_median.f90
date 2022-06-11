program main_stats_median
    use mod_common
    use mod_stats
    use mod_sort
    implicit none


    integer(kind=8) :: n
    real(kind=8), allocatable :: x(:), y(:), z(:)
    real(kind=8) :: med_val_1, med_val_2

    n = 10011
    allocate(x(n))
    allocate(y(n))
    allocate(z(n))

    call random_number(x)
    y = x
    z = x

    med_val_1 = median_r8(x, n)
    call quick_sort(y, n)
    print*, med_val_1
    if (mod(n,2)==0) then
        print*, ( y(n/2) + y(n/2+1) ) * .5d0
    else
        print*, y(n/2+1)
    end if
    med_val_2 = median(z, n)
    print*, med_val_2


contains

    function median_r8(x, n_samples)
        implicit none
        real(kind=8)                :: median_r8
        real(kind=8), intent(in)    :: x(n_samples)
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8)             :: half_position
        real(kind=8), allocatable   :: x_copy(:)
        real(kind=8)                :: min_val_half
        logical(kind=4)             :: is_odd

        allocate(x_copy(n_samples))
        x_copy(:) = x(:)

        is_odd = mod(n_samples, 2_8) == 0_8
        half_position = n_samples / 2_8

        call quick_select(x_copy, n_samples, half_position)
        if (is_odd) then
            min_val_half = minval(x_copy(half_position+1:n_samples))
            median_r8 = (x_copy(half_position)+min_val_half) * .5d0
        else
            median_r8 = x_copy(half_position+1)
        end if
    end function median_r8 
  
end program main_stats_median