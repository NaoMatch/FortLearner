module mod_quick_select_nth_element
    use mod_sort
    implicit none
    

contains

    recursive function median(vector, n_samples) result(med)
        implicit none
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in)    :: vector(n_samples)
        real(kind=8)                :: med

        real(kind=8) :: idx
    end function median


    recursive subroutine quick_select(vector, n_samples, n_th)
        implicit none
        real(kind=8), intent(inout) :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8), intent(in) :: n_th

        real(kind=8) :: pivot, tmp
        integer(kind=8) :: i, j, idx, n_th_new

        if (n_samples .le. 32_8) then
            call insertion_sort(vector, n_samples)
            return
        end if

        idx = (1_8+n_samples) / 2_8
        pivot = vector(idx)
        i = 1
        j = n_samples

        do
            do while (vector(i) < pivot)
                i=i+1
            end do
            do while (pivot < vector(j))
                j=j-1
            end do
            if (i >= j) exit
            tmp = vector(i);  vector(i) = vector(j);  vector(j) = tmp
            i=i+1
            j=j-1
        end do

        if (n_th .le. j) then
            call quick_select(vector(1:j), j, n_th)
        else
            call quick_select(vector(i:), n_samples-i+1, n_th-i+1)
        end if
    end subroutine quick_select


    recursive subroutine quick_argselect(vector, indices, n_samples, n_th)
        implicit none
        real(kind=8), intent(inout) :: vector(n_samples)
        integer(kind=8), intent(inout) :: indices(n_samples)
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8), intent(in) :: n_th

        real(kind=8) :: pivot, tmp_r
        integer(kind=8) :: i, j, idx, tmp_i 

        if (n_samples .le. 32_8) then
            call insertion_argsort(vector, indices, n_samples)
            return
        end if

        idx = (1_8+n_samples) / 2_8
        pivot = vector(idx)
        i = 1
        j = n_samples

        do
            do while (vector(i) < pivot)
                i=i+1
            end do
            do while (pivot < vector(j))
                j=j-1
            end do
            if (i >= j) exit
            tmp_r = vector(i);  vector(i)  = vector(j);  vector(j)  = tmp_r
            tmp_i = indices(i); indices(i) = indices(j); indices(j) = tmp_i
            i=i+1
            j=j-1
        end do

        if (n_th .le. j) then
            call quick_argselect(vector(1:j), indices(1:j), j, n_th)
        else
            call quick_argselect(vector(i:), indices(i:), n_samples-i+1, n_th-i+1)
        end if
    end subroutine quick_argselect

end module mod_quick_select_nth_element
