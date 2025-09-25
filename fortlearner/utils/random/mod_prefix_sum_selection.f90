module mod_prefix_sum_selection
    use :: mod_kinds
    use :: mod_binary_search, only : binary_search_r64
    implicit none

contains

    function prefix_sum_selection_r64(prefix_sum_vec, n) result(idx)
        implicit none
        real(r64), intent(in)    :: prefix_sum_vec(n)
        integer(i64), intent(in) :: n
        integer(i64)             :: idx

        real(r64) :: tmp

        call random_number(tmp)
        tmp = tmp * prefix_sum_vec(n)
        
        idx = binary_search_r64(prefix_sum_vec, tmp, n)
    end function prefix_sum_selection_r64
    

end module mod_prefix_sum_selection
