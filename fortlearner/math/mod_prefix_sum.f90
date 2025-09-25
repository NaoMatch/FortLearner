module mod_prefix_sum
    use :: mod_kinds
    implicit none


contains

    subroutine prefix_sum_r64(vec_in, vec_out, vec_size) 
        implicit none
        real(r64), intent(in)  :: vec_in(vec_size)
        real(r64), intent(out) :: vec_out(vec_size)
        integer(i64), intent(in) :: vec_size

        integer(i64) :: i
        real(r64)    :: tot

        tot = 0_r64
        do i=1, vec_size, 1
            tot = tot + vec_in(i)
            vec_out(i) = tot
        end do
    end subroutine prefix_sum_r64

    subroutine prefix_sum_i64(vec_in, vec_out, vec_size) 
        implicit none
        integer(i64), intent(in)  :: vec_in(vec_size)
        integer(i64), intent(out) :: vec_out(vec_size)
        integer(i64), intent(in) :: vec_size

        integer(i64) :: i
        integer(i64) :: tot

        tot = 0_i64
        do i=1, vec_size, 1
            tot = tot + vec_in(i)
            vec_out(i) = tot
        end do
    end subroutine prefix_sum_i64

end module mod_prefix_sum
