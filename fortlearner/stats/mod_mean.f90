module mod_mean
    use :: mod_kinds
    use :: mod_predicates,    only: is_positive_sized_array, is_finite, is_allowed_option
    implicit none

contains

    ! MARK: Mean Vector
    subroutine mean_vec_r64(mean, vec, n, status, stable, do_check)
        implicit none
        real(r64), intent(out)        :: mean
        real(r64), intent(in)         :: vec(n)
        integer(i64), intent(in)      :: n
        integer(i64), intent(out)     :: status
        logical, intent(in), optional :: stable
        logical, intent(in), optional :: do_check

        logical      :: stable_   = .false.
        logical      :: do_check_ = .true.
        logical      :: happened

        ! Optional Argument
        if (present(stable))   stable_   = stable
        if (present(do_check)) do_check_ = do_check

        if (do_check_) then
            ! Empty Vector
            ! Contain nan/inf
            ! happened = (is_empty_vector_r64(vec)) &
            !         .or.  (has_non_finite_vec_r64(vec))
            happened = (.not. is_positive_sized_array(vec))
            happened = (.not. all(is_finite(vec))) .or. happened
            if (happened) then
                status = 1_i64
                return
            end if
        end if

        ! Compute Mean
        if (stable_) then
            call mean_vec_stable_r64(mean, vec, n)
        else
            call mean_vec_canonical_r64(mean, vec, n)
        end if
        status = 0_i64 ! Success
    end subroutine mean_vec_r64

    subroutine mean_vec_canonical_r64(mean, vec, n)
        real(r64), intent(out)        :: mean
        real(r64), intent(in)         :: vec(n)
        integer(i64), intent(in)      :: n

        mean = sum(vec) / real(n, r64)
    end subroutine mean_vec_canonical_r64

    subroutine mean_vec_stable_r64(mean, vec, n)
        real(r64), intent(out)        :: mean
        real(r64), intent(in)         :: vec(n)
        integer(i64), intent(in)      :: n

        integer(i64) :: i
        mean = 0.0_r64
        do i=1, n, 1
            mean = mean + (vec(i)-mean) / real(i, r64)
        end do
    end subroutine mean_vec_stable_r64


    ! MARK; Mean Matrix
    subroutine mean_mat_r64(mean, mat, n, m, status, stable, do_check)
        real(r64), intent(out)        :: mean
        real(r64), intent(in)         :: mat(n, m)
        integer(i64), intent(in)      :: n, m
        integer(i64), intent(out)     :: status
        logical, intent(in), optional :: stable
        logical, intent(in), optional :: do_check

        logical      :: stable_   = .false.
        logical      :: do_check_ = .true.
        logical      :: happened

        ! Optional Argument
        if (present(stable))   stable_   = stable
        if (present(do_check)) do_check_ = do_check

        if (do_check_) then
            happened = (.not. is_positive_sized_array(mat))
            happened = (.not. all(is_finite(mat))) .or. happened
            if (happened) then
                status = 1_i64
                return
            end if
        end if

        ! Compute Mean
        if (stable_) then
            call mean_mat_stable_r64(mean, mat, n, m)
        else
            call mean_mat_canonical_r64(mean, mat, n, m)
        end if
        status = 0_i64 ! Success
    end subroutine mean_mat_r64


    subroutine mean_mat_canonical_r64(mean, mat, n, m)
        real(r64), intent(out)        :: mean
        real(r64), intent(in)         :: mat(n, m)
        integer(i64), intent(in)      :: n, m

        mean = sum(mat) / real(n*m, r64)
    end subroutine mean_mat_canonical_r64

    subroutine mean_mat_stable_r64(mean, mat, n, m)
        real(r64), intent(out)        :: mean
        real(r64), intent(in)         :: mat(n, m)
        integer(i64), intent(in)      :: n, m

        integer(i64) :: i, j, counter
        mean = 0.0_r64
        counter = 0_i64
        do j=1, m, 1
            do i=1, n, 1
                counter = counter + 1_i64
                mean = mean + (mat(i,j)-mean) / real(counter, r64)
            end do
        end do
    end subroutine mean_mat_stable_r64


    subroutine mean_mat_axis_r64(mean, mat, n, m, axis, status, stable, do_check)
        real(r64), intent(out)        :: mean(:)
        real(r64), intent(in)         :: mat(n, m)
        integer(i64), intent(in)      :: n, m
        integer(i64), intent(in)      :: axis
        integer(i64), intent(out)     :: status
        logical, intent(in), optional :: stable
        logical, intent(in), optional :: do_check

        logical      :: stable_   = .false.
        logical      :: do_check_ = .true.
        logical      :: happened

        ! Optional Argument
        if (present(stable))   stable_   = stable
        if (present(do_check)) do_check_ = do_check

        if (do_check_) then
            ! Empty Matrix
            ! Contain nan/inf
            ! Shape Check, dim=1/2
            happened = (.not. is_positive_sized_array(mat))
            happened = (.not. all(is_finite(mat))) .or. happened
            happened = (is_allowed_option(axis, [1_i64, 2_i64])) .or. happened
            if (happened) then
                status = 1_i64
                return
            end if
        end if

        ! Compute Mean
        if (axis == 1_i64) then
            if (stable_) then
                call mean_mat_axis1_stable_r64(mean, mat, n, m)
            else
                call mean_mat_axis1_canonical_r64(mean, mat, n, m)
            end if
        else
            if (stable_) then
                call mean_mat_axis2_stable_r64(mean, mat, n, m)
            else
                call mean_mat_axis2_canonical_r64(mean, mat, n, m)
            end if
        end if
        status = 0_i64 ! Success
    end subroutine mean_mat_axis_r64


    subroutine mean_mat_axis1_stable_r64(mean, mat, n, m)
        real(r64), intent(out)        :: mean(m)
        real(r64), intent(in)         :: mat(n, m)
        integer(i64), intent(in)      :: n, m

        real(r64) :: tmp_mean
        integer(i64) :: i, j

        do j=1, m, 1
            tmp_mean = 0.0_r64
            do i=1, n, 1
                tmp_mean = tmp_mean + (mat(i,j)-tmp_mean) / real(i, r64)
            end do
            mean(j) = tmp_mean
        end do
    end subroutine mean_mat_axis1_stable_r64


    subroutine mean_mat_axis1_canonical_r64(mean, mat, n, m)
        real(r64), intent(out)        :: mean(m)
        real(r64), intent(in)         :: mat(n, m)
        integer(i64), intent(in)      :: n, m

        mean(:) = sum(mat, dim=1) / real(n, r64)
    end subroutine mean_mat_axis1_canonical_r64


    subroutine mean_mat_axis2_stable_r64(mean, mat, n, m)
        real(r64), intent(out)        :: mean(n)
        real(r64), intent(in)         :: mat(n, m)
        integer(i64), intent(in)      :: n, m

        real(r64) :: tmp_mean
        integer(i64) :: i, j

        do i=1, n, 1
            tmp_mean = 0.0_r64
            do j=1, m, 1
                tmp_mean = tmp_mean + (mat(i,j)-tmp_mean) / real(j, r64)
            end do
            mean(i) = tmp_mean
        end do
    end subroutine mean_mat_axis2_stable_r64


    subroutine mean_mat_axis2_canonical_r64(mean, mat, n, m)
        real(r64), intent(out)        :: mean(n)
        real(r64), intent(in)         :: mat(n, m)
        integer(i64), intent(in)      :: n, m

        mean(:) = sum(mat, dim=2) / real(m, r64)
    end subroutine mean_mat_axis2_canonical_r64



end module mod_mean