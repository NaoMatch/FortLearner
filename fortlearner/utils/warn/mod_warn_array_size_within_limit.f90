module mod_warn_array_size_within_limit 
    use :: mod_kinds,             only: i64
    use :: mod_config,            only: ENABLE_WARNING
    use :: iso_fortran_env,       only: error_unit
    use :: mod_character_helpers, only: make_prefix
    use :: mod_program_limits,    only: MAX_ARRAY_SIZE_GB
    implicit none


contains

    subroutine warn_array_size_within_limit(shape, file, class_name, value_name)
        !-----------------------------------------------------------------
        ! Warn if the array implied by SHAPE would exceed MAX_ARRAY_SIZE_GB.
        !
        ! Notes
        !   * elem_bytes is fixed to 8 B (REAL(r64) / default distance matrix element).
        !   * If the request is over the 4 GiB limit, emit a message suggesting
        !     chunked computation (chunk_rows) instead of full-matrix allocation.
        !-----------------------------------------------------------------
        integer(i64), intent(in) :: shape(:)
        character(*), intent(in) :: file, class_name, value_name

        integer(i64), parameter :: ELEM_BYTES = 8_i64          ! fixed element size
        integer(i64)            :: bytes
        integer(i64)            :: req_gib, lim_gib
        character(:), allocatable :: prefix

        if (.not. ENABLE_WARNING) return

        ! total bytes for the requested distance matrix
        bytes = product(shape) * ELEM_BYTES

        !–— act only when bytes > 4 GiB ————————————————————————————————
        if (bytes > MAX_ARRAY_SIZE_GB) then
            ! round *up* to whole GiB so the user sees the worst-case need
            req_gib = (bytes + 1024_i64**3 - 1_i64) / (1024_i64**3)
            lim_gib = MAX_ARRAY_SIZE_GB / (1024_i64**3)
            prefix  = make_prefix(file, class_name, value_name)

            write(error_unit,'("[Memory-Warning] ",a,": requested ",i0," GiB exceeds limit ",i0," GiB (MAX_ARRAY_SIZE_GB). ", &
                "Consider enabling chunked distance-matrix computation (e.g., set chunk_rows) to avoid allocation failure.")') &
                trim(prefix), req_gib, lim_gib
        end if
    end subroutine warn_array_size_within_limit 

end module mod_warn_array_size_within_limit 
