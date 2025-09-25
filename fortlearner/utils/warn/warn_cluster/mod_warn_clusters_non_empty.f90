module mod_warn_clusters_non_empty
    use :: mod_kinds
    use :: mod_config
    use :: iso_fortran_env,    only: error_unit
    use :: mod_character_helpers, only: bound_str, make_prefix
    use :: mod_program_limits, only: MAX_ARRAY_SIZE_GB
    implicit none

contains

    subroutine warn_clusters_non_empty(labels_counter, &
                                   file, class_name, value_name)
        implicit none
        integer(i64), intent(in) :: labels_counter(:)
        character(*), intent(in) :: file, class_name, value_name

        integer(i64), allocatable :: empty_idx(:)
        character(:), allocatable :: prefix
        integer(i64) :: n_empty, i

        if (.not. ENABLE_WARNING) return

        empty_idx = pack([(i,i=1,size(labels_counter))], labels_counter==0_i64)
        n_empty   = size(empty_idx)

        if (n_empty > 0) then
            prefix  = make_prefix(file, class_name, value_name)

            write(error_unit,'(a,"Empty cluster detected: ",i0," cluster(s) -> Idx=",*(i0,:,","))') &
                prefix, n_empty, empty_idx
        end if
    end subroutine warn_clusters_non_empty

end module mod_warn_clusters_non_empty
