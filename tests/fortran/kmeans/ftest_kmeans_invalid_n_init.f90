#ifdef FAIL
    program fail_kmeans_invalid_n_init
        use :: mod_kinds
        use :: mod_kmeans,      only : kmeans, new_kmeans
        implicit none
        type(kmeans) :: km
        km = new_kmeans(n_init = 0_i64)   ! → error stop EXPECT
    end program
#endif

#ifdef TEST
    program test_kmeans_invalid_n_init
        use :: mod_error_codes, only : ERR_NON_POSITIVE_VALUE
        implicit none
        integer :: estat
        call execute_command_line("./fail_kmeans_invalid_n_init", exitstat = estat)
        if (estat == ERR_NON_POSITIVE_VALUE) stop 0
        write(*,*) "FAIL: exit=", estat, " expect=", ERR_NON_POSITIVE_VALUE
        error stop 1
    end program
#endif
