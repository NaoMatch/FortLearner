#ifdef FAIL
    program fail_kmeans_invalid_init
        use :: mod_kinds
        use :: mod_kmeans,      only : kmeans, new_kmeans
        implicit none
        type(kmeans) :: km
        km = new_kmeans(init = "invalid_option")   ! → error stop EXPECT
    end program
#endif

#ifdef TEST
    program test_kmeans_invalid_init
        use :: mod_error_codes, only : ERR_DISALLOWED_OPTION
        implicit none
        integer :: estat
        call execute_command_line("./fail_kmeans_invalid_init", exitstat = estat)
        if (estat == ERR_DISALLOWED_OPTION) stop 0
        write(*,*) "FAIL: exit=", estat, " expect=", ERR_DISALLOWED_OPTION
        error stop 1
    end program
#endif
