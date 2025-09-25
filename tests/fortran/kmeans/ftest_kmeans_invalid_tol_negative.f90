#ifdef FAIL
    program fail_kmeans_invalid_tol_negative
        use :: mod_kinds
        use :: mod_kmeans,      only : kmeans, new_kmeans
        implicit none
        type(kmeans) :: km
        km = new_kmeans(tol = -10.0_r64)   ! → error stop EXPECT
    end program
#endif

#ifdef TEST
    program test_kmeans_invalid_tol
        use :: mod_error_codes, only : ERR_NEGATIVE_VALUE
        implicit none
        integer :: estat
        call execute_command_line("./fail_kmeans_invalid_tol_negative", exitstat = estat)
        if (estat == ERR_NEGATIVE_VALUE) stop 0
        write(*,*) "FAIL: exit=", estat, " expect=", ERR_NEGATIVE_VALUE
        error stop 1
    end program
#endif
