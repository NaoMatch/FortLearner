#ifdef FAIL
    program fail_kmeans_invalid_tol_inf
        use :: mod_kinds
        use :: mod_kmeans,      only : kmeans, new_kmeans
        use, intrinsic :: ieee_arithmetic
        implicit none
        type(kmeans) :: km
        real(r64) :: tol_inf
        tol_inf = ieee_value(0.0_r64, ieee_positive_inf)
        km = new_kmeans(tol = tol_inf)   ! → error stop EXPECT
    end program
#endif

#ifdef TEST
    program test_kmeans_invalid_tol_inf
        use :: mod_error_codes, only : ERR_NON_FINITE
        implicit none
        integer :: estat
        call execute_command_line("./fail_kmeans_invalid_tol_inf", exitstat = estat)
        if (estat == ERR_NON_FINITE) stop 0
        write(*,*) "FAIL: exit=", estat, " expect=", ERR_NON_FINITE
        error stop 1
    end program
#endif
