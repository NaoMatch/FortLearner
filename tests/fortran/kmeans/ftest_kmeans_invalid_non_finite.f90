#ifdef FAIL
    program fail_kmeans_invalid_non_finite
        use, intrinsic :: ieee_arithmetic
        use :: mod_kinds
        use :: mod_kmeans,      only : kmeans, new_kmeans
        implicit none
        type(kmeans) :: km
        real(r64), allocatable  :: X(:,:)

        allocate(X(10, 2))
        call random_number(X)

        X(1,1) = ieee_value(0.0_r64, ieee_quiet_nan)
        X(2,2) = ieee_value(0.0_r64,  ieee_positive_inf)

        km = new_kmeans()   ! → error stop EXPECT
        call km%fit(X)

    end program
#endif

#ifdef TEST
    program test_kmeans_invalid_non_finite
        use :: mod_error_codes, only : ERR_NON_FINITE
        implicit none
        integer :: estat
        call execute_command_line("./fail_kmeans_invalid_non_finite", exitstat = estat)
        if (estat == ERR_NON_FINITE) stop 0
        write(*,*) "FAIL: exit=", estat, " expect=", ERR_NON_FINITE
        error stop 1
    end program
#endif
