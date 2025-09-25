#ifdef FAIL
    program fail_kmeans_invalid_not_fitted
        use :: mod_kinds
        use :: mod_kmeans,      only : kmeans, new_kmeans
        implicit none
        type(kmeans) :: km
        real(r64), allocatable  :: X(:,:)
        integer(i64), allocatable :: labels(:,:)
        integer(i64) :: n, d
        n = 1000;  d = 10
        allocate(X(n, d))
        call random_number(X)
        km = new_kmeans()   ! → error stop EXPECT
        ! call km%fit(X)
        labels = km%predict(X)
    end program
#endif

#ifdef TEST
    program test_kmeans_invalid_not_fitted
        use :: mod_error_codes, only : ERR_NOT_FITTED
        implicit none
        integer :: estat
        call execute_command_line("./fail_kmeans_invalid_not_fitted", exitstat = estat)
        if (estat == ERR_NOT_FITTED) stop 0
        write(*,*) "FAIL: exit=", estat, " expect=", ERR_NOT_FITTED
        error stop 1
    end program
#endif
