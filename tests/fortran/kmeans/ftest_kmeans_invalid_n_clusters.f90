#ifdef FAIL
    program fail_kmeans_invalid_n_clusters
        use :: mod_kinds
        use :: mod_kmeans,      only : kmeans, new_kmeans
        implicit none
        type(kmeans) :: km
        km = new_kmeans(n_clusters = 0_i64)   ! → error stop EXPECT
    end program
#endif

#ifdef TEST
    program test_kmeans_invalid_n_clusters
        use :: mod_error_codes, only : ERR_BELOW_MIN
        implicit none
        integer :: estat
        call execute_command_line("./fail_kmeans_invalid_n_clusters", exitstat = estat)
        if (estat == ERR_BELOW_MIN) stop 0
        write(*,*) "FAIL: exit=", estat, " expect=", ERR_BELOW_MIN
        error stop 1
    end program
#endif
