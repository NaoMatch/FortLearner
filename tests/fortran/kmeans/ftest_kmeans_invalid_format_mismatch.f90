#ifdef FAIL
    program fail_kmeans_invalid_format_mismatch
        use :: mod_kinds
        use :: mod_kmeans,        only : kmeans, new_kmeans
        use :: mod_config, only : FORMAT_VER     ! 現行番号
        implicit none
        integer(i64), parameter :: BUFSIZE = 4                 ! i64 = 4B
        integer(i64)            :: u
        integer(i64)      :: bad_ver
        type(kmeans) :: km
        character(len=*), parameter :: fn = "bad_model.bin"

        integer(i64),  parameter :: n = 500, d = 4, k = 3
        real(r64),  allocatable :: X(:,:)

        ! ---- データ生成 ----
        allocate(X(n,d))
        call random_number(X)

        ! ---- 1. 正常モデルを作って保存 ----
        km = new_kmeans()
        call km%fit(x)
        call km%dump(fn)     ! new_kmeans() は初期化ヘルパ

        ! ---- 2. 先頭 4 バイトを書き換えて ver+1 ----
        open(newunit=u, file=fn, access='stream', form='unformatted', &
             action='readwrite')
        bad_ver = int(FORMAT_VER+1, kind=i64)
        write(u, pos=1) bad_ver           ! 先頭を上書き
        close(u)

        ! ---- 3. Load → エラー停止期待 ----
        call km%load(fn)      ! → error stop ERR_INVALID_FORMAT_VERSION
    end program
#endif

#ifdef TEST
    program test_fmt_mismatch
        use :: mod_error_codes,  only : ERR_INVALID_FORMAT_VERSION
        implicit none
        integer :: estat
        call execute_command_line("./fail_kmeans_invalid_format_mismatch", exitstat=estat)

        if (estat == ERR_INVALID_FORMAT_VERSION) stop 0
        write(*,'("FAIL: exit=",i0," expect=",i0)') estat, ERR_INVALID_FORMAT_VERSION
        error stop 1
    end program
#endif