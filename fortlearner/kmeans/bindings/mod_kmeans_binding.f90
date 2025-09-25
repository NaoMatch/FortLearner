module mod_kmeans_binding
    use :: iso_c_binding
    use :: mod_kinds
    use :: mod_kmeans,          only: kmeans, new_kmeans
    use :: mod_kmeans_registry, only: kmeans_create_model, kmeans_pool, kmeans_used, initialized
    use :: mod_check,           only: check_file_exists
    use :: mod_error_manager,   only: last_error_code
    use :: mod_error_bindings,  only: get_last_error
    implicit none

contains

    !---------------------------------------
    ! ユーティリティ: Cのchar[] → Fortran文字列
    !---------------------------------------
    subroutine c_chars_to_fstring(cstr, fstr)
        use iso_c_binding
        implicit none
        character(kind=c_char), intent(in) :: cstr(*)
        character(:), allocatable          :: fstr
        integer :: n, i

        n = 0
        do i = 1, huge(0)
            if (cstr(i) == c_null_char) exit
            n = n + 1
        end do

        allocate(character(len=n) :: fstr)
        do i = 1, n
            fstr(i:i) = transfer(cstr(i), ' ')
        end do
    end subroutine c_chars_to_fstring

    subroutine c_chars_n_to_fstring(cstr, n_in, fstr)
        use iso_c_binding
        implicit none
        character(kind=c_char), intent(in) :: cstr(*)
        integer(c_size_t),      intent(in) :: n_in   ! bytes including null
        character(:), allocatable          :: fstr
        integer(c_size_t) :: n0
        integer :: i

        if (n_in <= 0_c_size_t) then
            allocate(character(len=0) :: fstr)
            return
        end if

        ! up to n_in-1 or first null
        n0 = 0_c_size_t
        do i = 1, int(n_in, kind=kind(i))
            if (i >= int(n_in, kind=kind(i))) exit
            if (cstr(i) == c_null_char) exit
            n0 = n0 + 1_c_size_t
        end do

        allocate(character(len=int(n0)) :: fstr)
        do i = 1, int(n0)
            fstr(i:i) = transfer(cstr(i), ' ')
        end do
    end subroutine c_chars_n_to_fstring

    function kmeans_create(n_clusters, max_iter, init, n_init, tol, &
        random_state, num_threads, print_log, chunk_size) result(model_id) bind(C, name="kmeans_create")
        integer(c_int64_t), value    :: n_clusters
        integer(c_int64_t), value    :: max_iter
        character(kind=c_char), intent(in) :: init(*)   ! Cのchar[]
        integer(c_int64_t), value    :: n_init
        real(c_double),  value    :: tol
        integer(c_int64_t), value    :: random_state
        integer(c_int64_t), value    :: num_threads
        logical(c_bool), value    :: print_log
        integer(c_int64_t), value    :: chunk_size
        
        integer(c_int64_t) :: model_id

        ! Fortran 側で使う変数
        integer(i64) :: n_clusters_
        integer(i64) :: max_iter_
        character(:), allocatable :: init_  ! Fortran文字列
        integer(i64) :: n_init_
        real(r64)    :: tol_
        integer(i64) :: random_state_
        integer(i64) :: num_threads_
        logical      :: print_log_
        integer(i64) :: chunk_size_

        ! ---- 変換処理 ----
        n_clusters_   = int(n_clusters, kind=i64)
        max_iter_     = int(max_iter, kind=i64)
        call c_chars_to_fstring(init, init_)
        n_init_       = int(n_init, kind=i64)
        tol_          = real(tol, kind=r64)
        random_state_ = int(random_state, kind=i64)
        num_threads_  = int(num_threads, kind=i64)
        print_log_    = (print_log)
        chunk_size_   = int(chunk_size, kind=i64)

        model_id = kmeans_create_model(n_clusters_, max_iter_, init_, n_init_, tol_, &
            random_state_, num_threads_, print_log_, chunk_size_)
    end function kmeans_create

    subroutine kmeans_fit(model_id, X, n_rows, n_cols) bind(C, name="kmeans_fit")
        integer(c_int64_t), value :: model_id, n_rows, n_cols
        real(c_double), intent(in) :: X(n_rows, n_cols)

        type(kmeans), pointer :: km
        km => get_model_by_id(model_id)

        if (.not. associated(km)) then
            print *, "[kmeans_fit] Invalid model_id=", model_id
            return
        end if

        call km%fit(X)
    end subroutine kmeans_fit

    subroutine kmeans_predict(model_id, X, n_rows, n_cols, labels) bind(C, name="kmeans_predict")
        integer(c_int64_t), value :: model_id, n_rows, n_cols
        real(c_double), intent(in) :: X(n_rows, n_cols)
        integer(c_int64_t), intent(out) :: labels(n_rows, 1)

        type(kmeans), pointer :: km
        km => get_model_by_id(model_id)

        if (.not. associated(km)) then
            print *, "[kmeans_predict] Invalid model_id=", model_id
            labels(:,:) = -1
            return
        end if
        labels = km%predict(X)
    end subroutine kmeans_predict

    function kmeans_score(model_id, X, n_rows, n_cols) result(score) bind(C, name="kmeans_score")
        integer(c_int64_t), value     :: model_id, n_rows, n_cols
        real(c_double), intent(in) :: X(n_rows, n_cols)
        real(c_double)             :: score

        type(kmeans), pointer :: km
        km => get_model_by_id(model_id)

        if (.not. associated(km)) then
            print *, "[kmeans_score] Invalid model_id=", model_id
            score = huge(0_r64)
            return
        end if

        score = km%score(X)
    end function kmeans_score

    function get_model_by_id(model_id) result(km)
        type(kmeans), pointer :: km
        integer(c_int64_t), intent(in) :: model_id

        nullify(km)

        if (.not. initialized) return
        if (model_id < 1 .or. model_id > size(kmeans_pool)) return
        if (.not. kmeans_used(model_id)) return

        km => kmeans_pool(model_id)
    end function get_model_by_id

    subroutine kmeans_free(model_id) bind(C, name="kmeans_free")
        integer(c_int64_t), value :: model_id

        if (.not. initialized) return
        if (model_id < 1 .or. model_id > size(kmeans_pool)) return
        if (.not. kmeans_used(model_id)) return

        kmeans_pool(model_id) = new_kmeans()
        kmeans_used(model_id) = .false.
    end subroutine kmeans_free

    subroutine kmeans_dump(model_id, file_name, n) bind(C, name="kmeans_dump")
        integer(c_int64_t), value :: model_id
        character(kind=c_char), intent(in) :: file_name(*)
        integer(c_size_t), value :: n

        type(kmeans), pointer :: km
        character(:), allocatable :: file_name_  ! Fortran文字列

        if (.not. initialized) return
        if (model_id < 1 .or. model_id > size(kmeans_pool)) return
        if (.not. kmeans_used(model_id)) return

        call c_chars_n_to_fstring(file_name, n, file_name_)

        km => kmeans_pool(model_id)
        call km%dump(file_name_)
    end subroutine kmeans_dump

    function kmeans_load(file_name, n) result(model_id) bind(C, name="kmeans_load")
        character(kind=c_char), intent(in) :: file_name(*)
        integer(c_size_t), value :: n
        integer(c_int64_t) :: model_id

        type(kmeans), pointer :: km
        character(:), allocatable :: file_name_  ! Fortran文字列

        call c_chars_n_to_fstring(file_name, n, file_name_)

        ! Pre-check existence to avoid I/O runtime errors; report via last_error
        last_error_code = 0_i64
        call check_file_exists( &
            path=file_name_,     &
            file=__FILE__,       &
            class_name="kmeans", &
            func_name="load",    &
            fatal=.false.)
        if (last_error_code /= 0_i64) then
            model_id = 0_c_int64_t
            return
        end if

        ! Create a fresh model slot (fatal=.false. so errors are reported via last_error)
        model_id = kmeans_create_model(2_i64, 1_i64, "kmeans++", 1_i64, 0.0001_r64, &
            1_i64, 1_i64, .false., 1_i64)
        km => kmeans_pool(model_id)
        call km%load(file_name_)

        ! If load recorded an error, free the slot and return (Python側が拾う)
        if (last_error_code /= 0_i64) then
            call kmeans_free(model_id)
        end if
    end function kmeans_load




end module mod_kmeans_binding
