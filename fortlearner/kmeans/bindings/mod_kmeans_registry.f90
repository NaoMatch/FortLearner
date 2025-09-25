module mod_kmeans_registry
    use :: iso_c_binding
    use :: mod_config, only: INITIAL_MODEL_POOL_CAPACITY, GROWTH_MODEL_POOL_CAPACITY
    use :: mod_kinds,  only: i64, r64
    use :: mod_kmeans
    use :: mod_error_manager
    use :: mod_c_to_f
    implicit none
    
    type(kmeans), allocatable, target :: kmeans_pool(:)
    logical, allocatable :: kmeans_used(:)
    logical, save :: initialized = .false.

    public :: kmeans_create_model, kmeans_pool, kmeans_used, initialized

contains

    subroutine extend_pool()
        implicit none

        integer :: old_n, new_n
        type(kmeans), allocatable, target :: new_pool(:)
        logical,      allocatable         :: new_used(:)

        old_n = size(kmeans_pool)

        ! 既に未使用スロットがあるなら拡張不要（呼び出し側で判定しているならこの early return は省略可）
        if (any(.not. kmeans_used)) return

        ! 容量計算：倍増（最低でも +GROWTH_MODEL_POOL_CAPACITY）
        new_n = old_n + GROWTH_MODEL_POOL_CAPACITY

        ! 新領域を確保
        allocate(new_pool(new_n))
        allocate(new_used(new_n))

        ! 既存の中身と使用フラグをコピー
        new_pool(1:old_n) = kmeans_pool
        new_used(1:old_n) = kmeans_used

        ! 追加領域は未使用に
        new_used(old_n+1:new_n) = .false.

        ! move_alloc で差し替え（コピー不要、O(1)）
        call move_alloc(new_pool, kmeans_pool)
        call move_alloc(new_used, kmeans_used)
    end subroutine extend_pool

    subroutine initialize_kmeans_registry()
        if (.not. initialized) then
            if (.not. allocated(kmeans_pool)) then
                allocate(kmeans_pool(INITIAL_MODEL_POOL_CAPACITY))
                allocate(kmeans_used(INITIAL_MODEL_POOL_CAPACITY))
            end if
            kmeans_used(:) = .false.
            initialized = .true.
        end if
    end subroutine initialize_kmeans_registry

    function kmeans_create_model(n_clusters, max_iter, init, n_init, tol, &
        random_state, num_threads, print_log, chunk_size) result(model_id)
        integer(i64), intent(in)     :: n_clusters
        integer(i64), intent(in)     :: max_iter
        character(len=*), intent(in) :: init
        integer(i64), intent(in)     :: n_init
        real(r64),    intent(in)     :: tol
        integer(i64), intent(in)     :: random_state
        integer(i64), intent(in)     :: num_threads
        logical, intent(in)          :: print_log
        integer(i64), intent(in)     :: chunk_size

        integer(c_long) :: model_id
        integer :: i

        call initialize_kmeans_registry()
        call extend_pool()

        do i = 1, size(kmeans_pool)
            if (.not. kmeans_used(i)) then

                if (chunk_size /= -1_i64) then
                    kmeans_pool(i) = new_kmeans(n_clusters, max_iter, init, n_init, tol, &
                        random_state, num_threads, print_log, chunk_size, fatal=.false.)
                else
                    kmeans_pool(i) = new_kmeans(n_clusters, max_iter, init, n_init, tol, &
                        random_state, num_threads, print_log, fatal=.false.)
                end if
                kmeans_used(i) = .true.
                model_id = i
                return
            end if
        end do
    end function kmeans_create_model

end module mod_kmeans_registry
