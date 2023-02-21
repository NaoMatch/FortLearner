module mod_k_nearest_neighbor_regressor
    use mod_hyperparameter
    use mod_brute_force_search
    use mod_kdtree
    use mod_balltree
    use mod_nearest_neighbour, only: neighbor_results
    implicit none

    type k_nearest_neighbor_regressor
        type(hparam_k_nearest_neighbor_regressor) :: hparam
        real(kind=8), allocatable :: y(:,:)
        integer(kind=8) :: n_samples, n_columns, n_outputs

        type(brute_force_search) :: bf
        type(kdtree) :: kd
        type(balltree) :: bt
    contains
        procedure :: fit => fit_k_nearest_neighbor_regressor
        procedure :: predict => predict_k_nearest_neighbor_regressor
    end type k_nearest_neighbor_regressor

    !> Construct New 'k_nearest_neighbor_regressor' object.
    interface k_nearest_neighbor_regressor
        module procedure :: new_k_nearest_neighbor_regressor
    end interface k_nearest_neighbor_regressor

contains

    function new_k_nearest_neighbor_regressor(n_neighbors, weight, algorithm, min_samples_in_leaf, split_algo) result(knn)
        implicit none
        type(k_nearest_neighbor_regressor) :: knn
        integer(kind=8), optional  :: n_neighbors
        character(len=*), optional :: weight
        character(len=*), optional :: algorithm
        integer(kind=8), optional  :: min_samples_in_leaf
        character(len=*), optional :: split_algo
                
        if (present(n_neighbors))         knn%hparam%n_neighbors = n_neighbors
        if (present(weight))              knn%hparam%weight = weight
        if (present(algorithm))           knn%hparam%algorithm = algorithm
        if (present(min_samples_in_leaf)) knn%hparam%min_samples_in_leaf = min_samples_in_leaf
        if (present(split_algo))          knn%hparam%split_algo = split_algo
    end function 


    subroutine fit_k_nearest_neighbor_regressor(this, x, y)
        implicit none
        class(k_nearest_neighbor_regressor) :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), intent(in) :: y(:,:)

        if (this%hparam%algorithm == "brute_force") then
            this%bf = brute_force_search()
            call this%bf%build(x)
        elseif (this%hparam%algorithm == "kd_tree") then
            this%kd = kdtree(min_samples_in_leaf=this%hparam%min_samples_in_leaf)
            call this%kd%build(x)
        elseif (this%hparam%algorithm == "ball_tree") then
            this%bt = balltree(min_samples_in_leaf=this%hparam%min_samples_in_leaf, split_algo=this%hparam%split_algo)
            call this%bt%build(x)
        end if

        this%n_samples = size(x, dim=1)
        this%n_columns = size(x, dim=2)
        this%n_outputs = size(y, dim=2)
        allocate(this%y, source=y)
    end subroutine fit_k_nearest_neighbor_regressor


    function predict_k_nearest_neighbor_regressor(this, x) result(pred)
        implicit none
        class(k_nearest_neighbor_regressor) :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), allocatable :: pred(:,:)

        type(neighbor_results) :: neighbors
        integer(kind=8) :: n, n_samples, i, idx
        integer(kind=8), allocatable :: idxs(:)
        real(kind=8) :: mean_scale
        real(kind=8), allocatable :: tmp_pred(:)
        real(kind=8), allocatable :: dsts(:)

        if (this%hparam%algorithm == "brute_force") then
            neighbors = this%bf%query(q=x, n_neighbors=this%hparam%n_neighbors)
        elseif (this%hparam%algorithm == "kd_tree") then
            neighbors = this%kd%query(q=x, n_neighbors=this%hparam%n_neighbors)
        elseif (this%hparam%algorithm == "ball_tree") then
            neighbors = this%bt%query(q=x, n_neighbors=this%hparam%n_neighbors)
        else
            stop "NotImplemented Error."
        end if

        n_samples = size(x, dim=1)
        allocate(pred(n_samples, this%n_outputs))
        allocate(idxs(this%hparam%n_neighbors))
        mean_scale = 1d0 / dble(this%hparam%n_neighbors)
        if (this%hparam%weight == "uniform") then
            do n=1, n_samples, 1
                idxs = neighbors%indices(n)%idx(1:this%hparam%n_neighbors)
                pred(n,:) = sum(this%y(idxs,:), dim=1) * mean_scale
            end do
        elseif (this%hparam%weight == "distance") then
            allocate(tmp_pred(this%n_outputs))
            allocate(dsts(this%hparam%n_neighbors))
            do n=1, n_samples, 1
                idxs = neighbors%indices(n)%idx(1:this%hparam%n_neighbors)
                dsts = neighbors%distances(n)%dst(1:this%hparam%n_neighbors)
                if (minval(dsts) <= 0d0) then
                    dsts = 0d0
                    dsts(1) = 1d0
                else
                    dsts = 1d0 / abs(neighbors%distances(n)%dst(1:this%hparam%n_neighbors))
                    dsts = dsts / sum(dsts)
                end if
                tmp_pred = 0d0
                do i=1, this%hparam%n_neighbors, 1
                    idx = idxs(i)
                    tmp_pred(:) = tmp_pred(:) + this%y(idx,:) * dsts(i)
                end do
                pred(n,:) = tmp_pred(:)
            end do
        end if
    end function predict_k_nearest_neighbor_regressor








end module mod_k_nearest_neighbor_regressor