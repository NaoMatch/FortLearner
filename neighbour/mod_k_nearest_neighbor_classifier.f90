module mod_k_nearest_neighbor_classifier
    use mod_hyperparameter
    use mod_brute_force_search
    use mod_kdtree
    use mod_balltree
    use mod_nearest_neighbour, only: neighbor_results
    use mod_common
    use mod_const
    implicit none

    type k_nearest_neighbor_classifier
        type(hparam_k_nearest_neighbor_classifier) :: hparam
        integer(kind=8), allocatable :: y(:,:)
        integer(kind=8) :: n_samples, n_columns, n_classes

        type(brute_force_search) :: bf
        type(kdtree) :: kd
        type(balltree) :: bt
    contains
        procedure :: fit => fit_k_nearest_neighbor_classifier
        procedure :: predict => predict_k_nearest_neighbor_classifier
    end type k_nearest_neighbor_classifier

    !> Construct New 'k_nearest_neighbor_classifier' object.
    interface k_nearest_neighbor_classifier
        module procedure :: new_k_nearest_neighbor_classifier
    end interface k_nearest_neighbor_classifier

contains

    function new_k_nearest_neighbor_classifier(n_neighbors, weight, algorithm, min_samples_in_leaf, split_algo, kernel) result(knn)
        implicit none
        type(k_nearest_neighbor_classifier) :: knn
        integer(kind=8), optional  :: n_neighbors
        character(len=*), optional :: weight
        character(len=*), optional :: algorithm
        integer(kind=8), optional  :: min_samples_in_leaf
        character(len=*), optional :: split_algo
        character(len=*), optional :: kernel
                
        if (present(n_neighbors))         knn%hparam%n_neighbors = n_neighbors
        if (present(weight))              knn%hparam%weight = weight
        if (present(algorithm))           knn%hparam%algorithm = algorithm
        if (present(min_samples_in_leaf)) knn%hparam%min_samples_in_leaf = min_samples_in_leaf
        if (present(split_algo))          knn%hparam%split_algo = split_algo
        if (present(kernel))              knn%hparam%kernel = kernel
    end function 


    subroutine fit_k_nearest_neighbor_classifier(this, x, y)
        implicit none
        class(k_nearest_neighbor_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)

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
        this%n_classes = size(y, dim=2)
        allocate(this%y, source=y)
    end subroutine fit_k_nearest_neighbor_classifier


    function predict_k_nearest_neighbor_classifier(this, x) result(pred)
        implicit none
        class(k_nearest_neighbor_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), allocatable :: pred(:,:)

        type(neighbor_results) :: neighbors
        integer(kind=8) :: n, n_samples, i, idx, cls_idx
        integer(kind=8), allocatable :: idxs(:)
        real(kind=8) :: mean_scale, coef_
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
        if (this%hparam%kernel=="linear") then
            ! skip
        elseif (this%hparam%kernel=="exponential") then
            coef_ = 1d0 / sqrt(2d0 * pi_)
            do n=1, n_samples, 1
                neighbors%distances(n)%dst = 2d0 * coef_ * ( exp(0.5d0) - exp(0.5d0 - neighbors%distances(n)%dst) )
            end do
        end if

        allocate(pred(n_samples, this%n_classes)); pred=0_8
        allocate(idxs(this%hparam%n_neighbors))
        if (this%hparam%weight == "uniform") then
            do n=1, n_samples, 1
                idxs = neighbors%indices(n)%idx(1:this%hparam%n_neighbors)
                cls_idx = maxloc(sum(this%y(idxs,:), dim=1), dim=1)
                pred(n, cls_idx) = 1
            end do
        elseif (this%hparam%weight == "distance") then
            allocate(tmp_pred(this%n_classes))
            allocate(dsts(this%hparam%n_neighbors))
            do n=1, n_samples, 1
                idxs = neighbors%indices(n)%idx(1:this%hparam%n_neighbors)
                dsts = neighbors%distances(n)%dst(1:this%hparam%n_neighbors)
                if (minval(dsts) <= 0d0) then
                    dsts = 0d0
                    dsts(1) = 1d0
                else
                    dsts = 1d0 / neighbors%distances(n)%dst(1:this%hparam%n_neighbors)
                    dsts = dsts / sum(dsts)
                end if
                tmp_pred = 0d0
                do i=1, this%hparam%n_neighbors, 1
                    idx = idxs(i)
                    tmp_pred(:) = tmp_pred(:) + this%y(idx,:) * dsts(i)
                end do
                cls_idx = maxloc(tmp_pred, dim=1)
                pred(n,cls_idx) = 1_8
            end do
        end if
    end function predict_k_nearest_neighbor_classifier








end module mod_k_nearest_neighbor_classifier