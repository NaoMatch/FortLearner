module mod_nearest_neighbour
    use mod_const
    implicit none

    type index
        integer(kind=8), allocatable :: idx(:) !< indices from query to N-th nearest points
    end type index

    type distance
        real(kind=8), allocatable :: dst(:) !< distances from query to N-th nearest points
    end type distance

    !> Neghbor Informations.
    !> indices: sample indices of domain
    !> distaneces: distances between each query from domain
    type neighbor_results
        type(index),    ALLOCATABLE :: indices(:)
        type(distance), ALLOCATABLE :: distances(:)
    end type neighbor_results

    !> Common members of 'node' for 'kdtree', 'ball' for 'balltree'.
    type base_node_for_nearest_neighbor
        integer(kind=8) :: idx !< node index
        logical(kind=4) :: is_root=f_ !< is root node or not
        logical(kind=4) :: is_leaf=f_ !< is leaf node or not
        logical(kind=4) :: is_left=f_ !< is left child node or not
        integer(kind=8) :: depth !< node depth
        integer(kind=8) :: n_samples !< number of samples
        integer(kind=8) :: n_samples_ !< number of samples including split point indices
        integer(kind=8) :: n_columns !< number of samples
        integer(kind=8) :: min_samples_in_leaf !< minimum number of samples in leaf node

        integer(kind=8), ALLOCATABLE :: indices(:) !< sample point indices
        integer(kind=8), ALLOCATABLE :: indices_(:) !< include split point indices
    end type base_node_for_nearest_neighbor




end module mod_nearest_neighbour
