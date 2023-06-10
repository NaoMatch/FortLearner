module mod_forest_packing_
    use iso_c_binding
    use mod_common
    use mod_decision_tree
    use mod_hash
    use mod_woodworking_tools
    implicit none
    
    type forest_packing
        integer(kind=8) :: n_trees=100_8
        integer(kind=8) :: max_depth=100_8
        integer(kind=8), allocatable :: features_pack(:)
        real(kind=8), allocatable :: thresholds_pack(:)
        real(kind=8), allocatable :: responses_pack(:,:)

        logical(kind=4) :: is_packed=f_
        type(c_funptr) :: proc_addr_branchless = C_NULL_FUNPTR
        type(c_ptr) :: handle
    contains
        procedure :: packing
        procedure :: predict_
    end type forest_packing
    
    !> An interface to create new 'forest_packing'
    interface forest_packing
        procedure :: new_forest_packing
    end interface forest_packing

contains
    function new_forest_packing() result(obj)
        type(forest_packing) :: obj
        obj%is_packed = f_
    end function new_forest_packing

    subroutine packing(this, trees)
        class(forest_packing) :: this
        type(decision_tree_regressor) :: trees(:)

        integer(kind=8) :: n_trees, t, id, n_nodes, i, d, n, unit, tmp
        integer(kind=8) :: node_idx, depth, max_depth, n_nodes_min, n_nodes_max, n_max_nodes
        integer(kind=8), allocatable :: hashes(:)
        type(node_axis), target, allocatable :: root_nodes(:)
        type(node_axis), pointer :: root_node_ptr
        type(node_axis_ptr), allocatable :: selected_node_ptrs(:)
        character(:), allocatable :: filename, cmd, func_name, lines, n_trees_char, insert
        integer(c_int), parameter :: rtld_lazy=1 ! value extracte from the C header file
        func_name = "predict_"


        ! print*, "Set Hash"
        n_trees = size(trees)
        n_trees_char = num2char(n_trees)
        allocate(root_nodes(n_trees), hashes(n_trees))
        do t=1, n_trees, 1
            n_nodes = size(trees(t)%results%split_features_)
            hashes(t) = abs(one_at_a_time_hash(trees(t)%results%split_features_, n_nodes))
        end do
        id = abs(one_at_a_time_hash(hashes, n_trees))

        ! print*, "Reconstructing Trees"
        do t=1, n_trees, 1
            node_idx = 0
            depth = 0
            call reconstruct_tree(root_nodes(t), &
                trees(t)%results, node_idx=node_idx, depth=depth)
        end do

        ! print*, "Check Max Depth and #Nodes before ALIGNMENT"
        max_depth = - huge(0_8)
        n_nodes_min = huge(0_8)
        n_nodes_max = -huge(0_8)
        do t=1, n_trees, 1
            depth = - huge(0_8)
            n_nodes = 0
            call check_max_depth(root_nodes(t), depth)
            root_node_ptr => root_nodes(t)
            call count_all_nodes(root_node_ptr, n_nodes, is_root=t_)

            max_depth = maxval([depth, max_depth])
            n_nodes_min = minval([n_nodes_min, n_nodes])
            n_nodes_max = maxval([n_nodes_min, n_nodes])
        end do
        ! print*, max_depth, n_nodes_min, n_nodes_max

        n_max_nodes = 2**(max_depth+1) - 1
        n_max_nodes = 2**(max_depth)
        ! print*, "Align the depth of trees", n_trees

        if (allocated(this%features_pack)) deallocate(this%features_pack)
        if (allocated(this%thresholds_pack)) deallocate(this%thresholds_pack)
        if (allocated(this%responses_pack)) deallocate(this%responses_pack)

        allocate(this%features_pack(0))
        allocate(this%thresholds_pack(0))
        allocate(this%responses_pack(n_max_nodes*n_trees, 1)); this%responses_pack=0d0

        ! Collect Root Node
        ! print*, "Collect Root Node"
        i=1
        do t=1, n_trees, 1
            root_node_ptr => root_nodes(t)
            this%features_pack = [this%features_pack, root_node_ptr%feature_id_]
            this%thresholds_pack = [this%thresholds_pack, root_node_ptr%threshold_]
            ! this%responses_pack(i,:) = root_node_ptr%response
            ! i = i + 1
        end do
        
        ! Internal and Leaf Node
        ! print*, "Internal and Leaf Node"
        do d=1, max_depth, 1
            do t=1, n_trees, 1
                allocate(selected_node_ptrs(0))
                root_node_ptr => root_nodes(t)
                call extract_specific_depth_node_ptrs_axis_for_jit(root_node_ptr, d, selected_node_ptrs)

                do n=1, size(selected_node_ptrs), 1
                    if (.not. allocated(selected_node_ptrs(n)%node_ptr%node_l)) then
                        call clone_nodes(selected_node_ptrs(n)%node_ptr)
                    end if
                    this%features_pack = [this%features_pack, selected_node_ptrs(n)%node_ptr%feature_id_]
                    this%thresholds_pack = [this%thresholds_pack, selected_node_ptrs(n)%node_ptr%threshold_]
                    ! this%responses_pack(i,:) = selected_node_ptrs(n)%node_ptr%response
                    ! i = i + 1
                    if (d==max_depth) then
                        this%responses_pack(i,:) = selected_node_ptrs(n)%node_ptr%response
                        i = i + 1
                    end if
                end do
                deallocate(selected_node_ptrs)
            end do
        end do
        ! print*, size(this%features_pack), size(this%features_pack)/n_trees, 2**(max_depth+1)-1, shape(this%responses_pack)
        ! print*, "features_pack: ", this%features_pack
        ! print*, "thresholds_pack: ", this%thresholds_pack
        ! print*, "responses_pack: ", this%responses_pack
        this%features_pack = pack(this%features_pack, this%features_pack/=-2)
        this%thresholds_pack = pack(this%thresholds_pack, this%features_pack/=-2)
        this%is_packed = t_
        this%n_trees = n_trees
        this%max_depth = max_depth
    end subroutine packing


    subroutine predict_(this, event, features, thresholds, responses, res)
        implicit none
        class(forest_packing) :: this
        real(kind=8), intent(in) :: event(:)
        integer(kind=8), intent(in) :: features(:)
        real(kind=8), intent(in) :: thresholds(:)
        real(kind=8), intent(in) :: responses(:,:)
        real(kind=8), intent(out) :: res(:)
        integer(kind=8), allocatable :: idxs(:)
        integer(kind=8) :: i, flg, d, sub_idx
        allocate(idxs(this%n_trees))
        idxs = (/(i, i=1, this%n_trees, 1)/)
        do d=1, this%max_depth, 1
            idxs = 2_8*idxs + this%n_trees-1 + merge(1_8, 0_8, event(features(idxs)) > thresholds(idxs))
        end do
        sub_idx = (2_8 ** this%max_depth - 1_8) * this%n_trees
        res = res + sum(responses(idxs-sub_idx,:), dim=1)
        ! stop 
    end subroutine predict_





end module mod_forest_packing_