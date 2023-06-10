module mod_forest_jitter
    use mod_decision_tree
    use mod_hash
    use mod_woodworking_tools
    implicit none

    type forest_jitter
    contains
        procedure :: jit => jit_forest_jitter
    end type forest_jitter

    interface forest_jitter
        procedure :: new_forest_jitter
    end interface forest_jitter
    
contains
    function new_forest_jitter() result(obj)
        type(forest_jitter) :: obj
    end function new_forest_jitter

    subroutine jit_forest_jitter(this, trees)
        implicit none
        class(forest_jitter) :: this
        type(decision_tree_regressor) :: trees(:)

        integer(kind=8) :: n_trees, t, id, n_nodes, i, d, n, unit
        integer(kind=8) :: node_idx, depth, max_depth, n_nodes_min, n_nodes_max, n_max_nodes
        integer(kind=8), allocatable :: hashes(:)
        type(node_axis), target, allocatable :: root_nodes(:)
        type(node_axis), pointer :: root_node_ptr
        type(node_axis_ptr), allocatable :: selected_node_ptrs(:)
        character(:), allocatable :: filename, cmd, func_name
        integer(c_int), parameter :: rtld_lazy=1 ! value extracte from the C header file
        func_name = "predict_"

        ! print*, "Set Hash"
        n_trees = size(trees)
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

        n_max_nodes = 2**max_depth
        ! print*, "Align the depth of trees", n_trees, n_max_nodes
        do t = 1, n_trees, 1
            root_node_ptr => root_nodes(t)

            if (allocated(trees(t)%features)) deallocate(trees(t)%features)
            if (allocated(trees(t)%thresholds)) deallocate(trees(t)%thresholds)
            if (allocated(trees(t)%responses)) deallocate(trees(t)%responses)
    
            allocate(trees(t)%features(0))
            allocate(trees(t)%thresholds(0))
            allocate(trees(t)%responses(n_max_nodes, 1))
    
            trees(t)%features = [trees(t)%features, root_node_ptr%feature_id_]
            trees(t)%thresholds = [trees(t)%thresholds, root_node_ptr%threshold_]
    
            i=1
            do d=1, max_depth, 1
                allocate(selected_node_ptrs(0))
                call extract_specific_depth_node_ptrs_axis_for_jit(root_node_ptr, d, selected_node_ptrs)
                do n=1, size(selected_node_ptrs), 1
                    if (.not. allocated(selected_node_ptrs(n)%node_ptr%node_l)) then
                        call clone_nodes(selected_node_ptrs(n)%node_ptr)
                    end if
                    trees(t)%features = [trees(t)%features, selected_node_ptrs(n)%node_ptr%feature_id_]
                    trees(t)%thresholds = [trees(t)%thresholds, selected_node_ptrs(n)%node_ptr%threshold_]
                    if (d == max_depth) then
                        trees(t)%responses(i,:) = selected_node_ptrs(n)%node_ptr%response(:)
                        i = i + 1
                    end if
                end do
                deallocate(selected_node_ptrs)
            end do

            trees(t)%features = pack(trees(t)%features, trees(t)%features/=-2)
            trees(t)%thresholds = pack(trees(t)%thresholds, trees(t)%features/=-2)
    
            allocate(selected_node_ptrs(0))
            call extract_specific_depth_node_ptrs_axis_for_jit(root_node_ptr, max_depth, selected_node_ptrs)
            do n=1, size(selected_node_ptrs), 1
                deallocate(selected_node_ptrs(n)%node_ptr%node_l)
                deallocate(selected_node_ptrs(n)%node_ptr%node_r)
            end do        
            deallocate(selected_node_ptrs)
        end do


        ! print*, "Check Max Depth and #Nodes after ALIGNMENT"
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
        
        filename = "random_forest_regressor_" //num2char(id)
        ! print*, "filename: ", filename
        open(newunit = unit, file = filename//".f90", status = 'replace')
        write(unit,"(a)") "module "//filename
        write(unit,"(a)") "    use, intrinsic :: iso_c_binding"
        write(unit,"(a)") "contains"

        write(unit,"(a)") "    subroutine predict_(event, features, thresholds, responses, res) bind(c, name='predict_')"
        write(unit,"(a)") "        implicit none"
        write(unit,"(a)") "        real(kind=8), intent(in) :: event(:)"
        write(unit,"(a)") "        integer(kind=8), intent(in) :: features(:)"
        write(unit,"(a)") "        real(kind=8), intent(in) :: thresholds(:)"
        write(unit,"(a)") "        real(kind=8), intent(in) :: responses(:,:)"
        write(unit,"(a)") "        real(kind=8), intent(out) :: res(:)"
        write(unit,"(a)") "        integer(kind=8) :: idx, flg"

        write(unit,"(a)") "        idx = 1"    
        do i=1, max_depth, 1
            write(unit,"(a)") "        idx = 2_8*idx + merge(1_8, 0_8, event(features(idx)) > thresholds(idx))"
        end do
        write(unit,"(a)") "        res = responses(idx-"//num2char(2**(max_depth)-1)//",:)"    
        write(unit,"(a)") "    end subroutine predict_"
        write(unit,"(a)") "end module "//filename
        close(unit)    


        cmd = "gfortran -O2 -fbounds-check -c   " // filename // ".f90"
        call system(cmd)
        cmd = "gfortran -O2 -fbounds-check -shared   -o " // filename // ".so " // filename // ".o"
        call system(cmd)

        do t=1, n_trees, 1
            trees(t)%handle=dlopen("./"// filename //".so"//c_null_char, RTLD_LAZY)
            if (.not. c_associated(trees(t)%handle))then
                print*, 'Unable to load DLL ./'// filename // ".f90"
                stop
            end if
        end do

        do t=1, n_trees, 1
            trees(t)%proc_addr_branchless=dlsym(trees(t)%handle, func_name//c_null_char)
            if (.not. c_associated(trees(t)%proc_addr_branchless))then
                write(*,*) 'Unable to load the procedure '//func_name
                stop
            end if
        end do
    end subroutine jit_forest_jitter


end module mod_forest_jitter