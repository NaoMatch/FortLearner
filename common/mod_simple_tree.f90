module mod_simple_tree
    use mod_const
    use iso_c_binding
    implicit none

    Interface
        function myfunc(val, n) & 
            Bind(C,Name='myfunc')
            Import
            Real(c_int64_t) :: myfunc
            integer(c_int64_t), value :: val, n
        end function myfunc
    end interface 

    type simple_node
        integer(kind=8) :: val

        type(simple_node), pointer :: node_l => null()
        type(simple_node), pointer :: node_r => null()
    end type simple_node

    interface simple_node
        module procedure :: new_simple_node
    end interface simple_node
    
contains

    function new_simple_node()
        implicit none
        type(simple_node) :: new_simple_node
    end function new_simple_node


    recursive subroutine construct(node, vector, n_samples)
        implicit none
        type(simple_node) :: node
        real(kind=8) :: vector(n_samples)
        integer(kind=8) :: n_samples

        integer(kind=8) :: mid, n_samples_l, n_samples_r
        type(simple_node), target :: node_l, node_r

        if (n_samples == 1_8) then
            node%val = vector(1)
            return
        end if

        if (n_samples == 2_8) then
            mid = 1
            node%val = vector(mid)
            n_samples_l = 0
            n_samples_r = 1
            allocate(node%node_r)
            call construct(node%node_r, [vector(2)], n_samples_r)
        else
            mid = ishft(1_8+n_samples,-1)
            node%val = vector(mid)

            n_samples_l = mid-1
            n_samples_r = n_samples-(mid)

            if (n_samples_l >= 1_8) then
                allocate(node%node_l)
                call construct(node%node_l, vector(1:mid), n_samples_l)
            end if

            if (n_samples_r >= 1_8) then
                allocate(node%node_r)
                call construct(node%node_r, vector(mid+1:), n_samples_r)
            end if
        end if
    end subroutine construct


    recursive subroutine extract_ordered_vector(nodes, ordered_vector, depth)
        implicit none
        type(simple_node) :: nodes(:)
        type(simple_node), allocatable :: chiled_nodes(:)
        integer(kind=8), allocatable :: ordered_vector(:)
        ! logical(kind=8), allocatable :: is_leaf(:)

        integer(kind=8) :: i, depth, depth_

        if (depth > 0) then
            if (size(nodes) == 0) then
                return
            end if
        end if
        allocate(chiled_nodes(0))
        do i=1, size(nodes), 1
            ordered_vector = [ordered_vector, nodes(i)%val]
            
            ! if (associated(nodes(i)%node_l) .or. associated(nodes(i)%node_r)) then
            !     is_leaf = [is_leaf, f_]
            ! end if
            
            if (associated(nodes(i)%node_l)) then
                chiled_nodes = [chiled_nodes, nodes(i)%node_l]
            end if
            
            if (associated(nodes(i)%node_r)) then
                chiled_nodes = [chiled_nodes, nodes(i)%node_r]
            end if            
        end do
        
        depth_ = depth + 1
        call extract_ordered_vector(chiled_nodes, ordered_vector, depth_)
    end subroutine extract_ordered_vector



end module mod_simple_tree