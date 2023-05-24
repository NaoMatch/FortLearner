program main_data_structure_union_find
    use mod_union_find
    implicit none

    integer(kind=8) :: n 
    type(union_find) :: uf

    n = 6
    uf = union_find(n)
    call uf%union(1_8, 2_8)
    print*, "uf%union(3_8, 4_8)"
    call uf%union(3_8, 4_8)
    print*, "uf%union(4_8, 5_8)"
    ! call uf%union(4_8, 5_8)
    print*, "uf%find(id=1_8): ", uf%find(id=1_8)
    print*, "uf%find(id=2_8): ", uf%find(id=2_8)
    print*, "uf%find(id=3_8): ", uf%find(id=3_8)
    print*, "uf%find(id=4_8): ", uf%find(id=4_8)

    print*, "uf%is_same_root(1_8, 2_8): ", uf%is_same_root(1_8, 2_8)
    print*, "uf%is_same_root(1_8, 3_8): ", uf%is_same_root(1_8, 3_8)
    
    print*, "uf%union(1_8, 4_8)"
    ! call uf%union(1_8, 4_8)
    call uf%union(4_8, 1_8)
    print*, "uf%find(id=1_8): ", uf%find(id=1_8)
    print*, "uf%find(id=2_8): ", uf%find(id=2_8)
    print*, "uf%find(id=3_8): ", uf%find(id=3_8)
    print*, "uf%find(id=4_8): ", uf%find(id=4_8)

    print*, "uf%is_same_root(1_8, 2_8): ", uf%is_same_root(1_8, 2_8)
    print*, "uf%is_same_root(1_8, 3_8): ", uf%is_same_root(1_8, 3_8)
    print*, uf%parents(:)
end program main_data_structure_union_find