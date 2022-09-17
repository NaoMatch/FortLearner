program main_encoder
    use mod_encoder
    implicit none

    type(one_hot_encoder) :: oh_enc
    integer(kind=8), allocatable :: v1(:), x1(:,:)
    integer(kind=8), allocatable :: v2(:), x2(:,:)
    integer(kind=8), allocatable :: x_ohe(:,:)
    integer(kind=8), allocatable :: v1_inv(:), v2_inv(:)
    integer(kind=4) :: i


    print*, '*********************************************************************************************'
    print*, "Transform -> Vector: [1,2,3,4,5]"
    oh_enc = one_hot_encoder()
    allocate(v1(5))
    v1(:) = [1,2,3,4,5]
    x_ohe = oh_enc%transform(v1)
    do i=1, 5, 1
        print*, "      ", i, " :: ", x_ohe(i,:)
    end do
    print*, "---------------------------------------------------------------------------------------------"
    print*, "            uniq_vals: ",  oh_enc%original_vals(:)
    print*, ""
    print*, ""


    print*, '*********************************************************************************************'
    print*, "Transform -> Matrix: [[1],[2],[3],[4],[5]]"
    oh_enc = one_hot_encoder()
    allocate(x1(5,1))
    x1(:,1) = [1,2,3,4,5]
    x_ohe = oh_enc%transform(x1)
    do i=1, 5, 1
        print*, "      ", i, " :: ", x_ohe(i,:)
    end do
    print*, "---------------------------------------------------------------------------------------------"
    print*, "            uniq_vals: ",  oh_enc%original_vals(:)
    print*, ""
    print*, ""


    print*, '*********************************************************************************************'
    print*, "Transform -> Vector: [1,3,5,7,8]"
    oh_enc = one_hot_encoder()
    allocate(v2(5))
    v2(:) = [1,3,5,7,8]
    x_ohe = oh_enc%transform(v2)
    do i=1, 5, 1
        print*, "      ", i, " :: ", x_ohe(i,:)
    end do
    print*, "---------------------------------------------------------------------------------------------"
    print*, "            uniq_vals: ",  oh_enc%original_vals(:)
    print*, ""
    print*, ""


    print*, '*********************************************************************************************'
    print*, "Transform -> Matrix: [[1],[3],[5],[7],[8]]"
    oh_enc = one_hot_encoder()
    allocate(x2(5,1))
    x2(:,1) = [1,3,5,7,8]
    x_ohe = oh_enc%transform(x2)
    do i=1, 5, 1
        print*, "      ", i, " :: ", x_ohe(i,:)
    end do
    print*, "---------------------------------------------------------------------------------------------"
    print*, "            uniq_vals: ",  oh_enc%original_vals(:)
    print*, ""
    print*, ""


    print*, '*********************************************************************************************'
    print*, "Inverse Transform -> Vector: [1,2,3,4,5]"
    v1(:) = [1,2,3,4,5]
    x_ohe = oh_enc%transform(v1)
    v1_inv = oh_enc%inverse_transform(x_ohe)
    do i=1, 5, 1
        print*, "      ", i, " :: ", v1_inv(i), " :: ", x_ohe(i,:)
    end do


    print*, '*********************************************************************************************'
    print*, "Inverse Transform -> Vector: [1,3,5,7,8]"
    v2(:) = [1,3,5,7,8]
    x_ohe = oh_enc%transform(v2)
    v2_inv = oh_enc%inverse_transform(x_ohe)
    do i=1, 5, 1
        print*, "      ", i, " :: ", v2_inv(i), " :: ", x_ohe(i,:)
    end do


    
end program main_encoder