module mod_balltree
    !$ use omp_lib
    use mod_const
    use mod_common
    use mod_linalg
    use mod_timer
    implicit none

    type ball
        integer(kind=8) :: idx !< node index
        logical(kind=4) :: is_root=f_ !< is root node or not
        logical(kind=4) :: is_leaf=f_ !< is leaf node or not
        integer(kind=8) :: depth !< node depth
        integer(kind=8) :: n_samples !< number of samples
        integer(kind=8) :: n_columns !< number of columns
        integer(kind=8) :: min_samples_in_leaf !< minimum number of samples in leaf node

        integer(kind=8), ALLOCATABLE :: indices(:) !< sample point indices
        integer(kind=8) :: pivot

        ! Ball Info
        real(kind=8) :: radius !< ball radius
        real(kind=8) :: radius_sq !< ball radius square
        real(kind=8), allocatable :: center(:) !< ball center coordinate

        ! Additional Info
        real(kind=8), allocatable :: x_sq_sum(:)

        ! Parent and children node pointers
        type(ball), pointer :: ball_p_ptr => null() !< pointer to parent ball
        type(ball), pointer :: ball_l_ptr => null() !< pointer to child left ball
        type(ball), pointer :: ball_r_ptr => null() !< pointer to child right ball
    contains
        procedure :: init => init_ball
        procedure :: info => info_ball
        procedure :: split => split_ball
    end type ball

    type balltree
        integer(kind=8) :: n_samples, n_columns
    
        type(ball), pointer :: root_ball_ptr
        integer(kind=8)  :: min_samples_in_leaf=32_8

        integer(kind=8), allocatable :: x_sq_sum(:)
    contains
        procedure :: build => build_ball_tree
        procedure :: build_ball_tree_rec
    end type balltree

    interface balltree
        module procedure :: new_balltree
    end interface balltree


contains

    ! ----------------------------------------------------------------------------------
    ! For Ball Tree
    ! ----------------------------------------------------------------------------------

    function new_balltree(min_samples_in_leaf)
        implicit none
        type(balltree) :: new_balltree
        integer(kind=8), optional :: min_samples_in_leaf
        if ( present(min_samples_in_leaf) ) new_balltree%min_samples_in_leaf = min_samples_in_leaf
    end function new_balltree


    subroutine build_ball_tree(this, x)
        implicit none
        class(balltree) :: this
        real(kind=8), target, intent(in) :: x(:,:)

        real(kind=8), pointer :: x_ptr(:,:)
        integer(kind=8) :: x_shape(2), ball_idx
        type(ball), target :: root_ball
        integer(kind=8), allocatable :: times(:)

        allocate(times(6))
        times(:) = 0

        x_ptr => x
        x_shape(:) = shape(x)
        this%n_samples = x_shape(1)
        this%n_columns = x_shape(2)

        call root_ball%init(t_, f_, 0_8, this%n_samples, this%n_columns, this%min_samples_in_leaf)
        this%root_ball_ptr =>  root_ball

        ball_idx = 0_8
        call this%build_ball_tree_rec(this%root_ball_ptr, x_ptr, ball_idx, times)

        ! print*, '============================================================='
        ! print*, "Calculate Centroid    : ", times(1)
        ! print*, "Calculate Radius      : ", times(2)
        ! print*, "Get Most Spread Dim   : ", times(3)
        ! print*, "Get Pivot Position    : ", times(4)
        ! print*, "Initialize Child Balls: ", times(5)
        ! print*, "Allocate Indices      : ", times(6)
    end subroutine build_ball_tree


    subroutine build_ball_tree_rec(this, ball_ptr, x_ptr, ball_idx, times)
        implicit none
        class(balltree) :: this
        type(ball), pointer, intent(inout) :: ball_ptr
        real(kind=8), pointer, intent(in)  :: x_ptr(:,:)
        integer(kind=8), intent(inout)     :: ball_idx
        integer(kind=8), intent(inout)     :: times(:)

        ball_ptr%idx = ball_idx

        call ball_ptr%split(x_ptr, times)

        if (this%min_samples_in_leaf > ball_ptr%n_samples .or. ball_ptr%radius .eq. 0d0) then
            ball_ptr%is_leaf = t_
            return
        end if

        ball_idx = ball_idx + 1
        call this%build_ball_tree_rec(ball_ptr%ball_l_ptr, x_ptr, ball_idx, times)

        ball_idx = ball_idx + 1
        call this%build_ball_tree_rec(ball_ptr%ball_r_ptr, x_ptr, ball_idx, times)
    end subroutine build_ball_tree_rec


    ! ----------------------------------------------------------------------------------
    ! For Ball
    ! ----------------------------------------------------------------------------------

    subroutine init_ball(this, is_root, is_leaf, depth, n_samples, n_columns, min_samples_in_leaf, parent_ball)
        implicit none
        class(ball) :: this
        logical(kind=4), intent(in) :: is_root, is_leaf
        integer(kind=8), intent(in) :: depth, n_samples, n_columns, min_samples_in_leaf
        type(ball), intent(in), optional, target :: parent_ball

        integer(kind=8) :: n

        this%is_root = is_root
        this%is_leaf = is_leaf
        this%depth = depth
        this%n_samples = n_samples
        this%n_columns = n_columns
        this%min_samples_in_leaf = min_samples_in_leaf

        this%radius = 0d0
        call ifdealloc(this%center)
        allocate(this%center(this%n_columns))
        this%center(:) = huge(0d0)

        call ifdealloc(this%indices)
        allocate(this%indices(this%n_samples))

        if (ASSOCIATED(this%ball_p_ptr)) nullify(this%ball_p_ptr)
        if (ASSOCIATED(this%ball_l_ptr)) nullify(this%ball_l_ptr)
        if (ASSOCIATED(this%ball_r_ptr)) nullify(this%ball_r_ptr)

        if (present(parent_ball)) then
            if (ASSOCIATED(this%ball_p_ptr)) deallocate(this%ball_p_ptr)
            allocate(this%ball_p_ptr)
            this%ball_p_ptr => parent_ball
        end if

        if (is_root) then
            do n=1, this%n_samples, 1
                this%indices(n) = n
            end do
        end if
    end subroutine init_ball


    !> Print Node information
    subroutine info_ball(this)
        implicit none
        class(ball) :: this
        integer(kind=8) :: fin

        print*, '============================================================='
        print*, '============================================================='
        print*, "ball%is_root                 : ", this%is_root
        print*, "ball%is_leaf                 : ", this%is_leaf
        print*, "ball%idx                     : ", this%idx
        print*, "ball%depth                   : ", this%depth
        print*, "ball%n_samples               : ", this%n_samples
        print*, "ball%n_columns               : ", this%n_columns
        print*, "ball%min_samples_in_leaf     : ", this%min_samples_in_leaf

        print*, "ball%radius                  : ", this%radius
        if (allocated(this%center)) then
            fin = minval((/5, size(this%center)/))
            print*, "this%center                 : ", this%center(1:fin)
        end if

        print*, "allocated(ball%indices)      : ", allocated(this%indices)
        if (allocated(this%indices)) then
            fin = minval((/5, size(this%indices)/))
            print*, "this%indices                 : ", this%indices(1:fin)
        end if
        print*, "associated(ball%ball_p)      : ", associated(this%ball_p_ptr)
        print*, "associated(ball%ball_l)      : ", associated(this%ball_l_ptr)
        print*, "associated(ball%ball_r)      : ", associated(this%ball_r_ptr)
    end subroutine info_ball


    subroutine split_ball(this, x_ptr, times)
        implicit none
        class(ball) :: this
        real(kind=8), pointer, intent(in) :: x_ptr(:,:)
        integer(kind=8), intent(inout) :: times(:)

        integer(kind=8) :: n, f, idx, p_idx_1st, p_idx_2nd
        real(kind=8), allocatable :: distance_from_center(:), tmp_vec(:), tmp_X(:,:), distance_from_center_base(:)
        real(kind=8), allocatable :: distance_from_1st(:), distance_from_2nd(:)
        real(kind=8) :: center_sq_sum, p_1st_sq_sum, p_2nd_sq_sum

        integer(kind=8) :: flg, cnt_l, cnt_r, max_spread_dim 
        integer(kind=8), ALLOCATABLE :: which_side(:)
        real(kind=8), ALLOCATABLE    :: min_vals(:), max_vals(:), center(:)
        real(kind=8) :: tmp_min, tmp_max, val

        integer(kind=8) :: date_value1(8), date_value2(8)

        allocate(distance_from_center(this%n_samples), tmp_vec(this%n_samples), tmp_X(this%n_samples, this%n_columns))
        allocate(distance_from_center_base(this%n_samples))
        distance_from_center_base(:) = 0d0

        ! Calculate Centroid and Square Sum of Points ----------------------------------
        ! call date_and_time(values=date_value1)
        allocate( center(this%n_columns) )
        center(:) = 0d0
        !$omp parallel num_threads(2)
        !$omp do private(n, idx) reduction(+:center)
        do n=1, this%n_samples, 1
            idx = this%indices(n)
            center(:) = center(:) + x_ptr(idx,:)
            distance_from_center_base(n) = distance_from_center_base(n) + sum(x_ptr(idx,:)**2d0)
        end do
        !$omp end do
        !$omp end parallel
        this%center(:) = center(:)
        distance_from_center(:) = distance_from_center_base(:)
        this%center(:) = this%center(:) / dble(this%n_samples)
        ! call date_and_time(values=date_value2)
        ! ! times(1) = times(1) + time_diff(date_value1, date_value2)

        ! Choose Farthest Point From Centroid and Set Ball Radius
        ! call date_and_time(values=date_value1)        
        center_sq_sum = sum( this%center(:)**2d0 )
        call multi_mat_vec(x_ptr(this%indices(:),:), this%center, &
            tmp_vec, this%n_samples, this%n_columns, f_)
        distance_from_center(:) = distance_from_center(:) + center_sq_sum &
            -2d0 * tmp_vec(:)
        this%radius_sq = maxval(distance_from_center)
        this%radius = sqrt(this%radius_sq)
        ! call date_and_time(values=date_value2)
        ! ! times(2) = times(2) + time_diff(date_value1, date_value2)

        ! Fast Return - All points are dullicated.
        if ( this%radius_sq .eq. 0d0 ) return

        ! call date_and_time(values=date_value1)        
        ! Get Most Spread Dimension
        allocate( min_vals(this%n_columns) )
        allocate( max_vals(this%n_columns) )
        ! call get_matrix_minmax(min_vals, max_vals, transpose(x_ptr), this%indices, this%n_samples, &
        !     size(x_ptr, dim=1)+0_8, this%n_columns)
        !$omp parallel num_threads(2)
        !$omp do private(f, n, tmp_min, tmp_max, idx, val)
        do f=1, this%n_columns, 1
            tmp_min =   huge(0d0)
            tmp_max = - huge(0d0)
            do n=1, this%n_samples, 1
                idx = this%indices(n)
                val = x_ptr(idx,f)
                tmp_min = minval((/tmp_min, val/))
                tmp_max = maxval((/tmp_max, val/))
            end do
            min_vals(f) = tmp_min
            max_vals(f) = tmp_max
        end do
        !$omp end do
        !$omp end parallel
        max_spread_dim = maxloc( abs( max_vals - min_vals ), dim=1)
        ! call date_and_time(values=date_value2)
        ! ! times(3) = times(3) + time_diff(date_value1, date_value2)

        ! Get Pivot
        ! call date_and_time(values=date_value1)        
        this%pivot = this%n_samples / 2_8
        do n=1, this%n_samples, 1
            idx = this%indices(n)
            tmp_vec(n) = x_ptr(idx, max_spread_dim)
        end do
        call quick_argselect(tmp_vec, this%indices, this%n_samples, this%pivot)
        cnt_l = this%pivot
        cnt_r = this%n_samples - cnt_l
        ! call date_and_time(values=date_value2)
        ! ! times(4) = times(4) + time_diff(date_value1, date_value2)

        ! call date_and_time(values=date_value1)        
        allocate(this%ball_l_ptr)
        call this%ball_l_ptr%init(f_, f_, this%depth+1, &
            cnt_l, this%n_columns, this%min_samples_in_leaf, this)

        allocate(this%ball_r_ptr)
        call this%ball_r_ptr%init(f_, f_, this%depth+1, &
            cnt_r, this%n_columns, this%min_samples_in_leaf, this)
        ! call date_and_time(values=date_value2)
        ! ! times(5) = times(5) + time_diff(date_value1, date_value2)

        ! call date_and_time(values=date_value1)        
        cnt_l = 1
        do n=1, this%pivot, 1
            idx = this%indices(n)
            this%ball_l_ptr%indices(cnt_l) = idx
            cnt_l = cnt_l + 1
        end do

        cnt_r = 1
        do n=this%pivot+1, this%n_samples, 1
            idx = this%indices(n)
            this%ball_r_ptr%indices(cnt_r) = idx
            cnt_r = cnt_r + 1
        end do
        ! call date_and_time(values=date_value2)
        ! ! times(6) = times(6) + time_diff(date_value1, date_value2)
    end subroutine split_ball



    ! ! Re-arrange elements of POINTS into a binary ball tree.
    ! RECURSIVE SUBROUTINE BUILD_TREE(POINTS, SQ_SUMS, RADII, ORDER,&
    !     ROOT, LEAF_SIZE, COMPUTED_SQ_SUMS)
    !     REAL(KIND=REAL64),   INTENT(INOUT), DIMENSION(:,:) :: POINTS
    !     REAL(KIND=REAL64),   INTENT(OUT),   DIMENSION(:) :: SQ_SUMS
    !     REAL(KIND=REAL64),   INTENT(OUT),   DIMENSION(:) :: RADII
    !     INTEGER(KIND=INT64), INTENT(INOUT), DIMENSION(:) :: ORDER
    !     INTEGER(KIND=INT64), INTENT(IN), OPTIONAL :: ROOT, LEAF_SIZE
    !     LOGICAL,             INTENT(IN), OPTIONAL :: COMPUTED_SQ_SUMS
    !     ! Local variables
    !     INTEGER(KIND=INT64) :: CENTER_IDX, MID, I, J, LS
    !     REAL(KIND=REAL64), DIMENSION(SIZE(POINTS,1)) :: PT
    !     REAL(KIND=REAL64), DIMENSION(SIZE(ORDER)) :: SQ_DISTS
    !     REAL(KIND=REAL64) :: MAX_SQ_DIST, SQ_DIST, SHIFT
    !     EXTERNAL :: DGEMM

    !     ! Set the index of the 'root' of the tree.
    !         IF (PRESENT(ROOT)) THEN ; CENTER_IDX = ROOT
    !         ELSE
    !             ! 1) Compute distances between first point (random) and all others.
    !             ! 2) Pick the furthest point (on conv hull) from first as the center node.
    !             J = ORDER(1)
    !             PT(:) = POINTS(:,J)
    !             SQ_DISTS(1) = 0.0_REAL64
    !             !$OMP PARALLEL DO
    !             DO I = 2, SIZE(ORDER)
    !                 SQ_DISTS(I) = SQ_SUMS(J) + SQ_SUMS(ORDER(I)) - &
    !                     2 * DOT_PRODUCT(POINTS(:,ORDER(I)), PT(:))
    !             END DO 
    !             !$OMP END PARALLEL DO
    !             CENTER_IDX = MAXLOC(SQ_DISTS(:),1)
    !             ! Now CENTER_IDX is the selected center for this node in tree.
    !         END IF

    !     ! Move the "center" to the first position.
    !         CALL SWAP_I64(ORDER(1), ORDER(CENTER_IDX))

    !     ! Measure squared distance beween "center" node and all other points.
    !         J = ORDER(1)
    !         PT(:) = POINTS(:,J) ! center
    !         SQ_DISTS(1) = 0.0_REAL64
    !         !$OMP PARALLEL DO
    !         DO I = 2, SIZE(ORDER)
    !             SQ_DISTS(I) = SQ_SUMS(J) + SQ_SUMS(ORDER(I)) - &
    !                 2 * DOT_PRODUCT(POINTS(:,ORDER(I)), PT(:))
    !         END DO
    !         !$OMP END PARALLEL DO

    !     ! Base case for recursion, once we have few enough points, exit.
    !         IF (SIZE(ORDER) .LE. LS) THEN
    !             RADII(ORDER(1)) = SQRT(MAXVAL(SQ_DISTS))
    !             IF (SIZE(ORDER) .GT. 1) RADII(ORDER(2:)) = 0.0_REAL64
    !             RETURN
    !         ELSE IF (SIZE(ORDER) .EQ. 2) THEN
    !             ! If the leaf size is 1 and there are only 2 elements, store
    !             ! the radius and exit (since there are no further steps.
    !             RADII(ORDER(1)) = SQRT(SQ_DISTS(2))
    !             RADII(ORDER(2)) = 0.0_REAL64
    !             RETURN
    !         END IF

    !     ! Rearrange "SQ_DISTS" about the median value.
    !         ! Compute the last index that will belong "inside" this node.
    !         MID = (SIZE(ORDER) + 2) / 2
    !         CALL ARGSELECT_R64(SQ_DISTS(2:), ORDER(2:), MID - 1)
    !         ! Now ORDER has been rearranged such that the median distance
    !         ! element of POINTS is at the median location.
    !         ! Identify the furthest point (must be in second half of list).
    !         I = MID + MAXLOC(SQ_DISTS(MID+1:),1)
    !         ! Store the "radius" of this ball, the furthest point.
    !         RADII(ORDER(1)) = SQRT(SQ_DISTS(I))
    !         ! Move the median point (furthest "interior") to the front (inner root).
    !         CALL SWAP_I64(ORDER(2), ORDER(MID))
    !         ! Move the furthest point into the spot after the median (outer root).
    !         CALL SWAP_I64(ORDER(MID+1), ORDER(I))

    !         !$OMP PARALLEL NUM_THREADS(2)
    !         !$OMP SECTIONS
    !         !$OMP SECTION
    !         ! Recurisively create this tree.
    !         !   build a tree with the root being the furthest from this center
    !         !   for the remaining "interior" points of this center node.
    !         CALL BUILD_TREE(POINTS, SQ_SUMS, RADII, ORDER(2:MID), 1_INT64, LS, .TRUE.)
    !         !$OMP SECTION
    !         !   build a tree with the root being the furthest from this center
    !         !   for the remaining "exterior" points of this center node.
    !         !   Only perform this operation if there are >0 points available.
    !         IF (MID < SIZE(ORDER)) &
    !                 CALL BUILD_TREE(POINTS, SQ_SUMS, RADII, &
    !                 ORDER(MID+1:), 1_INT64, LS, .TRUE.)
    !         !$OMP END SECTIONS
    !         !$OMP END PARALLEL
    ! END SUBROUTINE BUILD_TREE


end module mod_balltree

