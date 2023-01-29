module mod_linear_svm
    use mod_const
    use mod_common
    use mod_common_type
    use mod_sort
    use mod_hyperparameter
    use mod_timer
    use mod_linalg
    use mod_hash_map
    implicit none

    !> Linear SVM Classifier, Not yet optimized.
    type linear_svm_classifier
        type(hparam_linear_svm_classifier) :: hparam
        real(kind=8), allocatable :: w_(:)
        real(kind=8), allocatable :: w0_
        integer(kind=8), allocatable :: n_iter_
    contains
        procedure :: fit => fit_linear_svm_classifier
        procedure :: fit_linear_svm_classifier_hard
        procedure :: predict => predict_linear_svm_classifier

        procedure :: transform_y
    end type linear_svm_classifier

    !> An interface to create new 'linear_svm_classifier'
    interface linear_svm_classifier
        procedure :: new_linear_svm_classifier
    end interface linear_svm_classifier

        
    type kernel_cache
        integer(kind=8) :: idx_hash = 0_8
        real(kind=8), allocatable :: values(:)
    end type kernel_cache

    type svm_cache
        integer(kind=8)                 :: n_caches
        integer(kind=8)                 :: n_avail
        integer(kind=8)                 :: n_used
        logical(kind=4), allocatable    :: is_used(:)
        integer(kind=8), allocatable    :: recency(:) ! 直近利用のループカウント
        type(kernel_cache), allocatable :: kernels(:)
    contains
        procedure :: query => query_svm_cache
        procedure :: update => update_svm_cache
        procedure :: comput_cache_size => comput_cache_size_svm_cache
        procedure :: get_available_cache_count => get_available_cache_count_svm_cache
    end type svm_cache

    interface svm_cache
        procedure :: new_svm_cache
    end interface svm_cache



contains
    !> Cache space for svm only. 
    !> Performance of my implemented hash map is very low.
    !! \param n_caches number of cache space = number of training samples.
    function new_svm_cache(n_caches)
        implicit none
        type(svm_cache) :: new_svm_cache
        integer(kind=8) :: n_caches

        new_svm_cache%n_caches = n_caches
        new_svm_cache%n_used = 0

        allocate(new_svm_cache%is_used(n_caches))
        allocate(new_svm_cache%recency(n_caches))
        new_svm_cache%is_used(:) = f_
        new_svm_cache%recency(:) = huge(0_8)

        allocate(new_svm_cache%kernels(n_caches))
    end function new_svm_cache

    !> Extract key-th row of Q matrix. 
    !! \param key row index of Q matrix to be extracted.
    !! \param idx_hash hash of shrunk row indices.
    !! \param values extracted key-th row of Q matrix.
    !! \param n_samples number of training samples.
    !! \param loop_count loop counter. This is used for Least Recently Used Stratedy.
    !! \param status query status. 0 indicates no error. 1 means that the result of the calculation for k-th row of Q matrix does not exist. 2 indicates that the result of the calculation of the kth row of the Q matrix is present but partially missing.
    !! \param drop_existing_rows If .true., extract shrunk indices. 
    !! \param compute_non_overlapping If .true., extract overlapping indices. 'overlapping' means duplicated indices of previous shrinkage stage.
    !! \param idx_hash_old hash of shrunk row indices of previous shrinkage stage.
    subroutine query_svm_cache(this, key, idx_hash, values, n_samples, loop_count, status, &
        drop_existing_rows, compute_non_overlapping, idx_hash_old)
        implicit none
        class(svm_cache), target       :: this
        integer(kind=8), intent(in)    :: key
        integer(kind=8), intent(in)    :: idx_hash
        real(kind=8), intent(inout)    :: values(n_samples)
        integer(kind=8), intent(in)    :: n_samples
        integer(kind=8), intent(in)    :: loop_count
        integer(kind=8), intent(inout) :: status
        logical(kind=4), intent(in)    :: drop_existing_rows
        logical(kind=4), intent(in)    :: compute_non_overlapping
        integer(kind=8), intent(in)    :: idx_hash_old
        integer(kind=8)    :: i

        ! Is used or not.
        if ( this%is_used(key) ) then
            ! Kernel is already computed
            if (this%kernels(key)%idx_hash == idx_hash) then
                status = 0
                do i=1, n_samples, 1
                    values(i) = this%kernels(key)%values(i)
                end do
                this%recency(key) = loop_count
            elseif (drop_existing_rows .and. this%kernels(key)%idx_hash == idx_hash_old) then
                status = 0
                do i=1, n_samples, 1
                    values(i) = this%kernels(key)%values(i)
                end do
                this%recency(key) = loop_count
            elseif (compute_non_overlapping .and. this%kernels(key)%idx_hash == idx_hash_old) then
                status = 2
                do i=1, n_samples, 1
                    values(i) = this%kernels(key)%values(i)
                end do
                this%recency(key) = loop_count
            else
                status = 1
            end if
        else
            ! No results
            status = 1
        end if
    end subroutine query_svm_cache


    !> Update key-th row of Q matrix. 
    !! \param key row index of Q matrix to be extracted.
    !! \param idx_hash hash of shrunk row indices.
    !! \param values extracted key-th row of Q matrix.
    !! \param n_samples number of training samples.
    !! \param loop_count loop counter. This is used for Least Recently Used Stratedy.
    subroutine update_svm_cache(this, key, idx_hash, values, n_values, loop_count)
        implicit none
        class(svm_cache) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8), intent(in) :: idx_hash
        real(kind=8), intent(in)    :: values(n_values)
        integer(kind=8), intent(in) :: n_values
        integer(kind=8), intent(in) :: loop_count

        integer(kind=8) :: oldest_used_idx

        this%is_used(key) = t_
        if (.not. allocated(this%kernels(key)%values)) allocate(this%kernels(key)%values(n_values))
        this%kernels(key)%values(:) = values(:)
        this%kernels(key)%idx_hash = idx_hash
        this%recency(key) = loop_count
        this%n_used = this%n_used + 1

        if (this%n_used >= this%n_avail) then
            oldest_used_idx = minloc(this%recency, dim=1)
            deallocate(this%kernels(oldest_used_idx)%values)
            this%recency(oldest_used_idx) = huge(0_8)
            this%is_used(oldest_used_idx) = f_
            this%n_used = count(this%is_used)
            this%kernels(oldest_used_idx)%idx_hash = 0_8
            this%n_used = this%n_used - 1
        end if
    end subroutine update_svm_cache

    
    !> Compute Cache size in MB.
    function comput_cache_size_svm_cache(this) result(cache_size)
        implicit none
        class(svm_cache) :: this

        integer(kind=8)  :: count_used
        real(kind=8)  :: cache_size ! cache size in MB

        count_used = count(this%is_used)

        cache_size = count_used * 64_8 * this%n_caches / 8d0 / 1024d0 / 1024d0
    end function comput_cache_size_svm_cache

    !> Compute available number of cache.
    !! \param max_cache_size maximum cache size.
    subroutine get_available_cache_count_svm_cache(this, max_cache_size)
        implicit none
        class(svm_cache) :: this
        integer(kind=8), intent(in) :: max_cache_size
        real(kind=8) :: unit_size

        unit_size = 64_8 * this%n_caches / 8_8 / 1024d0 / 1024d0
        this%n_avail = max_cache_size / unit_size
    end subroutine get_available_cache_count_svm_cache


    !> A function to create new 'linear_svm_classifier'.
    !! \param c regularization parameter
    function new_linear_svm_classifier(c, tolerance, cache_size, num_threads)
        implicit none
        type(linear_svm_classifier) :: new_linear_svm_classifier
        real(kind=8), optional      :: c
        real(kind=8), optional      :: tolerance
        integer(kind=8), optional   :: cache_size
        integer(kind=8), optional   :: num_threads

        if (present(c)) new_linear_svm_classifier%hparam%c = c
        if (present(tolerance)) new_linear_svm_classifier%hparam%tolerance = tolerance
        if (present(cache_size)) new_linear_svm_classifier%hparam%cache_size = cache_size
        if (present(num_threads)) new_linear_svm_classifier%hparam%num_threads = num_threads
    end function new_linear_svm_classifier

    
    !> Predict 'linear_svm_classifier'
    !! \param x input data
    function predict_linear_svm_classifier(this, x) result(labels)
        implicit none
        class(linear_svm_classifier) :: this
        real(kind=8), intent(in)     :: x(:,:)
        integer(kind=8), allocatable :: labels(:,:)
        integer(kind=8) :: n_samples, n_columns

        real(kind=8), allocatable :: tmp_(:)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

        allocate(labels(n_samples, 1))
        labels = 0_8
        
        allocate(tmp_(n_samples))
        tmp_(:) = this%w0_

        call dgemv("n", n_samples, n_columns, &
                1d0, x, n_samples, &
                this%w_, 1_8, 1d0, &
                tmp_, 1_8)
        ! labels(:,1) = tmp_ / abs(tmp_)
        labels(:,1) = sign(1d0, tmp_(:))
        labels(:,1) = (labels(:,1)+1)*0.5d0
    end function predict_linear_svm_classifier


    !> Fit linear_svm_classifier. binary classification only
    !! \param x input data
    !! \param y labels
    subroutine fit_linear_svm_classifier(this, x, y)
        implicit none
        class(linear_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)

        integer(kind=8), allocatable :: y_(:), y__(:)
        integer(kind=8), allocatable :: indices(:), indices_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: n_samples_
        integer(kind=8) :: i, j, count_non_zero
        integer(kind=8) :: loop_cnt, n, i_idx, j_idx, idx
        integer(kind=8) :: status
        
        real(kind=8), allocatable :: x_non_overlap(:,:), x_shrunk(:,:)
        real(kind=8), allocatable :: vals_i(:), vals_j(:), vals_i_(:), vals_j_(:)
        real(kind=8), allocatable :: vals_i_non_over_lap(:), vals_j_non_over_lap(:)
        real(kind=8), allocatable :: a(:), a_(:)
        real(kind=8), allocatable :: ayx(:), ayx2(:)
        real(kind=8), allocatable :: ydf(:), ydf_(:)
        real(kind=8), allocatable :: x_i(:), x_j(:), x_ij(:)
        real(kind=8), allocatable :: ab(:), ab_(:)
        real(kind=8), allocatable :: krnl_diag(:), krnl_diag_(:)
        real(kind=8) :: ay, ay2, ai, aj
        real(kind=8) :: ya_i, ya_j
        real(kind=8) :: y_i, y_j, ydf_i

        logical(kind=1), allocatable :: condition_i(:), condition_j(:), condition_i_(:), condition_j_(:)
        logical(kind=4), allocatable :: y_posi(:), y_nega(:), y_posi_(:), y_nega_(:)
        logical(kind=4), allocatable :: a_posi(:), a_pena(:), a_posi_(:), a_pena_(:)
        logical(kind=1), allocatable :: independent(:)
        logical(kind=1), allocatable :: falser(:)

        type(svm_cache), target :: cache
        integer(kind=8) :: idx_hash, idx_hash_old, max_count, n_non_overlap, n_overlap
        integer(kind=8) :: n_samples_old_, n_samples_diff
        integer(kind=8), allocatable :: idx_non_overlap(:), idx_overlap(:)
        logical(kind=1), allocatable :: flag_map_new(:), flag_map_old(:)
        logical(kind=4) :: drop_existing_rows, compute_non_overlapping

        call openblas_set_num_threads(this%hparam%num_threads)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

        call this%transform_y(y, y_)

        allocate(x_i(n_columns), x_j(n_columns), x_ij(n_columns))

        allocate(a(n_samples)); a(:) = 0d0; ay = 0d0; ay2=0d0        
        allocate(ayx(n_columns), ayx2(n_columns)); ayx(:) = 0d0; ayx2(:) = 0d0
        allocate(ydf(n_samples))
        allocate(ab(n_samples))
        allocate(krnl_diag(n_samples))
        allocate(condition_i(n_samples), condition_j(n_samples))

        allocate(y_posi(n_samples), y_nega(n_samples))
        allocate(a_posi(n_samples), a_pena(n_samples))
        allocate(independent(n_samples))
        allocate(indices(n_samples))
        do i=1, n_samples, 1
            indices(i) = i
        end do
        independent = t_
        y_posi = y_(:) > 0d0
        y_nega = .not. y_posi
        a_posi = f_
        a_pena = t_
        condition_i = ((y_posi) .and. (a_posi) .or. ((y_nega) .and. (a_pena)))
        condition_j = ((y_nega) .and. (a_posi) .or. ((y_posi) .and. (a_pena)))
        loop_cnt = 0                  
   
        ydf(:) = y_(:)

        do i=1, n_samples, 1
            krnl_diag(i) = sum(x(i,:)**2d0)
        end do

        allocate(vals_i(n_samples), vals_j(n_samples))
        allocate(flag_map_new(n_samples), flag_map_old(n_samples), falser(n_samples))

        max_count = 0
        cache = svm_cache(n_caches=n_samples)
        call cache%get_available_cache_count(this%hparam%cache_size)

        falser = f_
        flag_map_old = t_
        n_samples_old_ = n_samples
        999 continue
        n_samples_ = count(independent)
        indices_ = pack(indices, mask=independent)
        condition_i_ = pack(condition_i, mask=independent)
        condition_j_ = pack(condition_j, mask=independent)
        ydf_ = pack(ydf, mask=independent)
        krnl_diag_ = pack(krnl_diag, mask=independent)
        ab_ = pack(ab, mask=independent)
        y__ = pack(y_, mask=independent)
        a_  = pack(a, mask=independent)
        a_posi_ = pack(a_posi, mask=independent)
        a_pena_ = pack(a_pena, mask=independent)
        y_posi_ = pack(y_posi, mask=independent)
        y_nega_ = pack(y_nega, mask=independent)

        if (allocated(vals_i_)) deallocate(vals_i_)
        if (allocated(vals_j_)) deallocate(vals_j_)
        allocate(vals_i_(n_samples_))
        allocate(vals_j_(n_samples_))

        if (allocated(x_shrunk)) deallocate(x_shrunk)
        allocate(x_shrunk(n_samples_, n_columns))
        flag_map_new = f_
        do n=1, n_samples_, 1
            idx = indices_(n)
            x_shrunk(n,:) = x(idx,:)
            flag_map_new(idx) = t_
        end do
        idx_hash = one_at_a_time_hash(indices_, n_samples_)
        max_count = maxval([max_count, count(cache%is_used, kind=8)])
        n_samples_diff = n_samples_ - n_samples_old_
        compute_non_overlapping = f_
        drop_existing_rows = f_

        n_non_overlap = 0
        n_overlap = count(flag_map_new .and. flag_map_old)
        idx_non_overlap = pack(indices, mask=falser)
        idx_overlap = pack(indices, mask=falser)
        if (n_samples_diff /= 0) then
            if (n_samples_diff == count(flag_map_new .and. flag_map_old) - n_samples_old_) then
                drop_existing_rows = t_
            end if
        elseif (n_non_overlap /= 0_8) then
            compute_non_overlapping = t_
            n_non_overlap = count(flag_map_new .xor. flag_map_old)
            idx_non_overlap = pack(indices, mask=flag_map_new .xor. flag_map_old)
            idx_overlap = pack(indices, mask=flag_map_new .and. flag_map_old)
        end if

        if (allocated(vals_i_non_over_lap)) deallocate(vals_i_non_over_lap)
        if (allocated(vals_j_non_over_lap)) deallocate(vals_j_non_over_lap)
        allocate(vals_i_non_over_lap(n_non_overlap))
        allocate(vals_j_non_over_lap(n_non_overlap))

        if (allocated(x_non_overlap)) deallocate(x_non_overlap)
        allocate(x_non_overlap(n_non_overlap, n_columns))
        do n=1, n_non_overlap, 1
            idx = idx_non_overlap(n)
            x_non_overlap(n,:) = x(idx,:)
        end do        

        do while (t_)
            i = minloc(ydf_, mask=condition_i_, dim=1)
            i_idx = indices_(i)
            ydf_i = ydf_(i)
            x_i(:) = x(i_idx,:)

            call cache%query(i_idx, idx_hash, vals_i, n_samples, loop_cnt, status, &
                    drop_existing_rows, compute_non_overlapping, idx_hash_old)
            if (status == 1_8) then
                call dgemv("N", n_samples_, n_columns, &
                                1d0, x_shrunk, n_samples_, &
                                x_i, 1_8, 0d0, &
                                vals_i_, 1_8)
                vals_i(indices_) = vals_i_
                call cache%update(i_idx, idx_hash, vals_i, n_samples, loop_cnt)
            elseif (status == 2_8) then
                call multi_mat_vec(x_non_overlap, x_i, vals_i_non_over_lap, n_non_overlap, n_columns)
                vals_i(idx_non_overlap) = vals_i_non_over_lap
                call cache%update(i_idx, idx_hash, vals_i, n_samples, loop_cnt)
            end if
            vals_i_(:) = vals_i(indices_)

            ab_(:) = (ydf_(:) - ydf_i)**2d0 / (krnl_diag_(i) + krnl_diag_(:) - 2d0*vals_i_(:))
            j = maxloc(ab_, mask=(condition_j_ .and. ( ydf_ > ydf_i)), dim=1)
            j_idx = indices_(j)
    
            if (ydf_(j) - ydf_(i) <= this%hparam%tolerance) exit

            y_i = y__(i)
            y_j = y__(j)
            ya_i=y_i*a_(i) 
            ya_j=y_j*a_(j)
            
            x_j(:) = x(j_idx,:)
            x_ij(:) = x_i - x_j

            ay2  = ay  - ya_i        - ya_j
            ayx2 = ayx - ya_i*x_i(:) - ya_j*x_j(:)

            ai = (1d0 &
                - y_i*y_j &
                + y_i &
                    * sum( &
                            x_ij(:) * (x_j(:)*ay2 - ayx2) &
                        ) &
                    ) / ( krnl_diag_(i) + krnl_diag_(j) - 2d0*vals_i_(j) )

            if (ai<0d0) then
                ai = 0d0
            elseif (ai>this%hparam%c) then
                ai = this%hparam%c
            end if
            aj = (-ai*y_i - ay2) * y_j

            if (aj<0d0) then
                aj = 0d0
                ai = (-aj*y_j - ay2) * y_i
            elseif (aj>this%hparam%c) then
                aj = this%hparam%c
                ai = (-aj*y_j - ay2) * y_i
            end if

            ay  = ay  + y_i * (ai-a_(i))        + y_j * (aj-a_(j))
            ayx = ayx + y_i * (ai-a_(i))*x_i(:) + y_j * (aj-a_(j))*x_j(:)

            call cache%query(j_idx, idx_hash, vals_j, n_samples, loop_cnt, status, &
                    drop_existing_rows, compute_non_overlapping, idx_hash_old)
            if (status == 1_8) then
                call dgemv("N", n_samples_, n_columns, &
                                1d0, x_shrunk, n_samples_, &
                                x_j, 1_8, 0d0, &
                                vals_j_, 1_8)
                vals_j(indices_) = vals_j_
                call cache%update(j_idx, idx_hash, vals_j, n_samples, loop_cnt)
            elseif (status == 2_8) then
                call multi_mat_vec(x_non_overlap, x_j, vals_j_non_over_lap, n_non_overlap, n_columns)
                vals_j(idx_non_overlap) = vals_j_non_over_lap
                call cache%update(j_idx, idx_hash, vals_j, n_samples, loop_cnt)
            end if
            vals_j_(:) = vals_j(indices_)

            ydf_(:) = ydf_(:) - y_i * (ai-a_(i)) * vals_i_(:) - y_j * (aj-a_(j)) * vals_j_(:)

            if (ai==a_(i)) exit

            a_(i) = ai
            a_(j) = aj

            a_posi_(i) =  ai > 0d0
            a_posi_(j) =  aj > 0d0
            a_pena_(i) =  ai < this%hparam%c
            a_pena_(j) =  aj < this%hparam%c

            condition_i_(i) = (y_posi_(i) .and. a_posi_(i)) .or. (y_nega_(i) .and. a_pena_(i))
            condition_i_(j) = (y_posi_(j) .and. a_posi_(j)) .or. (y_nega_(j) .and. a_pena_(j))

            condition_j_(i) = (y_nega_(i) .and. a_posi_(i)) .or. (y_posi_(i) .and. a_pena_(i))
            condition_j_(j) = (y_nega_(j) .and. a_posi_(j)) .or. (y_posi_(j) .and. a_pena_(j))

            loop_cnt = loop_cnt + 1
            
            if (mod(loop_cnt, minval([1000_8, n_samples]))==0) then
                ydf(indices_) = ydf_
                condition_i(indices_) = condition_i_
                condition_j(indices_) = condition_j_
                a(indices_) = a_
                a_posi(indices_) = a_posi_
                a_pena(indices_) = a_pena_
                y_posi(indices_) = y_posi_
                y_nega(indices_) = y_nega_
                i = minloc(ydf, mask=condition_i, dim=1)
                j = maxloc(ydf, mask=condition_j, dim=1)

                independent = ((y_posi .and. a == 0d0) .or. (y_nega .and. a == this%hparam%c))  .and. (ydf<ydf(i))
                independent = independent & 
                    .or.   (  ((y_nega .and. a == 0d0) .or. (y_posi .and. a == this%hparam%c)) .and. (ydf>ydf(j)))
                independent = .not. independent
                flag_map_old = flag_map_new
                n_samples_old_ = n_samples_
                idx_hash_old = idx_hash
                goto 999
            end if
        end do

        1000 continue
        if (allocated(this%w_)) deallocate(this%w_)
        allocate(this%w_(n_columns))

        this%w_ = 0d0
        this%w0_ = 0d0
        count_non_zero = 0
        do s=1, n_samples
            if (a(s)>0d0) then
                this%w_  = this%w_  + (a(s)*y_(s)) * x(s,:)
                count_non_zero = count_non_zero + 1
            end if
        end do
        do s=1, n_samples
            if (a(s)>0d0) then
                this%w0_ = this%w0_ + (y_(s) - sum(x(s,:) * this%w_))
            end if
        end do
        this%w0_ = this%w0_ /dble(count_non_zero)

        this%n_iter_ = loop_cnt

        ! print*, "Cache Size [MB]:       ", cache%comput_cache_size()
        ! print*, "Cache Used Count:      ", count(cache%is_used), cache%n_used
        ! print*, "Cache Available Count: ", cache%n_avail
    end subroutine fit_linear_svm_classifier

    !> Fit linear_svm_classifier. binary classification only. hard margin
    !! \param x input data
    !! \param y labels
    subroutine fit_linear_svm_classifier_hard(this, x, y)
        implicit none
        class(linear_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)

        integer(kind=8), allocatable :: y_trans(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:)
        real(kind=8) :: ay, ay2, ai, aj

        call this%transform_y(y, y_trans)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

        allocate(a(n_samples)); a(:) = 0d0
        ay = 0d0; ay2=0d0
        allocate(ayx(n_columns), ayx2(n_columns)); ayx(:) = 0d0; ayx2(:) = 0d0
        allocate(yx, source=x)
        allocate(ydf(n_samples))
        allocate(condition_i(n_samples), condition_j(n_samples))

        do s=1, n_samples, 1
            yx(s,:) = y_trans(s) * yx(s,:)
        end do

        do while (t_)
            call dgemv("n", n_samples, n_columns, &
                    1d0, yx, n_samples, &
                    ayx, 1_8, 0d0, &
                    ydf, 1_8)
            ydf = y_trans(:) * (1d0 - ydf)

            condition_i = ((y_trans(:)<0d0) .or. (a(:)>0d0))
            i = minloc(ydf, mask=condition_i, dim=1)
            
            condition_j = ((y_trans(:)>0d0) .or. (a(:)>0d0))
            j = maxloc(ydf, mask=condition_j, dim=1)

            if (ydf(i) >= ydf(j)) exit

            ay2  = ay  - y_trans(i)*a(i)        - y_trans(j)*a(j)
            ayx2 = ayx - y_trans(i)*a(i)*x(i,:) - y_trans(j)*a(j)*x(j,:)

            ai = (1d0 &
                - y_trans(i)*y_trans(j) &
                + y_trans(i) &
                    * sum(&
                            (x(i,:)-x(j,:)) * (x(j,:)*ay2 - ayx2) &
                        ) &
                    ) / sum((x(i,:)-x(j,:))**2d0)

            ai = maxval([ai, 0d0])
            aj = (-ai*y_trans(i) - ay2) * y_trans(j)

            if (aj<0d0) then
                aj = 0d0
                ai = (-aj*y_trans(j) - ay2) * y_trans(i)
            end if

            ay = ay   + y_trans(i) * (ai-a(i))        + y_trans(j) * (aj-a(j))
            ayx = ayx + y_trans(i) * (ai-a(i))*x(i,:) + y_trans(j) * (aj-a(j))*x(j,:)

            if (ai==a(i)) exit

            a(i) = ai
            a(j) = aj
        end do

        allocate(this%w_(n_columns))
        this%w_ = 0d0
        this%w0_ = 0d0
        count_non_zero = 0
        do s=1, n_samples
            if (a(s)>0d0) then
                this%w_  = this%w_  + (a(s)*y_trans(s)) * x(s,:)
                count_non_zero = count_non_zero + 1
            end if
        end do
        do s=1, n_samples
            if (a(s)>0d0) then
                this%w0_ = this%w0_ + (y_trans(s) - sum(x(s,:) * this%w_))
            end if
        end do
        this%w0_ = this%w0_ /dble(count_non_zero)

        print*, this%w_
        print*, this%w0_
    end subroutine fit_linear_svm_classifier_hard


    !> Transform input labels for svm format. y in {+1, -1}.
    !! \param y input labels
    !! \param y_trans transformed labels
    subroutine transform_y(this, y, y_trans)
        implicit none
        class(linear_svm_classifier) :: this
        integer(kind=8), intent(in) :: y(:,:)
        integer(kind=8), allocatable, intent(inout) :: y_trans(:)

        integer(kind=8), allocatable :: uniq_y(:)
        integer(kind=8) :: n_y, uniq_

        n_y = size(y, dim=1)
        allocate(y_trans(n_y))
        y_trans(:) = y(:,1)
        call quick_sort(y_trans, n_y)

        call collect_unique_values(uniq_y, y_trans, n_y)

        if (all(uniq_y .eq. [-1_8, 1_8])) then    ! No processing required
            y_trans(:) = y(:,1)
        elseif (all(uniq_y .eq. [0_8, 1_8])) then ! 0 -> -1, 1 -> 1
            y_trans(:) = 2_8 * y(:,1) - 1_8
        elseif (size(uniq_y)>3) then              ! multi-class
            print*, "Unique Labels: ", uniq_y
            stop "Number of labels greater than 2."
        else                                      ! otherwise
            print*, "Unique Labels: ", uniq_y
            stop "NOT Implemented Error"
        end if
    end subroutine transform_y


end module mod_linear_svm