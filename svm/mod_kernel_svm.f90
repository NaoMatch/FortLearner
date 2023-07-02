!> A module for kernel svm.
module mod_kernel_svm
    use mod_const
    use mod_sort
    use mod_hyperparameter
    use mod_kernel
    use mod_svm_cache
    use mod_hash_map
    use mod_data_holder
    implicit none
    
    !> A kernel svc type.
    type kernel_svm_classifier
    type(hparam_kernel_svm_classifier) :: hparam !> hyper parameter 
        real(kind=8), allocatable :: a_(:) !> alpha. training result
        real(kind=8), allocatable :: y_(:) !> labels. training result
        real(kind=8) :: w0_ !> intercept training result
        integer(kind=8) :: n_iter_ !> number of iteration. training result
        type(kernel) :: kernel !> kernel
    contains
        procedure, pass :: fit_kernel_svm_classifier_x
        procedure, pass :: fit_kernel_svm_classifier_dholder
        generic :: fit => fit_kernel_svm_classifier_x, fit_kernel_svm_classifier_dholder
        procedure :: predict => predict_kernel_svm_classifier

        procedure :: transform_y
    end type kernel_svm_classifier

    interface kernel_svm_classifier
        procedure :: new_kernel_svm_classifier
    end interface kernel_svm_classifier

contains

    !> A function to create new 'kernel_svm_classifier'.
    !! \param kernel kernel type. linear/poly/sigmoid/rbf.
    !! \param c regularization parameter
    !! \param tolerance stopping criterion. Smaller is more accurate, but takes longer.
    !! \param cache_size kernel cache size. 
    !! \param num_threads number of threads of 'dgemv' 
    !! \param shrinking use shrinking strategy or not. 
    function new_kernel_svm_classifier(kernel, c, degree, w0, tolerance, cache_size, shrinking) result(svm)
        implicit none
        type(kernel_svm_classifier) :: svm
        character(len=*), optional  :: kernel
        real(kind=8), optional      :: c
        integer(kind=8), optional   :: degree
        real(kind=8), optional      :: w0
        real(kind=8), optional      :: tolerance
        integer(kind=8), optional   :: cache_size
        logical(kind=4), optional   :: shrinking

        if (present(kernel)) svm%hparam%kernel = kernel
        if (svm%hparam%kernel == "linear") then
            svm%hparam%kernel_int = 1_8
        elseif (svm%hparam%kernel == "poly") then
            svm%hparam%kernel_int = 2_8
        elseif (svm%hparam%kernel == "sigmoid") then
            svm%hparam%kernel_int = 3_8
        elseif (svm%hparam%kernel == "rgf") then
            svm%hparam%kernel_int = 4_8
        end if
        if (present(c)) svm%hparam%c = c
        if (present(degree)) svm%hparam%degree = degree
        if (present(w0)) svm%hparam%w0 = w0
        if (present(tolerance)) svm%hparam%tolerance = tolerance
        if (present(cache_size)) svm%hparam%cache_size = cache_size
        if (present(shrinking)) svm%hparam%shrinking = shrinking

        ! Kernel settings
        svm%kernel%kernel_int = svm%hparam%kernel_int
        svm%kernel%degree = svm%hparam%degree
        svm%kernel%w0 = svm%hparam%w0

    end function new_kernel_svm_classifier

    subroutine fit_kernel_svm_classifier_dholder(this, dholder)
        implicit none
        class(kernel_svm_classifier) :: this
        type(data_holder) :: dholder
        call this%fit_kernel_svm_classifier_x(dholder%x_ptr%x_r8_ptr, dholder%y_ptr%y_i8_ptr)
    end subroutine fit_kernel_svm_classifier_dholder

    !> Fit linear_svm_classifier. binary classification only
    !! \param x input data
    !! \param y labels
    subroutine fit_kernel_svm_classifier_x(this, x, y)
        implicit none
        class(kernel_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero, n_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:), ays(:)
        real(kind=8), allocatable :: ab(:)
        real(kind=8), allocatable :: kernel_mat(:,:), kernel_diag(:)
        integer(kind=8), allocatable :: idx_non_zero(:), indices(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:), is_non_zero(:)
        real(kind=8) :: ay, ay2, ai, aj
        integer(kind=8) :: yi, yj
        real(kind=8) :: kii, kjj, kij
        real(kind=8), allocatable :: kxi(:), kxj(:)

        integer(kind=8) :: loop_cnt, status, idx_hash, idx_hash_old
        type(svm_cache) :: cache
        integer(kind=8), allocatable :: indices_(:)
        real(kind=8), allocatable, target :: x_(:,:), ydf_(:)
        real(kind=8), allocatable :: kxi_(:), kxj_(:), kernel_diag_(:)
        real(kind=8), allocatable :: ays_(:), ab_(:)
        logical(kind=4), allocatable :: condition_i_(:), condition_j_(:)
        logical(kind=4), allocatable :: independent(:)
        integer(kind=8) :: i_idx, j_idx
        integer(kind=8) :: n_samples_, n_overlap, n_non_overlap
        logical(kind=4) :: drop_existing_rows, compute_non_overlapping
        logical(kind=4) :: is_aggressive
        logical(kind=1), allocatable :: flag_map_new(:), flag_map_old(:), falser(:)
        integer(kind=8) :: n_samples_old_, n_samples_diff, loop_cnt_base
        integer(kind=8), allocatable :: idx_non_overlap(:), idx_overlap(:)

        integer(kind=8) :: ws(2), ws_old(2), count_tol, max_count_tol
        max_count_tol = 2_8

        call this%transform_y(y, y_)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)
        cache = svm_cache(n_caches=n_samples)

        allocate(a(n_samples)); a(:) = 0d0
        ay = 0d0
        allocate(ayx(n_columns)); ayx(:) = 0d0
        allocate(ydf(n_samples))
        allocate(indices(n_samples))
        allocate(condition_i(n_samples), condition_j(n_samples))
        allocate(is_non_zero(n_samples))
        allocate(ays(n_samples))
        allocate(kernel_diag(n_samples))
        allocate(ab(n_samples))
        allocate(independent(n_samples))
        allocate(kxi(n_samples), kxj(n_samples))
        allocate(falser(n_samples))

        falser = f_
        do s=1, n_samples, 1
            indices(s) = s
        end do
        loop_cnt = 0

        ydf(:) = y_(:)  
        call this%kernel%set_training_data(x)

        ays(:) = 0d0
        ydf(:) = y_(:)
        call this%kernel%compute_diagonal_elements(kernel_diag, n_samples)
        
        condition_i = ((y_(:)>0_8) .and. (a(:)>0d0) .or. ((y_(:)<0_8) .and. (a(:)<this%hparam%c)))
        condition_j = ((y_(:)<0_8) .and. (a(:)>0d0) .or. ((y_(:)>0_8) .and. (a(:)<this%hparam%c)))

        idx_hash_old = 0_8
        independent = t_
        is_aggressive = t_
        ays = 0d0
        ws_old = 0_8
        count_tol = 0

        ! shrinking
        1000 continue
        n_samples_ = count(independent)
        indices_ = pack(indices, mask=independent)
        condition_i_ = pack(condition_i, mask=independent)
        condition_j_ = pack(condition_j, mask=independent)
        kernel_diag_ = pack(kernel_diag, mask=independent)
        x_ = x(indices_,:)
        ydf_ = pack(ydf, mask=independent)
        kxi_ = ydf_
        kxj_ = ydf_
        ab_ = ydf_
        ays_ = pack(ays, mask=independent)

        idx_hash = one_at_a_time_hash(indices_, n_samples_)

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

        drop_existing_rows = f_
        compute_non_overlapping = f_
        ! print*, loop_cnt, n_samples_
        do while (t_)
            i = minloc(ydf_, mask=condition_i_, dim=1)
            if (i<1) exit
            i_idx = indices_(i)
            call cache%query(i_idx, idx_hash, kxi, n_samples, loop_cnt, status, &
                            drop_existing_rows, compute_non_overlapping, idx_hash_old)
            if (status /= 0_8) then
                call this%kernel%compute_kth_row(i_idx, kxi, n_samples)
                call cache%update(i_idx, idx_hash, kxi, n_samples, loop_cnt)
            end if
            kxi_ = kxi(indices_)
            
            ab_ = (ydf_ - ydf_(i))**2d0 / ( kernel_diag_(i) + kernel_diag_(:) - 2d0*kxi_(:) )
            j = maxloc(ab_, mask=condition_j_ .and. ( ydf_ > ydf_(i)), dim=1)
            if (j<1) exit
            j_idx = indices_(j)
            call cache%query(j_idx, idx_hash, kxj, n_samples, loop_cnt, status, &
                            drop_existing_rows, compute_non_overlapping, idx_hash_old)
            if (status /= 0_8) then
                call this%kernel%compute_kth_row(j_idx, kxj, n_samples)
                call cache%update(j_idx, idx_hash, kxj, n_samples, loop_cnt)
            end if
            kxj_ = kxj(indices_)
            
            yi = y_(i_idx)
            yj = y_(j_idx)
            ws(:) = [i_idx, j_idx]
            if (all(ws == ws_old)) count_tol = count_tol + 1
            if (count_tol>=max_count_tol) exit
            if (ydf_(j) - ydf_(i) <= this%hparam%tolerance) exit
            ws_old = ws
            ay2 = ay - yi*a(i_idx) - yj*a(j_idx)
            
            kii = kernel_diag_(i)
            kjj = kernel_diag_(j)
            kij = kxi_(j)
            
            ai = ((1d0 - yi*yj + yi*((kij-kjj)*ay2 - sum( ays_(:)*(kxi_(:)-kxj_(:)) )))) & 
                / (kii+kjj-2d0*kij)

            if (ai<0d0) then
                ai =0d0
            elseif (ai>this%hparam%c) then
                ai = this%hparam%c
            end if
            aj = (-ai*yi - ay2) * yj

            if (aj<0d0) then
                aj =0d0
                ai = (-aj*yj - ay2) * yi
            elseif (aj>this%hparam%c) then
                aj = this%hparam%c
                ai = (-aj*yj - ay2) * yi
            end if

            ay = ay + yi * (ai-a(i_idx)) + yj * (aj-a(j_idx))
            if (ai==a(i_idx)) exit

            ydf_(:) = ydf_(:) - yi * (ai-a(i_idx)) * kxi_(:) - yj * (aj-a(j_idx)) * kxj_(:)

            a(i_idx) = ai
            a(j_idx) = aj

            ays_(i) = ai * yi
            ays_(j) = aj * yj
            condition_i_(i) = ((yi>0_8) .and. (ai>0d0) .or. ((yi<0_8) .and. (ai<this%hparam%c)))
            condition_j_(j) = ((yj<0_8) .and. (aj>0d0) .or. ((yj>0_8) .and. (aj<this%hparam%c)))

            if (is_aggressive) then
                if (ydf_(j) - ydf_(i) <= 10d0*this%hparam%tolerance) then
                    is_aggressive = f_
                    loop_cnt_base = loop_cnt
                    loop_cnt = loop_cnt + 1
                end if
            else
                if (mod(loop_cnt-loop_cnt_base, minval([1000_8, n_samples]))==0 .and. this%hparam%shrinking) then
                    ! print*, this%hparam%shrinking
                    ydf(indices_) = ydf_
                    condition_i(indices_) = condition_i_
                    condition_j(indices_) = condition_j_
                    ays(indices_) = ays_
                    i = minloc(ydf, mask=condition_i, dim=1)
                    j = maxloc(ydf, mask=condition_j, dim=1)

                    independent = ((y_(:)>0_8 .and. a == 0d0) .or. (y_(:)<0_8 .and. a == this%hparam%c))  .and. (ydf<ydf(i))
                    independent = independent & 
                        .or.   (  ((y_(:)<0_8 .and. a == 0d0) .or. (y_(:)>0_8 .and. a == this%hparam%c)) .and. (ydf>ydf(j)))
                    independent = .not. independent
                    idx_hash_old = idx_hash
                    loop_cnt = loop_cnt + 1
                    goto 1000
                end if
            end if
            loop_cnt = loop_cnt + 1
        end do
        ydf(indices_) = ydf_

        if (allocated(this%a_)) deallocate(this%a_)
        if (allocated(this%y_)) deallocate(this%y_)
        indices_ = pack(indices, mask=a(:)>0d0)
        n_samples_ = size(indices_)

        allocate(this%a_(n_samples_))
        allocate(this%y_(n_samples_))

        this%a_ = pack(a,  mask=a(:)>0d0)
        this%y_ = pack(y_, mask=a(:)>0d0)
        call this%kernel%set_training_result(x, n_samples, n_columns, indices_, n_samples_)
        this%w0_ = sum(this%y_ - ydf(indices_)) / dble(n_samples_)

        this%n_iter_ = loop_cnt 
    end subroutine fit_kernel_svm_classifier_x


    !> Predict 'linear_svm_classifier'
    !! \param x input data
    function predict_kernel_svm_classifier(this, x) result(labels)
        implicit none
        class(kernel_svm_classifier) :: this
        real(kind=8), intent(in)     :: x(:,:)
        integer(kind=8), allocatable :: labels(:,:)
        integer(kind=8) :: n_samples, n_columns

        real(kind=8), allocatable :: tmp_(:)
        integer(kind=8) :: n
        real(kind=8), allocatable :: kernel_mat(:,:), ays(:), ydf(:), kay(:)
        real(kind=8), allocatable :: x_sq_sum(:)
        integer(kind=8), allocatable :: idx_non_zero(:), indices(:)
        logical(kind=1), allocatable :: is_non_zero(:)
        
        call openblas_set_num_threads(this%hparam%num_threads)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

        if (allocated(labels)) deallocate(labels)
        allocate(labels(n_samples, 1))
        
        if (allocated(tmp_)) deallocate(tmp_)
        allocate(tmp_(n_samples))

        if (allocated(x_sq_sum)) deallocate(x_sq_sum)
        allocate(x_sq_sum(n_samples))
        call matrix_sqsum_row(x, x_sq_sum, n_samples, n_columns)

        if (allocated(kay)) deallocate(kay)
        allocate(kay(this%kernel%n_samples))

        if (allocated(ydf)) deallocate(ydf)
        allocate(ydf(n_samples))
        ays = (this%a_*this%y_)
        if (this%kernel%kernel_int == 4_8) then
            do n=1, n_samples, 1
                kay = this%kernel%x_sq_sum + x_sq_sum(n)
                call dgemv("n", this%kernel%n_samples, this%kernel%n_columns, &
                            -2d0, this%kernel%x_, this%kernel%n_samples, &
                            x(n,:), 1_8, 1d0, &
                            kay, 1_8)
                kay = exp(-kay*this%kernel%gamma)
                ydf(n) = dot_product(kay, ays) + this%w0_
            end do
        elseif (this%kernel%kernel_int == 3_8) then
            do n=1, n_samples, 1
                call dgemv("n", this%kernel%n_samples, this%kernel%n_columns, &
                                1d0, this%kernel%x_, this%kernel%n_samples, &
                                x(n,:), 1_8, 0d0, &
                                kay, 1_8)
                kay = tanh(kay+this%hparam%w0)
                ydf(n) = dot_product(kay, ays) + this%w0_
            end do
        elseif (this%kernel%kernel_int == 2_8) then
            do n=1, n_samples, 1
                call dgemv("n", this%kernel%n_samples, this%kernel%n_columns, &
                                1d0, this%kernel%x_, this%kernel%n_samples, &
                                x(n,:), 1_8, 0d0, &
                                kay, 1_8)
                kay = (kay+this%hparam%w0) ** this%kernel%degree
                ydf(n) = dot_product(kay, ays) + this%w0_
            end do
        elseif (this%kernel%kernel_int == 1_8) then
            do n=1, n_samples, 1
                call dgemv("n", this%kernel%n_samples, this%kernel%n_columns, &
                                1d0, this%kernel%x_, this%kernel%n_samples, &
                                x(n,:), 1_8, 0d0, &
                                kay, 1_8)
                ydf(n) = dot_product(kay, ays) + this%w0_
            end do
        end if
        labels(:,1) = ydf / abs(ydf)
        labels(:,1) = (labels(:,1)+1)*0.5d0
    end function predict_kernel_svm_classifier


    !> Transform input labels for svm format.
    !! \param y input labels
    !! \param y_trans transformed labels
    subroutine transform_y(this, y, y_trans)
        implicit none
        class(kernel_svm_classifier) :: this
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
end module mod_kernel_svm