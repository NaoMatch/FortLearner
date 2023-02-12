module mod_kernel_svm
    use mod_const
    use mod_sort
    use mod_hyperparameter
    use mod_kernel
    use mod_svm_cache
    use mod_hash_map
    implicit none
    
    type kernel_svm_classifier
        real(kind=8), allocatable :: a_(:)
        real(kind=8), allocatable :: y_(:)
        real(kind=8), allocatable :: x_(:,:)
        real(kind=8) :: w0_        
        integer(kind=8) :: n_iter_
        type(hparam_kernel_svm_classifier) :: hparam
        type(rbf_kernel) :: krnl
        type(kernel) :: kernel
    contains
        procedure :: fit => fit_kernel_svm_classifier
        procedure :: fit_kernel_svm_classifier_ver01
        procedure :: fit_kernel_svm_classifier_ver02
        procedure :: fit_kernel_svm_classifier_ver03
        procedure :: fit_kernel_svm_classifier_ver04
        procedure :: fit_kernel_svm_classifier_ver05
        procedure :: predict => predict_kernel_svm_classifier

        procedure :: transform_y
    end type kernel_svm_classifier

    interface kernel_svm_classifier
        procedure :: new_kernel_svm_classifier
    end interface kernel_svm_classifier

contains

    function new_kernel_svm_classifier(kernel, c, degree, sigma, w0, tolerance, cache_size) result(svm)
        implicit none
        type(kernel_svm_classifier) :: svm
        character(len=*), optional  :: kernel
        real(kind=8), optional      :: c
        integer(kind=8), optional   :: degree
        real(kind=8), optional      :: sigma
        real(kind=8), optional      :: w0
        real(kind=8), optional      :: tolerance
        integer(kind=8), optional   :: cache_size

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
        if (present(sigma)) svm%hparam%sigma = sigma
        if (present(w0)) svm%hparam%w0 = w0
        if (present(tolerance)) svm%hparam%tolerance = tolerance
        if (present(cache_size)) svm%hparam%cache_size = cache_size

        ! Kernel settings
        svm%kernel%kernel_int = svm%hparam%kernel_int
        svm%kernel%degree = svm%hparam%degree
        svm%kernel%w0 = svm%hparam%w0

        svm%kernel%sigma = svm%hparam%sigma
        svm%kernel%sigma_sq_inv = (1d0/svm%hparam%sigma)**2d0 * 0.5d0
    end function new_kernel_svm_classifier
    
    
    subroutine fit_kernel_svm_classifier(this, x, y)
        implicit none
        class(kernel_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero, n_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:), ays(:)
        real(kind=8), allocatable :: kernel_mat(:,:)
        integer(kind=8), allocatable :: idx_non_zero(:), indices(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:), is_non_zero(:)
        real(kind=8) :: ay, ay2, ai, aj
        real(kind=8) :: kii, kjj, kij
        real(kind=8), allocatable :: kxi(:,:), kxj(:,:)

        integer(kind=8) :: loop_cnt, status, idx_hash, idx_hash_old
        type(svm_cache) :: cache

        this%krnl = rbf_kernel(x=x, sigma=this%hparam%sigma)

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

        do s=1, n_samples, 1
            indices(s) = s
        end do
        loop_cnt = 0
        idx_hash = one_at_a_time_hash(indices, n_samples)
        idx_hash_old = idx_hash-1
        do while (t_)
            ! print*, '*********************************************************************************************'
            ! print*, '*********************************************************************************************'
            ! print*, '*********************************************************************************************'
            ! print*, '*********************************************************************************************'
            is_non_zero = a(:)/=0d0
            idx_non_zero = pack(indices, mask=is_non_zero)
            n_non_zero = size(idx_non_zero)
            kernel_mat = this%krnl%eval(x, n_samples, n_columns, idx_non_zero, n_non_zero)
            ays = (a(idx_non_zero)*y_(idx_non_zero))

            ydf = 0d0
            call dgemv("n", n_samples, n_non_zero, &
                    1d0, kernel_mat, n_samples, &
                    ays, 1_8, 0d0, &
                    ydf, 1_8)
            ! print*, "iter=", iter, "sum(ydf)= ", sum(ydf), "shape(ydf)=", shape(ydf), " ::: size(ays)=", size(ays)
            ydf(:) = y_(:) * (1d0 - y_(:)*ydf(:))
            ! print*, kernel_mat(:,1)
            ! print*, kernel_mat(:,2)
            ! print*, "iter=", iter, "sum(ydf)= ", sum(ydf), "shape(ydf)=", shape(ydf), &
            !     "sum(kernel_mat)=", sum(kernel_mat), " :: ", int(idx_non_zero), n_non_zero, sum(ays)

            condition_i = ((y_(:)>0_8) .and. (a(:)>0d0) .or. ((y_(:)<0_8) .and. (a(:)<this%hparam%c)))
            i = minloc(ydf, mask=condition_i, dim=1)
            
            condition_j = ((y_(:)<0_8) .and. (a(:)>0d0) .or. ((y_(:)>0_8) .and. (a(:)<this%hparam%c)))
            j = maxloc(ydf, mask=condition_j, dim=1)

            ! print*, iter, i, j, ydf(i) >= ydf(j)
            if (ydf(j) - ydf(i) <= this%hparam%tolerance) exit

            ay2 = ay - y_(i)*a(i) - y_(j)*a(j)
            ! print*, "                           ay2     = ", ay2, sum(a)

            kii = this%krnl%val_ij(i, i)
            kjj = this%krnl%val_ij(j, j)
            kij = this%krnl%val_ij(i, j)
            ! print*, "                           kii, kjj, kij     = ", kii, kjj, kij

            is_non_zero = a(:)/=0d0
            is_non_zero(i) = f_
            is_non_zero(j) = f_
            idx_non_zero = pack(indices, mask=is_non_zero)
            n_non_zero = size(idx_non_zero)
            
            kxi = this%krnl%eval(x(i:i,:), 1_8, n_columns, idx_non_zero, n_non_zero)
            kxj = this%krnl%eval(x(j:j,:), 1_8, n_columns, idx_non_zero, n_non_zero)
            ! print*, "                           kxi     = ", sum(kxi), shape(kxi)
            ! print*, "                           kxj     = ", sum(kxj), shape(kxj)
            
            ai = ((1 - y_(i)*y_(j) + y_(i)*((kij-kjj)*ay2 - sum( a(idx_non_zero)*y_(idx_non_zero)*(kxi(1,:)-kxj(1,:)) )))) & 
                / (kii+kjj-2d0*kij)
            ! print*, "                           ai      = ", ai
            ! print*, "                                 a(idx_non_zero)*y_(idx_non_zero) = ", sum(a(idx_non_zero)*y_(idx_non_zero))
            ! print*, "                                 kxi(1,:)-kxj(1,:) = ", sum(kxi(1,:)-kxj(1,:))
            
            if (ai<0d0) then
                ai =0d0
            elseif (ai>this%hparam%c) then
                ai = this%hparam%c
            end if
            aj = (-ai*y_(i) - ay2) * y_(j)

            if (aj<0d0) then
                aj =0d0
                ai = (-aj*y_(j) - ay2) * y_(i)
            elseif (aj>this%hparam%c) then
                aj = this%hparam%c
                ai = (-aj*y_(j) - ay2) * y_(i)
            end if

            ! print*, "                           ai, aj      = ", ai, aj
            ay = ay + y_(i) * (ai-a(i)) + y_(j) * (aj-a(j))
            ! print*, "                           ay      = ", ay

            ! print*, "ai==a(i) :: ", ai==a(i)
            if (ai==a(i)) exit

            a(i) = ai
            a(j) = aj
            ! print*, "                           sum(a)      = ", sum(a)

            loop_cnt = loop_cnt + 1
        end do

        if (allocated(this%a_)) deallocate(this%a_)
        if (allocated(this%y_)) deallocate(this%y_)
        if (allocated(this%x_)) deallocate(this%x_)
        allocate(this%a_(n_samples))
        allocate(this%y_(n_samples))
        allocate(this%x_(n_samples, n_columns))
        this%a_ = a
        this%y_ = y_
        this%x_ = x

        is_non_zero = a(:)/=0d0
        idx_non_zero = pack(indices, mask=is_non_zero)
        n_non_zero = size(idx_non_zero)
        kernel_mat = this%krnl%eval(x(idx_non_zero,:), n_non_zero, n_columns, idx_non_zero, n_non_zero)
        ays = (a(idx_non_zero)*y_(idx_non_zero))

        deallocate(ydf)
        allocate(ydf(n_non_zero))
        ydf = 0d0
        call dgemv("n", n_non_zero, n_non_zero, &
                1d0, kernel_mat, n_non_zero, &
                ays, 1_8, 0d0, &
                ydf, 1_8)

        this%w0_ = sum(y_(idx_non_zero) - ydf) / dble(n_non_zero)

        ! print*, a
        ! print*, this%w0_
    end subroutine fit_kernel_svm_classifier
    
    
    ! １．a/=0以外も計算対象とする
    ! ２．Cacheを用いていない
    ! ３．Shrinkingを用いていない
    ! ４．ydfのアップデート効率が悪い
    ! ５．FanのMethodを用いる
    subroutine fit_kernel_svm_classifier_ver01(this, x, y)
        implicit none
        class(kernel_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero, n_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:), ays(:)
        real(kind=8), allocatable :: kernel_mat(:,:)
        integer(kind=8), allocatable :: idx_non_zero(:), indices(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:), is_non_zero(:)
        real(kind=8) :: ay, ay2, ai, aj
        real(kind=8) :: kii, kjj, kij
        real(kind=8), allocatable :: kxi(:), kxj(:)

        integer(kind=8) :: loop_cnt, status, idx_hash, idx_hash_old
        type(svm_cache) :: cache

        this%krnl = rbf_kernel(x=x, sigma=this%hparam%sigma)

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
        allocate(kernel_mat(n_samples, n_samples), ays(n_samples))

        do s=1, n_samples, 1
            indices(s) = s
        end do
        loop_cnt = 0
        idx_hash = one_at_a_time_hash(indices, n_samples)
        idx_hash_old = idx_hash-1

        ydf(:) = y_(:)  
        call this%kernel%set_training_data(x)


        ! do while (t_)
        call this%kernel%compute_qmatrix(kernel_mat, n_samples)
        do iter=1, 1000, 1
            ays(:) = a(:) * y_(:)
            call dgemv("n", n_samples, n_samples, &
                    1d0, kernel_mat, n_samples, &
                    ays, 1_8, 0d0, &
                    ydf, 1_8)
            ydf(:) = y_(:) * (1d0 - y_(:)*ydf(:))


            condition_i = ((y_(:)>0_8) .and. (a(:)>0d0) .or. ((y_(:)<0_8) .and. (a(:)<this%hparam%c)))
            i = minloc(ydf, mask=condition_i, dim=1)
            
            condition_j = ((y_(:)<0_8) .and. (a(:)>0d0) .or. ((y_(:)>0_8) .and. (a(:)<this%hparam%c)))
            j = maxloc(ydf, mask=condition_j, dim=1)

            if (ydf(j) - ydf(i) <= this%hparam%tolerance) exit
            ay2 = ay - y_(i)*a(i) - y_(j)*a(j)

            kii = kernel_mat(i, i)
            kjj = kernel_mat(j, j)
            kij = kernel_mat(i, j)

            kxi = kernel_mat(i,:)
            kxj = kernel_mat(j,:)

            ai = ((1 - y_(i)*y_(j) + y_(i)*((kij-kjj)*ay2 - sum( ays(:)*(kxi(:)-kxj(:)) )))) & 
                / (kii+kjj-2d0*kij)

            if (ai<0d0) then
                ai =0d0
            elseif (ai>this%hparam%c) then
                ai = this%hparam%c
            end if
            aj = (-ai*y_(i) - ay2) * y_(j)

            if (aj<0d0) then
                aj =0d0
                ai = (-aj*y_(j) - ay2) * y_(i)
            elseif (aj>this%hparam%c) then
                aj = this%hparam%c
                ai = (-aj*y_(j) - ay2) * y_(i)
            end if

            if (ai==a(i)) exit
            ay = ay + y_(i) * (ai-a(i)) + y_(j) * (aj-a(j))

            a(i) = ai
            a(j) = aj
            loop_cnt = loop_cnt + 1
        end do

        if (allocated(this%a_)) deallocate(this%a_)
        if (allocated(this%y_)) deallocate(this%y_)
        if (allocated(this%x_)) deallocate(this%x_)
        allocate(this%a_(n_samples))
        allocate(this%y_(n_samples))
        allocate(this%x_(n_samples, n_columns))
        this%a_ = a
        this%y_ = y_
        this%x_ = x

        is_non_zero = a(:)/=0d0
        idx_non_zero = pack(indices, mask=is_non_zero)
        n_non_zero = size(idx_non_zero)
        kernel_mat = this%krnl%eval(x(idx_non_zero,:), n_non_zero, n_columns, idx_non_zero, n_non_zero)
        ays = (a(idx_non_zero)*y_(idx_non_zero))

        deallocate(ydf)
        allocate(ydf(n_non_zero))
        ydf = 0d0
        call dgemv("n", n_non_zero, n_non_zero, &
                1d0, kernel_mat, n_non_zero, &
                ays, 1_8, 0d0, &
                ydf, 1_8)

        this%w0_ = sum(y_(idx_non_zero) - ydf) / dble(n_non_zero)

        ! print*, a
        ! print*, this%w0_
    end subroutine fit_kernel_svm_classifier_ver01
    
    
    ! １．a/=0以外も計算対象とする
    ! ２．Cacheを用いていない
    ! ３．Shrinkingを用いていない
    ! ４．ydfのアップデート効率が悪い <- 改善
    ! ５．FanのMethodを用いる
    subroutine fit_kernel_svm_classifier_ver02(this, x, y)
        implicit none
        class(kernel_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero, n_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:), ays(:)
        real(kind=8), allocatable :: ab(:)
        real(kind=8), allocatable :: kernel_mat(:,:)
        integer(kind=8), allocatable :: idx_non_zero(:), indices(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:), is_non_zero(:)
        real(kind=8) :: ay, ay2, ai, aj
        real(kind=8) :: kii, kjj, kij
        real(kind=8), allocatable :: kxi(:), kxj(:)

        integer(kind=8) :: loop_cnt, status, idx_hash, idx_hash_old
        type(svm_cache) :: cache

        this%krnl = rbf_kernel(x=x, sigma=this%hparam%sigma)

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
        allocate(kernel_mat(n_samples, n_samples), ays(n_samples))
        allocate(ab(n_samples))

        do s=1, n_samples, 1
            indices(s) = s
        end do
        loop_cnt = 0
        idx_hash = one_at_a_time_hash(indices, n_samples)
        idx_hash_old = idx_hash-1

        ydf(:) = y_(:)  
        call this%kernel%set_training_data(x)

        ays(:) = a(:) * y_(:)
        call dgemv("n", n_samples, n_samples, &
                1d0, kernel_mat, n_samples, &
                ays, 1_8, 0d0, &
                ydf, 1_8)
        ydf(:) = y_(:) - ydf(:)
        call this%kernel%compute_qmatrix(kernel_mat, n_samples)
        
        condition_i = ((y_(:)>0_8) .and. (a(:)>0d0) .or. ((y_(:)<0_8) .and. (a(:)<this%hparam%c)))
        condition_j = ((y_(:)<0_8) .and. (a(:)>0d0) .or. ((y_(:)>0_8) .and. (a(:)<this%hparam%c)))
        do while (t_)
            i = minloc(ydf, mask=condition_i, dim=1)
            j = maxloc(ydf, mask=condition_j, dim=1)

            if (ydf(j) - ydf(i) <= this%hparam%tolerance) exit
            ay2 = ay - y_(i)*a(i) - y_(j)*a(j)

            kii = kernel_mat(i, i)
            kjj = kernel_mat(j, j)
            kij = kernel_mat(i, j)

            call cache%query(i, 1_8, kxi, n_samples, loop_cnt, status, f_, f_, 1_8)
            if (status /= 0_8) then
                kxi = kernel_mat(:,i)
                call cache%update(i, 1_8, kxi, n_samples, loop_cnt)
            end if
                
            call cache%query(j, 1_8, kxj, n_samples, loop_cnt, status, f_, f_, 1_8)
            if (status /= 0_8) then
                kxj = kernel_mat(:,j)
                call cache%update(j, 1_8, kxj, n_samples, loop_cnt)
            end if

            ai = ((1 - y_(i)*y_(j) + y_(i)*((kij-kjj)*ay2 - sum( ays(:)*(kxi(:)-kxj(:)) )))) & 
                / (kii+kjj-2d0*kij)

            if (ai<0d0) then
                ai =0d0
            elseif (ai>this%hparam%c) then
                ai = this%hparam%c
            end if
            aj = (-ai*y_(i) - ay2) * y_(j)

            if (aj<0d0) then
                aj =0d0
                ai = (-aj*y_(j) - ay2) * y_(i)
            elseif (aj>this%hparam%c) then
                aj = this%hparam%c
                ai = (-aj*y_(j) - ay2) * y_(i)
            end if

            if (ai==a(i)) exit
            ay = ay + y_(i) * (ai-a(i)) + y_(j) * (aj-a(j))

            ydf(:) = ydf(:) - y_(i) * (ai-a(i)) * kxi(:) - y_(j) * (aj-a(j)) * kxj(:)

            a(i) = ai
            a(j) = aj

            ays(i) = a(i) * y_(i)
            ays(j) = a(j) * y_(j)
            condition_i(i) = ((y_(i)>0_8) .and. (a(i)>0d0) .or. ((y_(i)<0_8) .and. (a(i)<this%hparam%c)))
            condition_j(j) = ((y_(j)<0_8) .and. (a(j)>0d0) .or. ((y_(j)>0_8) .and. (a(j)<this%hparam%c)))

            loop_cnt = loop_cnt + 1
        end do

        if (allocated(this%a_)) deallocate(this%a_)
        if (allocated(this%y_)) deallocate(this%y_)
        if (allocated(this%x_)) deallocate(this%x_)
        allocate(this%a_(n_samples))
        allocate(this%y_(n_samples))
        allocate(this%x_(n_samples, n_columns))
        this%a_ = a
        this%y_ = y_
        this%x_ = x

        is_non_zero = a(:)/=0d0
        idx_non_zero = pack(indices, mask=is_non_zero)
        n_non_zero = size(idx_non_zero)
        kernel_mat = this%krnl%eval(x(idx_non_zero,:), n_non_zero, n_columns, idx_non_zero, n_non_zero)
        ays = (a(idx_non_zero)*y_(idx_non_zero))

        deallocate(ydf)
        allocate(ydf(n_non_zero))
        ydf = 0d0
        call dgemv("n", n_non_zero, n_non_zero, &
                1d0, kernel_mat, n_non_zero, &
                ays, 1_8, 0d0, &
                ydf, 1_8)

        this%w0_ = sum(y_(idx_non_zero) - ydf) / dble(n_non_zero)
        this%n_iter_ = loop_cnt
        ! print*, a
        ! print*, this%w0_
    end subroutine fit_kernel_svm_classifier_ver02
    
    
    ! １．a/=0以外も計算対象とする
    ! ２．Cacheを用いていない
    ! ３．Shrinkingを用いていない
    ! ４．ydfのアップデート効率が悪い <- 改善
    ! ５．FanのMethodを用いる <- 改善
    subroutine fit_kernel_svm_classifier_ver03(this, x, y)
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
        real(kind=8) :: kii, kjj, kij
        real(kind=8), allocatable :: kxi(:), kxj(:)

        integer(kind=8) :: loop_cnt, status, idx_hash, idx_hash_old
        type(svm_cache) :: cache

        this%krnl = rbf_kernel(x=x, sigma=this%hparam%sigma)

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
        allocate(kernel_mat(n_samples, n_samples), ays(n_samples))
        allocate(kernel_diag(n_samples))
        allocate(ab(n_samples))

        do s=1, n_samples, 1
            indices(s) = s
        end do
        loop_cnt = 0
        idx_hash = one_at_a_time_hash(indices, n_samples)
        idx_hash_old = idx_hash-1

        ydf(:) = y_(:)  
        call this%kernel%set_training_data(x)

        ays(:) = a(:) * y_(:)
        call dgemv("n", n_samples, n_samples, &
                1d0, kernel_mat, n_samples, &
                ays, 1_8, 0d0, &
                ydf, 1_8)
        ydf(:) = y_(:) - ydf(:)
        call this%kernel%compute_qmatrix(kernel_mat, n_samples)
        do i=1, n_samples, 1
            kernel_diag(i) = kernel_mat(i,i)
        end do
        
        condition_i = ((y_(:)>0_8) .and. (a(:)>0d0) .or. ((y_(:)<0_8) .and. (a(:)<this%hparam%c)))
        condition_j = ((y_(:)<0_8) .and. (a(:)>0d0) .or. ((y_(:)>0_8) .and. (a(:)<this%hparam%c)))
        do while (t_)
            i = minloc(ydf, mask=condition_i, dim=1)
            call cache%query(i, 1_8, kxi, n_samples, loop_cnt, status, f_, f_, 1_8)
            if (status /= 0_8) then
                kxi = kernel_mat(:,i)
                call cache%update(i, 1_8, kxi, n_samples, loop_cnt)
            end if

            ab = (ydf - ydf(i))**2d0 / (kernel_diag(i) + kernel_diag(:) - 2d0*kxi)
            j = maxloc(ab, mask=condition_j .and. ( ydf > ydf(i)), dim=1)
            call cache%query(j, 1_8, kxj, n_samples, loop_cnt, status, f_, f_, 1_8)
            if (status /= 0_8) then
                kxj = kernel_mat(:,j)
                call cache%update(j, 1_8, kxj, n_samples, loop_cnt)
            end if

            if (ydf(j) - ydf(i) <= this%hparam%tolerance) exit
            ay2 = ay - y_(i)*a(i) - y_(j)*a(j)

            kii = kernel_mat(i, i)
            kjj = kernel_mat(j, j)
            kij = kernel_mat(i, j)

            ai = ((1 - y_(i)*y_(j) + y_(i)*((kij-kjj)*ay2 - sum( ays(:)*(kxi(:)-kxj(:)) )))) & 
                / (kii+kjj-2d0*kij)

            if (ai<0d0) then
                ai =0d0
            elseif (ai>this%hparam%c) then
                ai = this%hparam%c
            end if
            aj = (-ai*y_(i) - ay2) * y_(j)

            if (aj<0d0) then
                aj =0d0
                ai = (-aj*y_(j) - ay2) * y_(i)
            elseif (aj>this%hparam%c) then
                aj = this%hparam%c
                ai = (-aj*y_(j) - ay2) * y_(i)
            end if

            if (ai==a(i)) exit
            ay = ay + y_(i) * (ai-a(i)) + y_(j) * (aj-a(j))

            ydf(:) = ydf(:) - y_(i) * (ai-a(i)) * kxi(:) - y_(j) * (aj-a(j)) * kxj(:)

            a(i) = ai
            a(j) = aj

            ays(i) = a(i) * y_(i)
            ays(j) = a(j) * y_(j)
            condition_i(i) = ((y_(i)>0_8) .and. (a(i)>0d0) .or. ((y_(i)<0_8) .and. (a(i)<this%hparam%c)))
            condition_j(j) = ((y_(j)<0_8) .and. (a(j)>0d0) .or. ((y_(j)>0_8) .and. (a(j)<this%hparam%c)))

            loop_cnt = loop_cnt + 1
        end do

        if (allocated(this%a_)) deallocate(this%a_)
        if (allocated(this%y_)) deallocate(this%y_)
        if (allocated(this%x_)) deallocate(this%x_)
        allocate(this%a_(n_samples))
        allocate(this%y_(n_samples))
        allocate(this%x_(n_samples, n_columns))
        this%a_ = a
        this%y_ = y_
        this%x_ = x

        is_non_zero = a(:)/=0d0
        idx_non_zero = pack(indices, mask=is_non_zero)
        n_non_zero = size(idx_non_zero)
        kernel_mat = this%krnl%eval(x(idx_non_zero,:), n_non_zero, n_columns, idx_non_zero, n_non_zero)
        ays = (a(idx_non_zero)*y_(idx_non_zero))

        deallocate(ydf)
        allocate(ydf(n_non_zero))
        ydf = 0d0
        call dgemv("n", n_non_zero, n_non_zero, &
                1d0, kernel_mat, n_non_zero, &
                ays, 1_8, 0d0, &
                ydf, 1_8)

        this%w0_ = sum(y_(idx_non_zero) - ydf) / dble(n_non_zero)

        this%n_iter_ = loop_cnt 
        ! print*, a
        ! print*, this%w0_
    end subroutine fit_kernel_svm_classifier_ver03
    
    
    ! １．a/=0以外も計算対象とする
    ! ２．Cacheを用いていない <- 改善
    ! ３．Shrinkingを用いていない
    ! ４．ydfのアップデート効率が悪い <- 改善
    ! ５．FanのMethodを用いる <- 改善
    ! ６．PostProcessの高速化、保持データについても
    ! ７．Predictの高速化
    subroutine fit_kernel_svm_classifier_ver04(this, x, y)
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
        real(kind=8) :: kii, kjj, kij
        real(kind=8), allocatable :: kxi(:), kxj(:)

        integer(kind=8) :: loop_cnt, status, idx_hash, idx_hash_old
        type(svm_cache) :: cache

        this%krnl = rbf_kernel(x=x, sigma=this%hparam%sigma)

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
        allocate(kxi(n_samples), kxj(n_samples))

        do s=1, n_samples, 1
            indices(s) = s
        end do
        loop_cnt = 0
        idx_hash = one_at_a_time_hash(indices, n_samples)
        idx_hash_old = idx_hash-1

        ydf(:) = y_(:)  
        call this%kernel%set_training_data(x)

        ays(:) = 0d0
        ydf(:) = y_(:)
        call this%kernel%compute_diagonal_elements(kernel_diag, n_samples)
        
        condition_i = ((y_(:)>0_8) .and. (a(:)>0d0) .or. ((y_(:)<0_8) .and. (a(:)<this%hparam%c)))
        condition_j = ((y_(:)<0_8) .and. (a(:)>0d0) .or. ((y_(:)>0_8) .and. (a(:)<this%hparam%c)))
        do while (t_)
            i = minloc(ydf, mask=condition_i, dim=1)
            call cache%query(i, 1_8, kxi, n_samples, loop_cnt, status, f_, f_, 1_8)
            if (status /= 0_8) then
                call this%kernel%compute_kth_row(i, kxi, n_samples)
                call cache%update(i, 1_8, kxi, n_samples, loop_cnt)
            end if
            
            ab = (ydf - ydf(i))**2d0 / (kernel_diag(i) + kernel_diag(:) - 2d0*kxi)
            j = maxloc(ab, mask=condition_j .and. ( ydf > ydf(i)), dim=1)
            call cache%query(j, 1_8, kxj, n_samples, loop_cnt, status, f_, f_, 1_8)
            if (status /= 0_8) then
                call this%kernel%compute_kth_row(j, kxj, n_samples)
                call cache%update(j, 1_8, kxj, n_samples, loop_cnt)
            end if
            
            if (ydf(j) - ydf(i) <= this%hparam%tolerance) exit
            ay2 = ay - y_(i)*a(i) - y_(j)*a(j)
            
            kii = kernel_diag(i)
            kjj = kernel_diag(j)
            kij = kxi(j)
            
            ai = ((1 - y_(i)*y_(j) + y_(i)*((kij-kjj)*ay2 - sum( ays(:)*(kxi(:)-kxj(:)) )))) & 
                / (kii+kjj-2d0*kij)

            if (ai<0d0) then
                ai =0d0
            elseif (ai>this%hparam%c) then
                ai = this%hparam%c
            end if
            aj = (-ai*y_(i) - ay2) * y_(j)

            if (aj<0d0) then
                aj =0d0
                ai = (-aj*y_(j) - ay2) * y_(i)
            elseif (aj>this%hparam%c) then
                aj = this%hparam%c
                ai = (-aj*y_(j) - ay2) * y_(i)
            end if

            if (ai==a(i)) exit
            ay = ay + y_(i) * (ai-a(i)) + y_(j) * (aj-a(j))

            ydf(:) = ydf(:) - y_(i) * (ai-a(i)) * kxi(:) - y_(j) * (aj-a(j)) * kxj(:)

            a(i) = ai
            a(j) = aj


            ays(i) = a(i) * y_(i)
            ays(j) = a(j) * y_(j)
            condition_i(i) = ((y_(i)>0_8) .and. (a(i)>0d0) .or. ((y_(i)<0_8) .and. (a(i)<this%hparam%c)))
            condition_j(j) = ((y_(j)<0_8) .and. (a(j)>0d0) .or. ((y_(j)>0_8) .and. (a(j)<this%hparam%c)))

            loop_cnt = loop_cnt + 1
        end do

        if (allocated(this%a_)) deallocate(this%a_)
        if (allocated(this%y_)) deallocate(this%y_)
        if (allocated(this%x_)) deallocate(this%x_)
        allocate(this%a_(n_samples))
        allocate(this%y_(n_samples))
        allocate(this%x_(n_samples, n_columns))
        this%a_ = a
        this%y_ = y_
        this%x_ = x
        ! print*, "done"
        is_non_zero = a(:)/=0d0
        idx_non_zero = pack(indices, mask=is_non_zero)
        n_non_zero = size(idx_non_zero)
        allocate(kernel_mat(n_non_zero, n_non_zero))
        kernel_mat = this%krnl%eval(x(idx_non_zero,:), n_non_zero, n_columns, idx_non_zero, n_non_zero)
        ays = (a(idx_non_zero)*y_(idx_non_zero))

        deallocate(ydf)
        allocate(ydf(n_non_zero))
        ydf = 0d0
        call dgemv("n", n_non_zero, n_non_zero, &
                1d0, kernel_mat, n_non_zero, &
                ays, 1_8, 0d0, &
                ydf, 1_8)

        this%w0_ = sum(y_(idx_non_zero) - ydf) / dble(n_non_zero)

        this%n_iter_ = loop_cnt 
        ! print*, a
        ! print*, this%w0_
    end subroutine fit_kernel_svm_classifier_ver04
    
    
    ! １．a/=0以外も計算対象とする
    ! ２．Cacheを用いていない <- 改善
    ! ３．Shrinkingを用いていない <- 改善
    ! ４．ydfのアップデート効率が悪い <- 改善
    ! ５．FanのMethodを用いる <- 改善
    ! ６．PostProcessの高速化、保持データについても
    ! ７．Predictの高速化
    subroutine fit_kernel_svm_classifier_ver05(this, x, y)
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


        this%krnl = rbf_kernel(x=x, sigma=this%hparam%sigma)

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
            ! j = maxloc(ydf_, mask=condition_j_, dim=1)
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
            ! print*, loop_cnt, ydf_(j) - ydf_(i), ydf_(j) - ydf_(i) <= this%hparam%tolerance
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
                    ! print*, " first shrinkage", ydf_(j) - ydf_(i)
                    is_aggressive = f_
                    loop_cnt_base = loop_cnt
                    loop_cnt = loop_cnt + 1
                end if
            else
                if (mod(loop_cnt-loop_cnt_base, minval([1000_8, n_samples]))==0) then
                    ! print*, " second ... shrinkage", ydf_(j) - ydf_(i)
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

        if (allocated(this%a_)) deallocate(this%a_)
        if (allocated(this%y_)) deallocate(this%y_)
        if (allocated(this%x_)) deallocate(this%x_)
        allocate(this%a_(n_samples))
        allocate(this%y_(n_samples))
        allocate(this%x_(n_samples, n_columns))
        this%a_ = a
        this%y_ = y_
        this%x_ = x
        ! print*, "done"
        ! idx_non_zero = indices
        ! n_non_zero = size(indices)
        idx_non_zero = pack(indices, mask=a(:)/=0d0)
        n_non_zero = count(a(:)/=0d0)
        allocate(kernel_mat(n_non_zero, n_non_zero))
        kernel_mat = this%krnl%eval(x(idx_non_zero,:), n_non_zero, n_columns, idx_non_zero, n_non_zero)
        ays = (a(idx_non_zero)*y_(idx_non_zero))

        deallocate(ydf)
        allocate(ydf(n_non_zero))
        ydf = 0d0
        call dgemv("n", n_non_zero, n_non_zero, &
                1d0, kernel_mat, n_non_zero, &
                ays, 1_8, 0d0, &
                ydf, 1_8)

        this%w0_ = sum(y_(idx_non_zero) - ydf) / dble(n_non_zero)

        this%n_iter_ = loop_cnt 
        ! print*, a
        ! print*, this%w0_
    end subroutine fit_kernel_svm_classifier_ver05

    !> Predict 'linear_svm_classifier'
    !! \param x input data
    function predict_kernel_svm_classifier(this, x) result(labels)
        implicit none
        class(kernel_svm_classifier) :: this
        real(kind=8), intent(in)     :: x(:,:)
        integer(kind=8), allocatable :: labels(:,:)
        integer(kind=8) :: n_samples, n_columns

        real(kind=8), allocatable :: tmp_(:)
        integer(kind=8) :: n_non_zero
        real(kind=8), allocatable :: kernel_mat(:,:), ays(:), ydf(:)
        integer(kind=8), allocatable :: idx_non_zero(:), indices(:)
        logical(kind=1), allocatable :: is_non_zero(:)

        allocate(indices(this%krnl%n_samples))
        do n_non_zero=1, this%krnl%n_samples, 1
            indices(n_non_zero) = n_non_zero
        end do

        allocate(is_non_zero(size(this%a_)))
        is_non_zero = this%a_(:)/=0d0
        idx_non_zero = pack(indices, mask=is_non_zero)
        n_non_zero = size(idx_non_zero)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

        if (allocated(labels)) deallocate(labels)
        allocate(labels(n_samples, 1))
        labels = 0_8
        
        if (allocated(tmp_)) deallocate(tmp_)
        allocate(tmp_(n_samples))

        kernel_mat = this%krnl%eval(x, n_samples, n_columns, idx_non_zero, n_non_zero)
        ays = (this%a_(idx_non_zero)*this%y_(idx_non_zero))

        allocate(ydf(n_samples))
        call dgemv("n", n_samples, n_non_zero, &
                1d0, kernel_mat, n_samples, &
                ays, 1_8, 0d0, &
                ydf, 1_8)

        tmp_(:) = this%w0_ + ydf(:)
        labels(:,1) = tmp_ / abs(tmp_)
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