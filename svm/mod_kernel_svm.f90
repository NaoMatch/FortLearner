module mod_kernel_svm
    use mod_sort
    use mod_hyperparameter
    use mod_kernel
    implicit none
    
    type kernel_svm_classifier
        real(kind=8), allocatable :: a_(:)
        real(kind=8), allocatable :: y_(:)
        real(kind=8), allocatable :: x_(:,:)
        real(kind=8) :: w0_        
        type(hparam_kernel_svm_classifier) :: hparam
        type(rbf_kernel) :: krnl
    contains
        procedure :: fit => fit_kernel_svm_classifier
        procedure :: predict => predict_kernel_svm_classifier

        procedure :: transform_y
    end type kernel_svm_classifier

    interface kernel_svm_classifier
        procedure :: new_kernel_svm_classifier
    end interface kernel_svm_classifier

contains

    function new_kernel_svm_classifier(c, sigma) result(svm)
        implicit none
        type(kernel_svm_classifier) :: svm
        real(kind=8), optional :: c
        real(kind=8), optional :: sigma

        if (present(c)) svm%hparam%c = c
        if (present(sigma)) svm%hparam%sigma = sigma
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

        this%krnl = rbf_kernel(x=x, sigma=this%hparam%sigma)

        call this%transform_y(y, y_)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

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
            if (ydf(i) >= ydf(j)) exit

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
        end do

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

        print*, a
        print*, this%w0_

    end subroutine fit_kernel_svm_classifier

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
        allocate(ydf(this%krnl%n_samples))
        do n_non_zero=1, this%krnl%n_samples, 1
            indices(n_non_zero) = n_non_zero
        end do

        allocate(is_non_zero(size(this%a_)))
        is_non_zero = this%a_(:)/=0d0
        idx_non_zero = pack(indices, mask=is_non_zero)
        n_non_zero = size(idx_non_zero)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

        allocate(labels(n_samples, 1))
        labels = 0_8
        
        allocate(tmp_(n_samples))

        kernel_mat = this%krnl%eval(x, n_samples, n_columns, idx_non_zero, n_non_zero)
        ays = (this%a_(idx_non_zero)*this%y_(idx_non_zero))

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