module mod_linear_svm
    use mod_const
    use mod_common
    use mod_sort
    use mod_hyperparameter
    implicit none

    !> Linear SVM Classifier, Not yet optimized.
    type linear_svm_classifier
        type(hparam_linear_svm_classifier) :: hparam
        real(kind=8), allocatable :: w_(:)
        real(kind=8), allocatable :: w0_
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

contains


    !> A function to create new 'linear_svm_classifier'.
    !! \param c regularization parameter
    function new_linear_svm_classifier(c)
        implicit none
        type(linear_svm_classifier) :: new_linear_svm_classifier
        real(kind=8), optional :: c

        if (present(c)) new_linear_svm_classifier%hparam%c = c
    end function new_linear_svm_classifier

    
    !> Predict 'linear_svm_classifier'
    !! \param x input data
    function predict_linear_svm_classifier(self, x) result(labels)
        implicit none
        class(linear_svm_classifier) :: self
        real(kind=8), intent(in)     :: x(:,:)
        integer(kind=8), allocatable :: labels(:,:)
        integer(kind=8) :: n_samples, n_columns

        real(kind=8), allocatable :: tmp_(:)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

        allocate(labels(n_samples, 1))
        labels = 0_8
        
        allocate(tmp_(n_samples))
        tmp_(:) = self%w0_

        call dgemv("n", n_samples, n_columns, &
                1d0, x, n_samples, &
                self%w_, 1_8, 1d0, &
                tmp_, 1_8)
        labels(:,1) = tmp_ / abs(tmp_)
        labels(:,1) = (labels(:,1)+1)*0.5d0
    end function predict_linear_svm_classifier


    !> Fit linear_svm_classifier. binary classification only
    !! \param x input data
    !! \param y labels
    subroutine fit_linear_svm_classifier(self, x, y)
        implicit none
        class(linear_svm_classifier) :: self
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:)
        real(kind=8) :: ay, ay2, ai, aj

        call self%transform_y(y, y_)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

        allocate(a(n_samples)); a(:) = 0d0
        ay = 0d0; ay2=0d0
        allocate(ayx(n_columns), ayx2(n_columns)); ayx(:) = 0d0; ayx2(:) = 0d0
        allocate(yx, source=x)
        allocate(ydf(n_samples))
        allocate(condition_i(n_samples), condition_j(n_samples))

        do s=1, n_samples, 1
            yx(s,:) = y_(s) * yx(s,:)
        end do

        do while (t_)
            call dgemv("n", n_samples, n_columns, &
                    1d0, yx, n_samples, &
                    ayx, 1_8, 0d0, &
                    ydf, 1_8)
            ydf = y_(:) * (1d0 - ydf)

            condition_i = ((y_(:)>0d0) .and. (a(:)>0d0) .or. ((y_(:)<0d0) .and. (a(:)<self%hparam%c)))
            i = minloc(ydf, mask=condition_i, dim=1)
            
            condition_i = ((y_(:)<0d0) .and. (a(:)>0d0) .or. ((y_(:)>0d0) .and. (a(:)<self%hparam%c)))
            j = maxloc(ydf, mask=condition_j, dim=1)

            if (ydf(i) >= ydf(j)) exit

            ay2 = ay - y_(i)*a(i) - y_(j)*a(j)
            ayx2 = ayx - y_(i)*a(i)*x(i,:) - y_(j)*a(j)*x(j,:)

            ai = (1d0 &
                - y_(i)*y_(j) &
                + y_(i) &
                    * sum(&
                            (x(i,:)-x(j,:)) * (x(j,:)*ay2 - ayx2) &
                        ) &
                    ) / sum((x(i,:)-x(j,:))**2d0)

            if (ai<0d0) then
                ai =0d0
            elseif (ai>self%hparam%c) then
                ai = self%hparam%c
            end if
            aj = (-ai*y_(i) - ay2) * y_(j)

            if (aj<0d0) then
                aj =0d0
                ai = (-aj*y_(j) - ay2) * y_(i)
            elseif (aj>self%hparam%c) then
                aj = self%hparam%c
                ai = (-aj*y_(j) - ay2) * y_(i)
            end if

            ay = ay   + y_(i) * (ai-a(i))        + y_(j) * (aj-a(j))
            ayx = ayx + y_(i) * (ai-a(i))*x(i,:) + y_(j) * (aj-a(j))*x(j,:)

            if (ai==a(i)) exit

            a(i) = ai
            a(j) = aj
        end do

        allocate(self%w_(n_columns))
        self%w_ = 0d0
        self%w0_ = 0d0
        count_non_zero = 0
        do s=1, n_samples
            if (a(s)>0d0) then
                self%w_  = self%w_  + (a(s)*y_(s)) * x(s,:)
                count_non_zero = count_non_zero + 1
            end if
        end do
        do s=1, n_samples
            if (a(s)>0d0) then
                self%w0_ = self%w0_ + (y_(s) - sum(x(s,:) * self%w_))
            end if
        end do
        self%w0_ = self%w0_ /dble(count_non_zero)
    end subroutine fit_linear_svm_classifier


    !> Fit linear_svm_classifier. binary classification only. hard margin
    !! \param x input data
    !! \param y labels
    subroutine fit_linear_svm_classifier_hard(self, x, y)
        implicit none
        class(linear_svm_classifier) :: self
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)

        integer(kind=8), allocatable :: y_trans(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:)
        real(kind=8) :: ay, ay2, ai, aj

        call self%transform_y(y, y_trans)

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

            ay2 = ay - y_trans(i)*a(i) - y_trans(j)*a(j)
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

        allocate(self%w_(n_columns))
        self%w_ = 0d0
        self%w0_ = 0d0
        count_non_zero = 0
        do s=1, n_samples
            if (a(s)>0d0) then
                self%w_  = self%w_  + (a(s)*y_trans(s)) * x(s,:)
                count_non_zero = count_non_zero + 1
            end if
        end do
        do s=1, n_samples
            if (a(s)>0d0) then
                self%w0_ = self%w0_ + (y_trans(s) - sum(x(s,:) * self%w_))
            end if
        end do
        self%w0_ = self%w0_ /dble(count_non_zero)

        print*, self%w_
        print*, self%w0_
    end subroutine fit_linear_svm_classifier_hard


    !> Transform input labels for svm format.
    !! \param y input labels
    !! \param y_trans transformed labels
    subroutine transform_y(self, y, y_trans)
        implicit none
        class(linear_svm_classifier) :: self
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