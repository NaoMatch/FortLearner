module mod_linear_svm
    use mod_const
    use mod_common
    use mod_sort
    use mod_hyperparameter
    use mod_timer
    use mod_linalg
    implicit none

    !> Linear SVM Classifier, Not yet optimized.
    type linear_svm_classifier
        type(hparam_linear_svm_classifier) :: hparam
        real(kind=8), allocatable :: w_(:)
        real(kind=8), allocatable :: w0_
    contains
        procedure :: fit => fit_linear_svm_classifier
        procedure :: fit_linear_svm_classifier_ver02
        procedure :: fit_linear_svm_classifier_ver03
        procedure :: fit_linear_svm_classifier_ver04
        procedure :: fit_linear_svm_classifier_ver05
        procedure :: fit_linear_svm_classifier_ver06
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
    subroutine fit_linear_svm_classifier(this, x, y, times)
        implicit none
        class(linear_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)
        integer(kind=8) :: times(:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:)
        real(kind=8) :: ay, ay2, ai, aj
        integer(kind=8)        :: date_value1(8), date_value2(8), cnt

        ! call date_and_time(values=date_value1)
        if (allocated(this%w_)) deallocate(this%w_)

        call this%transform_y(y, y_)

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
        ! call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)

        cnt = 0
        do while (t_)
            ! call date_and_time(values=date_value1)
            call dgemv("n", n_samples, n_columns, &
                    1d0, yx, n_samples, &
                    ayx, 1_8, 0d0, &
                    ydf, 1_8)
            ydf = y_(:) * (1d0 - ydf)
            ! call date_and_time(values=date_value2)
            ! times(1+1) = times(1+1) + time_diff(date_value1, date_value2)


            ! call date_and_time(values=date_value1)
            condition_i = ((y_(:)>0d0) .and. (a(:)>0d0) .or. ((y_(:)<0d0) .and. (a(:)<this%hparam%c)))
            i = minloc(ydf, mask=condition_i, dim=1)
            
            condition_j = ((y_(:)<0d0) .and. (a(:)>0d0) .or. ((y_(:)>0d0) .and. (a(:)<this%hparam%c)))
            j = maxloc(ydf, mask=condition_j, dim=1)
            ! call date_and_time(values=date_value2)
            ! times(2+1) = times(2+1) + time_diff(date_value1, date_value2)
            ! print*, cnt, i, j, count(condition_i), count(condition_j), sum(ydf), count(a>0)

            if (ydf(i) >= ydf(j)) exit

            ! call date_and_time(values=date_value1)
            ay2 = ay - y_(i)*a(i) - y_(j)*a(j)
            ayx2 = ayx - y_(i)*a(i)*x(i,:) - y_(j)*a(j)*x(j,:)
            ! call date_and_time(values=date_value2)
            ! times(3+1) = times(3+1) + time_diff(date_value1, date_value2)

            ! call date_and_time(values=date_value1)
            ai = (1d0 &
                - y_(i)*y_(j) &
                + y_(i) &
                    * sum(&
                            (x(i,:)-x(j,:)) * (x(j,:)*ay2 - ayx2) &
                        ) &
                    ) / sum((x(i,:)-x(j,:))**2d0)

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
            ! call date_and_time(values=date_value2)
            ! times(4+1) = times(4+1) + time_diff(date_value1, date_value2)

            ! call date_and_time(values=date_value1)
            ay = ay   + y_(i) * (ai-a(i))        + y_(j) * (aj-a(j))
            ayx = ayx + y_(i) * (ai-a(i))*x(i,:) + y_(j) * (aj-a(j))*x(j,:)

            if (ai==a(i)) exit

            a(i) = ai
            a(j) = aj
            ! call date_and_time(values=date_value2)
            ! times(5+1) = times(5+1) + time_diff(date_value1, date_value2)
            cnt = cnt + 1
            ! if (cnt>10) exit
        end do

        ! call date_and_time(values=date_value1)
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
        ! call date_and_time(values=date_value2)
        ! times(6+1) = times(6+1) + time_diff(date_value1, date_value2)

        times(8) = cnt
    end subroutine fit_linear_svm_classifier


    !> Fit linear_svm_classifier. binary classification only
    !! \param x input data
    !! \param y labels
    subroutine fit_linear_svm_classifier_ver02(this, x, y, times)
        implicit none
        class(linear_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)
        integer(kind=8) :: times(:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:)
        real(kind=8) :: ay, ay2, ai, aj
        real(kind=8) :: ya_i, ya_j
        real(kind=8) :: y_i, y_j
        real(kind=8), allocatable :: x_i(:), x_j(:), x_ij(:)
        logical(kind=8), allocatable :: y_posi(:), y_nega(:)
        logical(kind=8), allocatable :: a_posi(:), a_pena(:)

        integer(kind=8)        :: date_value1(8), date_value2(8), cnt

        ! call date_and_time(values=date_value1)
        if (allocated(this%w_)) deallocate(this%w_)

        call this%transform_y(y, y_)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)
        allocate(x_i(n_columns))
        allocate(x_j(n_columns))
        allocate(x_ij(n_columns))

        allocate(a(n_samples)); a(:) = 0d0
        ay = 0d0; ay2=0d0
        allocate(ayx(n_columns), ayx2(n_columns)); ayx(:) = 0d0; ayx2(:) = 0d0
        allocate(yx, source=x)
        allocate(ydf(n_samples))
        allocate(condition_i(n_samples), condition_j(n_samples))

        do s=1, n_samples, 1
            yx(s,:) = y_(s) * yx(s,:)
        end do

        allocate(y_posi(n_samples))
        allocate(y_nega(n_samples))
        allocate(a_posi(n_samples))
        allocate(a_pena(n_samples))

        y_posi = y_(:) > 0d0
        y_nega = .not. y_posi
        a_posi = f_
        a_pena = t_
        condition_i = ((y_posi) .and. (a_posi) .or. ((y_nega) .and. (a_pena)))
        condition_j = ((y_nega) .and. (a_posi) .or. ((y_posi) .and. (a_pena)))
        cnt = 0
        ! call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)
        do while (t_)
            ! call date_and_time(values=date_value1)
            ! call dgemv("n", n_samples, n_columns, &
            !         1d0, yx, n_samples, &
            !         ayx, 1_8, 0d0, &
            !         ydf, 1_8)
            call multi_mat_vec(yx, ayx, ydf, n_samples, n_columns)
            ydf(:) = y_(:) * (1d0 - ydf(:))
            ! call date_and_time(values=date_value2)
            ! times(1+1) = times(1+1) + time_diff(date_value1, date_value2)

            ! call date_and_time(values=date_value1)
            i = minloc(ydf, mask=condition_i, dim=1)            
            j = maxloc(ydf, mask=condition_j, dim=1)
            ! call date_and_time(values=date_value2)
            ! times(2+1) = times(2+1) + time_diff(date_value1, date_value2)
            ! print*, cnt, i, j, count(condition_i), count(condition_j), sum(ydf), count(a>0)

            ! if (ydf(i) >= ydf(j)) exit
            if (abs(ydf(i) - ydf(j)) <= 1d-3) exit

            ! call date_and_time(values=date_value1)
            y_i = y_(i)
            y_j = y_(j)
            ya_i=y_i*a(i) 
            ya_j=y_j*a(j)
            x_i(:) = x(i,:)
            x_j(:) = x(j,:)
            x_ij(:) = x_i - x_j

            ay2  = ay  - ya_i        - ya_j
            ayx2 = ayx - ya_i*x_i(:) - ya_j*x_j(:)
            ! call date_and_time(values=date_value2)
            ! times(3+1) = times(3+1) + time_diff(date_value1, date_value2)

            ! call date_and_time(values=date_value1)
            ai = (1d0 &
                - y_i*y_j &
                + y_i &
                    * sum( &
                            x_ij(:) * (x_j(:)*ay2 - ayx2) &
                        ) &
                    ) / sum((x_ij(:))**2d0)

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
            ! call date_and_time(values=date_value2)
            ! times(4+1) = times(4+1) + time_diff(date_value1, date_value2)


            ! call date_and_time(values=date_value1)
            ay  = ay  + y_i * (ai-a(i))        + y_j * (aj-a(j))
            ayx = ayx + y_i * (ai-a(i))*x_i(:) + y_j * (aj-a(j))*x_j(:)

            if (ai==a(i)) exit

            a(i) = ai
            a(j) = aj

            a_posi(i) =  ai > 0d0
            a_posi(j) =  aj > 0d0
            a_pena(i) =  ai < this%hparam%c
            a_pena(j) =  aj < this%hparam%c

            condition_i(i) = (y_posi(i) .and. a_posi(i)) .or. (y_nega(i) .and. a_pena(i))
            condition_i(j) = (y_posi(j) .and. a_posi(j)) .or. (y_nega(j) .and. a_pena(j))

            condition_j(i) = (y_nega(i) .and. a_posi(i)) .or. (y_posi(i) .and. a_pena(i))
            condition_j(j) = (y_nega(j) .and. a_posi(j)) .or. (y_posi(j) .and. a_pena(j))
            ! call date_and_time(values=date_value2)
            ! times(5+1) = times(5+1) + time_diff(date_value1, date_value2)
            cnt = cnt + 1
            ! if (cnt>10) exit
        end do
        ! print*, "LoopCount:", cnt
        times(8) = cnt

        ! call date_and_time(values=date_value1)
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
        ! call date_and_time(values=date_value2)
        ! times(6+1) = times(6+1) + time_diff(date_value1, date_value2)
    end subroutine fit_linear_svm_classifier_ver02


    !> Fit linear_svm_classifier. binary classification only
    !! \param x input data
    !! \param y labels
    subroutine fit_linear_svm_classifier_ver03(this, x, y, times)
        implicit none
        class(linear_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)
        integer(kind=8) :: times(:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:)
        real(kind=8) :: ay, ay2, ai, aj
        real(kind=8) :: ya_i, ya_j
        real(kind=8) :: y_i, y_j, ydf_i
        real(kind=8), allocatable :: x_i(:), x_j(:), x_ij(:)
        real(kind=8), allocatable :: x_sq_sum(:)
        real(kind=8), allocatable :: ab(:), dist_mat(:,:), krnl_mat(:,:)
        logical(kind=8), allocatable :: y_posi(:), y_nega(:)
        logical(kind=8), allocatable :: a_posi(:), a_pena(:)

        integer(kind=8)        :: date_value1(8), date_value2(8), cnt, n

        ! call date_and_time(values=date_value1)
        if (allocated(this%w_)) deallocate(this%w_)

        call this%transform_y(y, y_)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)
        allocate(x_i(n_columns))
        allocate(x_j(n_columns))
        allocate(x_ij(n_columns))

        allocate(a(n_samples)); a(:) = 0d0
        ay = 0d0; ay2=0d0
        allocate(ayx(n_columns), ayx2(n_columns)); ayx(:) = 0d0; ayx2(:) = 0d0
        allocate(yx, source=x)
        allocate(ydf(n_samples))
        allocate(ab(n_samples))
        allocate(x_sq_sum(n_samples))
        allocate(dist_mat(n_samples, n_samples))
        allocate(condition_i(n_samples), condition_j(n_samples))

        do s=1, n_samples, 1
            yx(s,:) = y_(s) * yx(s,:)
        end do

        allocate(y_posi(n_samples))
        allocate(y_nega(n_samples))
        allocate(a_posi(n_samples))
        allocate(a_pena(n_samples))

        y_posi = y_(:) > 0d0
        y_nega = .not. y_posi
        a_posi = f_
        a_pena = t_
        condition_i = ((y_posi) .and. (a_posi) .or. ((y_nega) .and. (a_pena)))
        condition_j = ((y_nega) .and. (a_posi) .or. ((y_posi) .and. (a_pena)))
        cnt = 0

        call matrix_sqsum_row(x, x_sq_sum, n_samples, n_columns)
        do j=1, n_samples, 1
            do i=1, n_samples, 1
                dist_mat(i,j) = x_sq_sum(i) + x_sq_sum(j)
            end do
        end do

        call dgemm("N", "T", & 
                    n_samples, n_samples, n_columns, &
                    -2d0, & 
                    x, n_samples, &
                    x, n_samples, &
                    1d0, &
                    dist_mat, n_samples)
        dist_mat = 1d0 / dist_mat

        ! call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)

        do while (t_)
            ! call date_and_time(values=date_value1)
            call multi_mat_vec(yx, ayx, ydf, n_samples, n_columns)
            ydf(:) = y_(:) * (1d0 - ydf(:))
            ! call date_and_time(values=date_value2)
            ! times(1+1) = times(1+1) + time_diff(date_value1, date_value2)

            ! call date_and_time(values=date_value1)
            i = minloc(ydf, mask=condition_i, dim=1)
            ydf_i = ydf(i)
            ab(:) = (ydf - ydf_i)**2d0 * dist_mat(:,i)
            j = maxloc(ab, mask=(condition_j .and. ( ydf > ydf_i)), dim=1)
            ! call date_and_time(values=date_value2)
            ! times(2+1) = times(2+1) + time_diff(date_value1, date_value2)

            ! print*, cnt, i, j, count(condition_i), count(condition_j), sum(ydf), sum(y_*a)
            ! if (ydf(i) >= ydf(j)) exit
            if (abs(ydf(i) - ydf(j)) <= 1d-3) exit

            ! call date_and_time(values=date_value1)
            y_i = y_(i)
            y_j = y_(j)
            ya_i=y_i*a(i) 
            ya_j=y_j*a(j)
            x_i(:) = x(i,:)
            x_j(:) = x(j,:)
            x_ij(:) = x_i - x_j

            ay2  = ay  - ya_i        - ya_j
            ayx2 = ayx - ya_i*x_i(:) - ya_j*x_j(:)
            call date_and_time(values=date_value2)
            ! times(3+1) = times(3+1) + time_diff(date_value1, date_value2)

            call date_and_time(values=date_value1)
            ai = (1d0 &
                - y_i*y_j &
                + y_i &
                    * sum( &
                            x_ij(:) * (x_j(:)*ay2 - ayx2) &
                        ) &
                    ) / sum((x_ij(:))**2d0)

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
            call date_and_time(values=date_value2)
            ! times(4+1) = times(4+1) + time_diff(date_value1, date_value2)


            call date_and_time(values=date_value1)
            ay  = ay  + y_i * (ai-a(i))        + y_j * (aj-a(j))
            ayx = ayx + y_i * (ai-a(i))*x_i(:) + y_j * (aj-a(j))*x_j(:)

            if (ai==a(i)) exit

            a(i) = ai
            a(j) = aj

            a_posi(i) =  ai > 0d0
            a_posi(j) =  aj > 0d0
            a_pena(i) =  ai < this%hparam%c
            a_pena(j) =  aj < this%hparam%c

            condition_i(i) = (y_posi(i) .and. a_posi(i)) .or. (y_nega(i) .and. a_pena(i))
            condition_i(j) = (y_posi(j) .and. a_posi(j)) .or. (y_nega(j) .and. a_pena(j))

            condition_j(i) = (y_nega(i) .and. a_posi(i)) .or. (y_posi(i) .and. a_pena(i))
            condition_j(j) = (y_nega(j) .and. a_posi(j)) .or. (y_posi(j) .and. a_pena(j))
            call date_and_time(values=date_value2)
            ! times(5+1) = times(5+1) + time_diff(date_value1, date_value2)
            cnt = cnt + 1
            ! if (cnt>50) exit
        end do
        ! print*, "LoopCount:", cnt
        times(8) = cnt

        call date_and_time(values=date_value1)
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
        call date_and_time(values=date_value2)
        ! times(6+1) = times(6+1) + time_diff(date_value1, date_value2)
    end subroutine fit_linear_svm_classifier_ver03


    !> Fit linear_svm_classifier. binary classification only
    !! \param x input data
    !! \param y labels
    subroutine fit_linear_svm_classifier_ver04(this, x, y, times)
        implicit none
        class(linear_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)
        integer(kind=8) :: times(:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:)
        real(kind=8) :: ay, ay2, ai, aj
        real(kind=8) :: ya_i, ya_j
        real(kind=8) :: y_i, y_j, ydf_i
        real(kind=8), allocatable :: x_i(:), x_j(:), x_ij(:)
        real(kind=8), allocatable :: x_sq_sum(:)
        real(kind=8), allocatable :: ab(:), dist_mat(:,:), krnl_mat(:,:), krnl_diag(:)
        logical(kind=8), allocatable :: y_posi(:), y_nega(:)
        logical(kind=8), allocatable :: a_posi(:), a_pena(:)

        integer(kind=8)        :: date_value1(8), date_value2(8), cnt, n

        call date_and_time(values=date_value1)
        if (allocated(this%w_)) deallocate(this%w_)

        call this%transform_y(y, y_)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)
        allocate(x_i(n_columns))
        allocate(x_j(n_columns))
        allocate(x_ij(n_columns))

        allocate(a(n_samples)); a(:) = 0d0
        ay = 0d0; ay2=0d0
        allocate(ayx(n_columns), ayx2(n_columns)); ayx(:) = 0d0; ayx2(:) = 0d0
        allocate(ydf(n_samples))
        allocate(ab(n_samples))
        allocate(krnl_mat(n_samples, n_samples))
        allocate(krnl_diag(n_samples))
        allocate(condition_i(n_samples), condition_j(n_samples))

        allocate(y_posi(n_samples))
        allocate(y_nega(n_samples))
        allocate(a_posi(n_samples))
        allocate(a_pena(n_samples))

        y_posi = y_(:) > 0d0
        y_nega = .not. y_posi
        a_posi = f_
        a_pena = t_
        condition_i = ((y_posi) .and. (a_posi) .or. ((y_nega) .and. (a_pena)))
        condition_j = ((y_nega) .and. (a_posi) .or. ((y_posi) .and. (a_pena)))
        cnt = 0

        call dgemm("N", "T", & 
                    n_samples, n_samples, n_columns, &
                    1d0, & 
                    x, n_samples, &
                    x, n_samples, &
                    0d0, &
                    krnl_mat, n_samples)                    

        call multi_mat_vec(x, ayx, ydf, n_samples, n_columns)
        ydf(:) = y_(:) - ydf(:)

        do i=1, n_samples, 1
            krnl_diag(i) = krnl_mat(i,i)
        end do

        call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)

        do while (t_)
            ! call date_and_time(values=date_value1)
            i = minloc(ydf, mask=condition_i, dim=1)
            ydf_i = ydf(i)
            ab(:) = (ydf - ydf_i)**2d0 / (krnl_mat(i,i) + krnl_diag(:) - 2d0*krnl_mat(:,i))
            j = maxloc(ab, mask=(condition_j .and. ( ydf > ydf_i)), dim=1)
            ! call date_and_time(values=date_value2)
            ! times(2+1) = times(2+1) + time_diff(date_value1, date_value2)

            if (abs(ydf(i) - ydf(j)) <= 1d-3) exit

            ! call date_and_time(values=date_value1)
            y_i = y_(i)
            y_j = y_(j)
            ya_i=y_i*a(i) 
            ya_j=y_j*a(j)
            x_i(:) = x(i,:)
            x_j(:) = x(j,:)
            x_ij(:) = x_i - x_j

            ay2  = ay  - ya_i        - ya_j
            ayx2 = ayx - ya_i*x_i(:) - ya_j*x_j(:)
            ! call date_and_time(values=date_value2)
            ! times(3+1) = times(3+1) + time_diff(date_value1, date_value2)

            ! call date_and_time(values=date_value1)
            ai = (1d0 &
                - y_i*y_j &
                + y_i &
                    * sum( &
                            x_ij(:) * (x_j(:)*ay2 - ayx2) &
                        ) &
                    ) / ( krnl_mat(i,i) + krnl_mat(j,j) - 2d0*krnl_mat(i,j) )

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
            ! call date_and_time(values=date_value2)
            ! times(4+1) = times(4+1) + time_diff(date_value1, date_value2)


            ! call date_and_time(values=date_value1)
            ay  = ay  + y_i * (ai-a(i))        + y_j * (aj-a(j))
            ayx = ayx + y_i * (ai-a(i))*x_i(:) + y_j * (aj-a(j))*x_j(:)

            ydf(:) = ydf(:) - y_i * (ai-a(i)) * krnl_mat(:,i) - y_j * (aj-a(j)) * krnl_mat(:,j)

            if (ai==a(i)) exit

            a(i) = ai
            a(j) = aj

            a_posi(i) =  ai > 0d0
            a_posi(j) =  aj > 0d0
            a_pena(i) =  ai < this%hparam%c
            a_pena(j) =  aj < this%hparam%c

            condition_i(i) = (y_posi(i) .and. a_posi(i)) .or. (y_nega(i) .and. a_pena(i))
            condition_i(j) = (y_posi(j) .and. a_posi(j)) .or. (y_nega(j) .and. a_pena(j))

            condition_j(i) = (y_nega(i) .and. a_posi(i)) .or. (y_posi(i) .and. a_pena(i))
            condition_j(j) = (y_nega(j) .and. a_posi(j)) .or. (y_posi(j) .and. a_pena(j))
            ! call date_and_time(values=date_value2)
            ! times(5+1) = times(5+1) + time_diff(date_value1, date_value2)

            cnt = cnt + 1
            ! if (cnt>1000) exit
        end do
        ! print*, "LoopCount:", cnt
        times(8) = cnt

        ! call date_and_time(values=date_value1)
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
        ! call date_and_time(values=date_value2)
        ! times(6+1) = times(6+1) + time_diff(date_value1, date_value2)
    end subroutine fit_linear_svm_classifier_ver04


    !> Fit linear_svm_classifier. binary classification only
    !! \param x input data
    !! \param y labels
    subroutine fit_linear_svm_classifier_ver05(this, x, y, times)
        implicit none
        class(linear_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)
        integer(kind=8) :: times(:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:)
        real(kind=8) :: ay, ay2, ai, aj
        real(kind=8) :: ya_i, ya_j
        real(kind=8) :: y_i, y_j, ydf_i
        real(kind=8), allocatable :: x_i(:), x_j(:), x_ij(:)
        real(kind=8), allocatable :: x_sq_sum(:)
        real(kind=8), allocatable :: ab(:), dist_mat(:,:), krnl_mat(:,:), krnl_diag(:)
        logical(kind=8), allocatable :: y_posi(:), y_nega(:)
        logical(kind=8), allocatable :: a_posi(:), a_pena(:)
        logical(kind=1), allocatable :: independent(:)

        integer(kind=8)        :: date_value1(8), date_value2(8), cnt, n

        call date_and_time(values=date_value1)
        if (allocated(this%w_)) deallocate(this%w_)

        call this%transform_y(y, y_)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)
        allocate(x_i(n_columns))
        allocate(x_j(n_columns))
        allocate(x_ij(n_columns))

        allocate(a(n_samples)); a(:) = 0d0
        ay = 0d0; ay2=0d0
        allocate(ayx(n_columns), ayx2(n_columns)); ayx(:) = 0d0; ayx2(:) = 0d0
        allocate(ydf(n_samples))
        allocate(ab(n_samples))
        allocate(krnl_mat(n_samples, n_samples))
        allocate(krnl_diag(n_samples))
        allocate(condition_i(n_samples), condition_j(n_samples))

        allocate(y_posi(n_samples))
        allocate(y_nega(n_samples))
        allocate(a_posi(n_samples))
        allocate(a_pena(n_samples))
        allocate(independent(n_samples))

        independent = t_
        y_posi = y_(:) > 0d0
        y_nega = .not. y_posi
        a_posi = f_
        a_pena = t_
        condition_i = ((y_posi) .and. (a_posi) .or. ((y_nega) .and. (a_pena)))
        condition_j = ((y_nega) .and. (a_posi) .or. ((y_posi) .and. (a_pena)))
        cnt = 0

        call dgemm("N", "T", & 
                    n_samples, n_samples, n_columns, &
                    1d0, & 
                    x, n_samples, &
                    x, n_samples, &
                    0d0, &
                    krnl_mat, n_samples)                    

        call multi_mat_vec(x, ayx, ydf, n_samples, n_columns)
        ydf(:) = y_(:) - ydf(:)

        do i=1, n_samples, 1
            krnl_diag(i) = krnl_mat(i,i)
        end do

        call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)

        do while (t_)
            ! call date_and_time(values=date_value1)
            i = minloc(ydf, mask=condition_i .and. independent, dim=1)
            ydf_i = ydf(i)
            ab(:) = (ydf - ydf_i)**2d0 / (krnl_mat(i,i) + krnl_diag(:) - 2d0*krnl_mat(:,i))
            j = maxloc(ab, mask=(condition_j .and. ( ydf > ydf_i)) .and. independent, dim=1)
            ! call date_and_time(values=date_value2)
            ! times(2+1) = times(2+1) + time_diff(date_value1, date_value2)

            if (abs(ydf(i) - ydf(j)) <= 1d-3) exit

            ! call date_and_time(values=date_value1)
            y_i = y_(i)
            y_j = y_(j)
            ya_i=y_i*a(i) 
            ya_j=y_j*a(j)
            x_i(:) = x(i,:)
            x_j(:) = x(j,:)
            x_ij(:) = x_i - x_j

            ay2  = ay  - ya_i        - ya_j
            ayx2 = ayx - ya_i*x_i(:) - ya_j*x_j(:)
            ! call date_and_time(values=date_value2)
            ! times(3+1) = times(3+1) + time_diff(date_value1, date_value2)

            ! call date_and_time(values=date_value1)
            ai = (1d0 &
                - y_i*y_j &
                + y_i &
                    * sum( &
                            x_ij(:) * (x_j(:)*ay2 - ayx2) &
                        ) &
                    ) / ( krnl_mat(i,i) + krnl_mat(j,j) - 2d0*krnl_mat(i,j) )

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
            ! call date_and_time(values=date_value2)
            ! times(4+1) = times(4+1) + time_diff(date_value1, date_value2)


            ! call date_and_time(values=date_value1)
            ay  = ay  + y_i * (ai-a(i))        + y_j * (aj-a(j))
            ayx = ayx + y_i * (ai-a(i))*x_i(:) + y_j * (aj-a(j))*x_j(:)

            ydf(:) = ydf(:) - y_i * (ai-a(i)) * krnl_mat(:,i) - y_j * (aj-a(j)) * krnl_mat(:,j)

            if (ai==a(i)) exit

            a(i) = ai
            a(j) = aj

            a_posi(i) =  ai > 0d0
            a_posi(j) =  aj > 0d0
            a_pena(i) =  ai < this%hparam%c
            a_pena(j) =  aj < this%hparam%c

            condition_i(i) = (y_posi(i) .and. a_posi(i)) .or. (y_nega(i) .and. a_pena(i))
            condition_i(j) = (y_posi(j) .and. a_posi(j)) .or. (y_nega(j) .and. a_pena(j))

            condition_j(i) = (y_nega(i) .and. a_posi(i)) .or. (y_posi(i) .and. a_pena(i))
            condition_j(j) = (y_nega(j) .and. a_posi(j)) .or. (y_posi(j) .and. a_pena(j))
            ! call date_and_time(values=date_value2)
            ! times(5+1) = times(5+1) + time_diff(date_value1, date_value2)

            cnt = cnt + 1
            if (mod(cnt, 1000)==0) then
                i = minloc(ydf, mask=condition_i, dim=1)
                j = maxloc(ydf, mask=condition_j, dim=1)
                independent =  (ydf < ydf(j) .or. ydf(i) < ydf)
                ! print*, cnt, count(condition_i), count(condition_j)
            end if
        end do
        ! print*, "LoopCount:", cnt
        times(8) = cnt

        ! call date_and_time(values=date_value1)
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
        ! call date_and_time(values=date_value2)
        ! times(6+1) = times(6+1) + time_diff(date_value1, date_value2)
    end subroutine fit_linear_svm_classifier_ver05


    !> Fit linear_svm_classifier. binary classification only
    !! \param x input data
    !! \param y labels
    subroutine fit_linear_svm_classifier_ver06(this, x, y, times)
        implicit none
        class(linear_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)
        integer(kind=8) :: times(:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:)
        real(kind=8) :: ay, ay2, ai, aj
        real(kind=8) :: ya_i, ya_j
        real(kind=8) :: y_i, y_j, ydf_i
        real(kind=8), allocatable :: x_i(:), x_j(:), x_ij(:)
        real(kind=8), allocatable :: x_sq_sum(:)
        real(kind=8), allocatable :: ab(:), dist_mat(:,:), krnl_mat(:,:), krnl_diag(:)
        logical(kind=8), allocatable :: y_posi(:), y_nega(:)
        logical(kind=8), allocatable :: a_posi(:), a_pena(:)
        logical(kind=1), allocatable :: independent(:)

        integer(kind=8)        :: date_value1(8), date_value2(8), cnt, n

        call date_and_time(values=date_value1)
        if (allocated(this%w_)) deallocate(this%w_)

        call this%transform_y(y, y_)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)
        allocate(x_i(n_columns))
        allocate(x_j(n_columns))
        allocate(x_ij(n_columns))

        allocate(a(n_samples)); a(:) = 0d0
        ay = 0d0; ay2=0d0
        allocate(ayx(n_columns), ayx2(n_columns)); ayx(:) = 0d0; ayx2(:) = 0d0
        allocate(ydf(n_samples))
        allocate(ab(n_samples))
        allocate(krnl_mat(n_samples, n_samples))
        allocate(krnl_diag(n_samples))
        allocate(condition_i(n_samples), condition_j(n_samples))

        allocate(y_posi(n_samples))
        allocate(y_nega(n_samples))
        allocate(a_posi(n_samples))
        allocate(a_pena(n_samples))
        allocate(independent(n_samples))

        independent = t_
        y_posi = y_(:) > 0d0
        y_nega = .not. y_posi
        a_posi = f_
        a_pena = t_
        condition_i = ((y_posi) .and. (a_posi) .or. ((y_nega) .and. (a_pena)))
        condition_j = ((y_nega) .and. (a_posi) .or. ((y_posi) .and. (a_pena)))
        cnt = 0

        call dgemm("N", "T", & 
                    n_samples, n_samples, n_columns, &
                    1d0, & 
                    x, n_samples, &
                    x, n_samples, &
                    0d0, &
                    krnl_mat, n_samples)                    

        call multi_mat_vec(x, ayx, ydf, n_samples, n_columns)
        ydf(:) = y_(:) - ydf(:)

        do i=1, n_samples, 1
            krnl_diag(i) = krnl_mat(i,i)
        end do

        call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)

        do while (t_)
            ! call date_and_time(values=date_value1)
            i = minloc(ydf, mask=condition_i .and. independent, dim=1)
            ydf_i = ydf(i)
            ab(:) = (ydf - ydf_i)**2d0 / (krnl_mat(i,i) + krnl_diag(:) - 2d0*krnl_mat(:,i))
            j = maxloc(ab, mask=(condition_j .and. ( ydf > ydf_i)) .and. independent, dim=1)
            ! call date_and_time(values=date_value2)
            ! times(2+1) = times(2+1) + time_diff(date_value1, date_value2)

            if (abs(ydf(i) - ydf(j)) <= 1d-3) exit

            ! call date_and_time(values=date_value1)
            y_i = y_(i)
            y_j = y_(j)
            ya_i=y_i*a(i) 
            ya_j=y_j*a(j)
            x_i(:) = x(i,:)
            x_j(:) = x(j,:)
            x_ij(:) = x_i - x_j

            ay2  = ay  - ya_i        - ya_j
            ayx2 = ayx - ya_i*x_i(:) - ya_j*x_j(:)
            ! call date_and_time(values=date_value2)
            ! times(3+1) = times(3+1) + time_diff(date_value1, date_value2)

            ! call date_and_time(values=date_value1)
            ai = (1d0 &
                - y_i*y_j &
                + y_i &
                    * sum( &
                            x_ij(:) * (x_j(:)*ay2 - ayx2) &
                        ) &
                    ) / ( krnl_mat(i,i) + krnl_mat(j,j) - 2d0*krnl_mat(i,j) )

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
            ! call date_and_time(values=date_value2)
            ! times(4+1) = times(4+1) + time_diff(date_value1, date_value2)


            ! call date_and_time(values=date_value1)
            ay  = ay  + y_i * (ai-a(i))        + y_j * (aj-a(j))
            ayx = ayx + y_i * (ai-a(i))*x_i(:) + y_j * (aj-a(j))*x_j(:)

            ydf(:) = ydf(:) - y_i * (ai-a(i)) * krnl_mat(:,i) - y_j * (aj-a(j)) * krnl_mat(:,j)

            if (ai==a(i)) exit

            a(i) = ai
            a(j) = aj

            a_posi(i) =  ai > 0d0
            a_posi(j) =  aj > 0d0
            a_pena(i) =  ai < this%hparam%c
            a_pena(j) =  aj < this%hparam%c

            condition_i(i) = (y_posi(i) .and. a_posi(i)) .or. (y_nega(i) .and. a_pena(i))
            condition_i(j) = (y_posi(j) .and. a_posi(j)) .or. (y_nega(j) .and. a_pena(j))

            condition_j(i) = (y_nega(i) .and. a_posi(i)) .or. (y_posi(i) .and. a_pena(i))
            condition_j(j) = (y_nega(j) .and. a_posi(j)) .or. (y_posi(j) .and. a_pena(j))
            ! call date_and_time(values=date_value2)
            ! times(5+1) = times(5+1) + time_diff(date_value1, date_value2)

            cnt = cnt + 1
            if (mod(cnt, 1000)==0) then
                i = minloc(ydf, mask=condition_i, dim=1)
                j = maxloc(ydf, mask=condition_j, dim=1)
                print*, "hoge "
                independent(:) = (a(:)>=this%hparam%c)
                print*, "hoge 1"
                ! independent = (a==this%hparam%c .and. y_==1) & 
                !     .or. (a==0d0 .and. y_==-1) & 
                !     .or. (a==this%hparam%c .and. y_==-1) & 
                !     .or. (a==0d0 .and. y_==1)
                print*, cnt, count(independent)
            end if
        end do
        ! print*, "LoopCount:", cnt
        times(8) = cnt

        ! call date_and_time(values=date_value1)
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
        ! call date_and_time(values=date_value2)
        ! times(6+1) = times(6+1) + time_diff(date_value1, date_value2)
    end subroutine fit_linear_svm_classifier_ver06






    !> Fit linear_svm_classifier. binary classification only
    !! \param x input data
    !! \param y labels
    subroutine fit_linear_svm_classifier_verXX(this, x, y, times)
        implicit none
        class(linear_svm_classifier) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)
        integer(kind=8) :: times(:)

        integer(kind=8), allocatable :: y_(:)
        integer(kind=8) :: iter, n_samples, n_columns, s, c
        integer(kind=8) :: i, j, count_non_zero
        real(kind=8), allocatable :: a(:), ayx(:), ayx2(:), yx(:,:), ydf(:)
        logical(kind=1), allocatable :: condition_i(:), condition_j(:)
        real(kind=8) :: ay, ay2, ai, aj
        real(kind=8) :: ya_i, ya_j
        real(kind=8) :: y_i, y_j, ydf_i
        real(kind=8), allocatable :: x_i(:), x_j(:), x_ij(:)
        real(kind=8), allocatable :: x_sq_sum(:)
        real(kind=8), allocatable :: ab(:), dist_mat(:,:), krnl_mat(:,:), krnl_diag(:)
        logical(kind=8), allocatable :: y_posi(:), y_nega(:)
        logical(kind=8), allocatable :: a_posi(:), a_pena(:)
        logical(kind=1), allocatable :: independent(:)

        integer(kind=8)        :: date_value1(8), date_value2(8), cnt, n

        integer(kind=8), allocatable :: indices(:), indices_(:)
        logical(kind=1), allocatable :: condition_i_(:), condition_j_(:)
        real(kind=8), allocatable    :: ydf_(:)
        real(kind=8), allocatable    :: krnl_diag_(:)
        real(kind=8), allocatable    :: krnl_mat_(:,:)
        real(kind=8), allocatable    :: ab_(:)
        real(kind=8), allocatable    :: a_(:)
        real(kind=8), allocatable    :: y__(:)
        real(kind=8), allocatable    :: x_(:,:)
        logical(kind=8), allocatable    :: a_posi_(:)
        logical(kind=8), allocatable    :: a_pena_(:)
        logical(kind=8), allocatable    :: y_posi_(:)
        logical(kind=8), allocatable    :: y_nega_(:)
        integer(kind=8) :: i_idx, j_idx, count_useful, idx

        call date_and_time(values=date_value1)
        if (allocated(this%w_)) deallocate(this%w_)

        call this%transform_y(y, y_)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)
        allocate(x_i(n_columns))
        allocate(x_j(n_columns))
        allocate(x_ij(n_columns))

        allocate(a(n_samples)); a(:) = 0d0
        ay = 0d0; ay2=0d0
        allocate(ayx(n_columns), ayx2(n_columns)); ayx(:) = 0d0; ayx2(:) = 0d0
        allocate(ydf(n_samples))
        allocate(ab(n_samples))
        allocate(krnl_mat(n_samples, n_samples))
        allocate(krnl_diag(n_samples))
        allocate(condition_i(n_samples), condition_j(n_samples))

        allocate(y_posi(n_samples))
        allocate(y_nega(n_samples))
        allocate(a_posi(n_samples))
        allocate(a_pena(n_samples))
        allocate(independent(n_samples))

        independent = t_
        y_posi = y_(:) > 0d0
        y_nega = .not. y_posi
        a_posi = f_
        a_pena = t_
        condition_i = ((y_posi) .and. (a_posi) .or. ((y_nega) .and. (a_pena)))
        condition_j = ((y_nega) .and. (a_posi) .or. ((y_posi) .and. (a_pena)))
        cnt = 0

        ! call date_and_time(values=date_value1)
        call dgemm("N", "T", & 
                    n_samples, n_samples, n_columns, &
                    1d0, & 
                    x, n_samples, &
                    x, n_samples, &
                    0d0, &
                    krnl_mat, n_samples)                    
        ! call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)

        call multi_mat_vec(x, ayx, ydf, n_samples, n_columns)
        ydf(:) = y_(:) - ydf(:)

        do i=1, n_samples, 1
            krnl_diag(i) = krnl_mat(i,i)
        end do

        ! call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)

        do while (t_)
            ! call date_and_time(values=date_value1)
            i = minloc(ydf, mask=condition_i .and. independent, dim=1)
            ydf_i = ydf(i)
            ab(:) = (ydf - ydf_i)**2d0 / (krnl_mat(i,i) + krnl_diag(:) - 2d0*krnl_mat(:,i))
            j = maxloc(ab, mask=(condition_j .and. ( ydf > ydf_i)) .and. independent, dim=1)
            ! call date_and_time(values=date_value2)
            ! times(2+1) = times(2+1) + time_diff(date_value1, date_value2)

            if (abs(ydf(i) - ydf(j)) <= 1d-3) exit

            ! call date_and_time(values=date_value1)
            y_i = y_(i)
            y_j = y_(j)
            ya_i=y_i*a(i) 
            ya_j=y_j*a(j)
            x_i(:) = x(i,:)
            x_j(:) = x(j,:)
            x_ij(:) = x_i - x_j

            ay2  = ay  - ya_i        - ya_j
            ayx2 = ayx - ya_i*x_i(:) - ya_j*x_j(:)
            ! call date_and_time(values=date_value2)
            ! times(3+1) = times(3+1) + time_diff(date_value1, date_value2)

            ! call date_and_time(values=date_value1)
            ai = (1d0 &
                - y_i*y_j &
                + y_i &
                    * sum( &
                            x_ij(:) * (x_j(:)*ay2 - ayx2) &
                        ) &
                    ) / ( krnl_mat(i,i) + krnl_mat(j,j) - 2d0*krnl_mat(i,j) )

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
            ! call date_and_time(values=date_value2)
            ! times(4+1) = times(4+1) + time_diff(date_value1, date_value2)


            ! call date_and_time(values=date_value1)
            ay  = ay  + y_i * (ai-a(i))        + y_j * (aj-a(j))
            ayx = ayx + y_i * (ai-a(i))*x_i(:) + y_j * (aj-a(j))*x_j(:)

            ydf(:) = ydf(:) - y_i * (ai-a(i)) * krnl_mat(:,i) - y_j * (aj-a(j)) * krnl_mat(:,j)

            if (ai==a(i)) exit

            a(i) = ai
            a(j) = aj

            a_posi(i) =  ai > 0d0
            a_posi(j) =  aj > 0d0
            a_pena(i) =  ai < this%hparam%c
            a_pena(j) =  aj < this%hparam%c

            condition_i(i) = (y_posi(i) .and. a_posi(i)) .or. (y_nega(i) .and. a_pena(i))
            condition_i(j) = (y_posi(j) .and. a_posi(j)) .or. (y_nega(j) .and. a_pena(j))

            condition_j(i) = (y_nega(i) .and. a_posi(i)) .or. (y_posi(i) .and. a_pena(i))
            condition_j(j) = (y_nega(j) .and. a_posi(j)) .or. (y_posi(j) .and. a_pena(j))
            ! call date_and_time(values=date_value2)
            ! times(5+1) = times(5+1) + time_diff(date_value1, date_value2)

            cnt = cnt + 1
            if (mod(cnt, 1000)==0) then
                i = minloc(ydf, mask=condition_i, dim=1)
                j = maxloc(ydf, mask=condition_j, dim=1)
                independent =  (ydf < ydf(j) .or. ydf(i) < ydf)
                exit
            end if
        end do

        ! Remove Independent Sets

        allocate(indices(n_samples))
        do i=1, n_samples, 1
            indices(i) = i
        end do

        999 continue
        ! call date_and_time(values=date_value1)
        count_useful = count(independent)
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
        ! call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)

        if (allocated(x_)) deallocate(x_)
        if (allocated(krnl_mat_)) deallocate(krnl_mat_)

        allocate(x_(count_useful, n_columns))
        allocate(krnl_mat_(count_useful, count_useful))
        do i=1, count_useful, 1
            i_idx = indices_(i)
            x_(i,:) = x(i_idx,:)
        end do
        ! call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)
        ! call date_and_time(values=date_value1)
        ! call dgemm("N", "T", & 
        !     count_useful, count_useful, n_columns, &
        !     1d0, & 
        !     x_, count_useful, &
        !     x_, count_useful, &
        !     0d0, &
        !     krnl_mat_, count_useful)                    
        ! call date_and_time(values=date_value2)
        ! times(0+1) = times(0+1) + time_diff(date_value1, date_value2)



        ! do while (t_)
        !     ! call date_and_time(values=date_value1)
        !     i = minloc(ydf(indices_), mask=condition_i(indices_), dim=1)
        !     i_idx = indices_(i)
        !     ydf_i = ydf(i_idx)
        !     ab(indices_) = (ydf(indices_) - ydf_i)**2d0 & 
        !                 / (krnl_mat(i_idx,i_idx) + krnl_diag(indices_) - 2d0*krnl_mat(indices_,i_idx))
        !     j = maxloc(ab(indices_), mask=(condition_j(indices_) .and. ( ydf(indices_) > ydf_i)), dim=1)
        !     j_idx = indices_(j)
        !     ! call date_and_time(values=date_value2)
        !     ! times(2+1) = times(2+1) + time_diff(date_value1, date_value2)

        !     if (abs(ydf(i_idx) - ydf(j_idx)) <= 1d-3) exit

        !     ! call date_and_time(values=date_value1)
        !     y_i = y_(i_idx)
        !     y_j = y_(j_idx)
        !     ya_i=y_i*a(i_idx) 
        !     ya_j=y_j*a(j_idx)
        !     x_i(:) = x(i_idx,:)
        !     x_j(:) = x(j_idx,:)
        !     x_ij(:) = x_i - x_j

        !     ay2  = ay  - ya_i        - ya_j
        !     ayx2 = ayx - ya_i*x_i(:) - ya_j*x_j(:)
        !     ! call date_and_time(values=date_value2)
        !     ! times(3+1) = times(3+1) + time_diff(date_value1, date_value2)

        !     ! call date_and_time(values=date_value1)
        !     ai = (1d0 &
        !         - y_i*y_j &
        !         + y_i &
        !             * sum( &
        !                     x_ij(:) * (x_j(:)*ay2 - ayx2) &
        !                 ) &
        !             ) / ( krnl_diag(i_idx) + krnl_diag(j_idx) - 2d0*krnl_mat(i_idx,j_idx) )

        !     if (ai<0d0) then
        !         ai = 0d0
        !     elseif (ai>this%hparam%c) then
        !         ai = this%hparam%c
        !     end if
        !     aj = (-ai*y_i - ay2) * y_j

        !     if (aj<0d0) then
        !         aj = 0d0
        !         ai = (-aj*y_j - ay2) * y_i
        !     elseif (aj>this%hparam%c) then
        !         aj = this%hparam%c
        !         ai = (-aj*y_j - ay2) * y_i
        !     end if
        !     ! call date_and_time(values=date_value2)
        !     ! times(4+1) = times(4+1) + time_diff(date_value1, date_value2)


        !     ! call date_and_time(values=date_value1)
        !     ay  = ay  + y_i * (ai-a(i_idx))        + y_j * (aj-a(j_idx))
        !     ayx = ayx + y_i * (ai-a(i_idx))*x_i(:) + y_j * (aj-a(j_idx))*x_j(:)

        !     ydf(indices_) = ydf(indices_) & 
        !                     - y_i * (ai-a(i_idx)) * krnl_mat(indices_,i_idx) & 
        !                     - y_j * (aj-a(j_idx)) * krnl_mat(indices_,j_idx)

        !     if (ai==a(i_idx)) exit

        !     a(i_idx) = ai
        !     a(j_idx) = aj

        !     a_posi(i_idx) =  ai > 0d0
        !     a_posi(j_idx) =  aj > 0d0
        !     a_pena(i_idx) =  ai < this%hparam%c
        !     a_pena(j_idx) =  aj < this%hparam%c

        !     condition_i(i_idx) = (y_posi(i_idx) .and. a_posi(i_idx)) .or. (y_nega(i_idx) .and. a_pena(i_idx))
        !     condition_i(j_idx) = (y_posi(j_idx) .and. a_posi(j_idx)) .or. (y_nega(j_idx) .and. a_pena(j_idx))

        !     condition_j(i_idx) = (y_nega(i_idx) .and. a_posi(i_idx)) .or. (y_posi(i_idx) .and. a_pena(i_idx))
        !     condition_j(j_idx) = (y_nega(j_idx) .and. a_posi(j_idx)) .or. (y_posi(j_idx) .and. a_pena(j_idx))
        !     ! call date_and_time(values=date_value2)
        !     ! times(5+1) = times(5+1) + time_diff(date_value1, date_value2)

        !     cnt = cnt + 1
        !     if (mod(cnt, 1000)==0) then
        !         i = minloc(ydf, mask=condition_i, dim=1)
        !         j = maxloc(ydf, mask=condition_j, dim=1)
        !         independent =  (ydf < ydf(j) .or. ydf(i) < ydf)
        !         goto 999
        !     end if
        ! end do
        

        do while (t_)
            ! call date_and_time(values=date_value1)
            i = minloc(ydf_, mask=condition_i_, dim=1)
            i_idx = indices_(i)
            ydf_i = ydf_(i)
            ab_(:) = (ydf_ - ydf_i)**2d0 / (krnl_mat(i_idx,i_idx) + krnl_diag_(:) - 2d0*krnl_mat(indices_,i_idx))
            j = maxloc(ab_, mask=(condition_j_ .and. ( ydf_ > ydf_i)), dim=1)
            j_idx = indices_(j)
            ! call date_and_time(values=date_value2)
            ! times(2+1) = times(2+1) + time_diff(date_value1, date_value2)

            if (abs(ydf_(i) - ydf_(j)) <= 1d-3) exit

            ! call date_and_time(values=date_value1)
            y_i = y__(i)
            y_j = y__(j)
            ya_i=y_i*a_(i) 
            ya_j=y_j*a_(j)
            x_i(:) = x_(i,:)
            x_j(:) = x_(j,:)
            x_ij(:) = x_i - x_j

            ay2  = ay  - ya_i        - ya_j
            ayx2 = ayx - ya_i*x_i(:) - ya_j*x_j(:)
            ! call date_and_time(values=date_value2)
            ! times(3+1) = times(3+1) + time_diff(date_value1, date_value2)

            ! call date_and_time(values=date_value1)
            ai = (1d0 &
                - y_i*y_j &
                + y_i &
                    * sum( &
                            x_ij(:) * (x_j(:)*ay2 - ayx2) &
                        ) &
                    ) / ( krnl_mat(i_idx,i_idx) + krnl_mat(j_idx,j_idx) - 2d0*krnl_mat(i_idx,j_idx) )

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
            ! call date_and_time(values=date_value2)
            ! times(4+1) = times(4+1) + time_diff(date_value1, date_value2)


            ! call date_and_time(values=date_value1)
            ay  = ay  + y_i * (ai-a_(i))        + y_j * (aj-a_(j))
            ayx = ayx + y_i * (ai-a_(i))*x_i(:) + y_j * (aj-a_(j))*x_j(:)

            ydf_(:) = ydf_(:) - y_i * (ai-a_(i)) * krnl_mat(indices_,i_idx) - y_j * (aj-a_(j)) * krnl_mat(indices_,j_idx)

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
            ! call date_and_time(values=date_value2)
            ! times(5+1) = times(5+1) + time_diff(date_value1, date_value2)

            cnt = cnt + 1
            if (mod(cnt, 1000)==0) then
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
                independent =  (ydf < ydf(j) .or. ydf(i) < ydf)
                goto 999
            end if
        end do
        ! print*, "LoopCount:", cnt
        times(8) = cnt

        ! call date_and_time(values=date_value1)
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
        ! call date_and_time(values=date_value2)
        ! times(6+1) = times(6+1) + time_diff(date_value1, date_value2)
    end subroutine fit_linear_svm_classifier_verXX


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


    !> Transform input labels for svm format.
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