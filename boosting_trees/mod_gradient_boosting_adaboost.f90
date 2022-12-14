module mod_gradient_boost_adaboost
    use mod_decision_tree
    use mod_hyperparameter
    use mod_data_holder
    use mod_random
    use mod_metric
    use mod_stats
    implicit none

    type adaboost_regressor
        type(hparam_adaboost) :: hparam
        type(decision_tree_regressor), allocatable :: estimators(:)
        integer(kind=8) :: n_samples, n_columns, n_outputs
        integer(kind=8) :: n_estimators_

        real(kind=8), allocatable :: sample_weights(:), sample_probas(:)
        real(kind=8), allocatable :: betas(:), estimator_weights(:)
    contains
        procedure :: fit => fit_adaboost_regressor
        procedure :: predict => predict_adaboost_regressor
        procedure :: initialize
        procedure :: compute_loss
        procedure :: update_sample_weights
        procedure :: dump => dump_adaboost_regressor
        procedure :: load => load_adaboost_regressor
    end type adaboost_regressor

    interface adaboost_regressor
        procedure :: new_adaboost_regressor
    end interface adaboost_regressor    
    
contains

    function new_adaboost_regressor(&
        n_estimators, loss_type)
        implicit none
        type(adaboost_regressor) :: new_adaboost_regressor
        type(adaboost_regressor) :: tmp

        character(len=*), optional :: loss_type
        integer(kind=8), optional  :: n_estimators
        character(len=256)         :: loss_type_list(3)
        integer(kind=8)            :: loss_type_int

        tmp%hparam%algo_name = "adaboost_regressor"

        loss_type_list(1) = "linear"
        loss_type_list(2) = "square"
        loss_type_list(3) = "exp"

        if ( present(n_estimators) ) tmp%hparam%n_estimators = n_estimators
        if ( present(loss_type) )    tmp%hparam%loss_type    = loss_type

        call tmp%hparam%validate_int_range("n_estimators",     tmp%hparam%n_estimators,     1_8,   huge(1_8))
        call tmp%hparam%validate_char_list("loss_type",          tmp%hparam%loss_type,          loss_type_list)

        tmp%hparam%loss_type_int = tmp%hparam%convert_char_to_int(tmp%hparam%loss_type, loss_type_list)

        new_adaboost_regressor = tmp
    end function new_adaboost_regressor


    subroutine initialize(this, dholder)
        implicit none
        class(adaboost_regressor)  :: this
        type(data_holder), pointer :: dholder

        allocate( this%sample_weights(dholder%n_samples), this%sample_probas(dholder%n_samples) )
        this%sample_weights(:) = 1d0
        this%sample_probas(:) = this%sample_weights(:) / sum(this%sample_weights(:))

        allocate( this%betas(0), this%estimator_weights(0) )
    end subroutine initialize


    subroutine compute_loss(this, loss, diff, n_samples)
        implicit none
        class(adaboost_regressor)   :: this
        real(kind=8), intent(inout) :: loss(n_samples)
        real(kind=8), intent(in)    :: diff(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8) :: max_diff

        max_diff = maxval(diff)
        max_diff = 1d0 / max_diff

        if (this%hparam%loss_type_int == 1_8) then
            loss(:) = diff(:) * max_diff
        elseif (this%hparam%loss_type_int == 2_8) then
            max_diff = max_diff**2d0
            loss(:) = (diff(:)**2d0) * max_diff
        elseif (this%hparam%loss_type_int == 3_8) then
            loss(:) = 1d0 - exp(-diff(:) * max_diff)
        end if
    end subroutine compute_loss

    
    subroutine update_sample_weights(this, beta, loss, n_samples)
        implicit none
        class(adaboost_regressor) :: this
        real(kind=8), intent(in)  :: beta
        real(kind=8), intent(in)  :: loss(n_samples)
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8) :: n

        do n=1, n_samples, 1
            this%sample_weights(n) = this%sample_weights(n) * (beta**(1d0-loss(n)))
        end do
        this%sample_probas(:) = this%sample_weights(:) / sum(this%sample_weights(:))
    end subroutine update_sample_weights

    
    subroutine fit_adaboost_regressor(this, dholder, X, y)
        implicit none
        class(adaboost_regressor)  :: this
        type(data_holder), pointer :: dholder
        real(kind=8), intent(inout), target   :: x(:,:), y(:,:)
        
        type(data_holder), pointer :: dholder_2_ptr
        type(data_holder), target :: dholder_2
        integer(kind=8) :: e
        integer(kind=8), allocatable :: indices(:)
        real(kind=8), allocatable    :: pred(:,:), diff(:), losses(:)
        real(kind=8), allocatable, target    :: x_copy(:,:), y_copy(:,:)
        real(kind=8)                 :: max_diff, avg_loss, beta
        type(metrics) :: met

        if (dholder%n_outputs /= 1_8) stop "#outputs must be 1."
        this%n_outputs = dholder%n_outputs

        allocate(indices(dholder%n_samples))
        call this%initialize(dholder)

        allocate(this%estimators(this%hparam%n_estimators))
        allocate(pred(dholder%n_samples, dholder%n_outputs))
        allocate(diff(dholder%n_samples))
        allocate(losses(dholder%n_samples))
        allocate(x_copy(dholder%n_samples,dholder%n_columns), y_copy(dholder%n_samples,dholder%n_outputs))

        do e=1, this%hparam%n_estimators
            this%estimators(e) = decision_tree_regressor(max_depth=4_8)
            call weighted_sampling(indices, dholder%n_samples, this%sample_weights, dholder%n_samples)
            call quick_sort(indices, dholder%n_samples)
            x_copy = dholder%x_ptr%x_r8_ptr(indices,:)
            y_copy = dholder%y_ptr%y_r8_ptr(indices,:)
            dholder_2 = data_holder(x_copy, y_copy, is_trans_x=f_)
            dholder_2_ptr => dholder_2
            call this%estimators(e)%fit(dholder_2_ptr)
            ! call this%estimators(e)%fit(dholder, sample_indices=indices)

            pred = this%estimators(e)%predict(dholder%x_ptr%x_r8_ptr)
            
            diff(:) = abs( dholder%y_ptr%y_r8_ptr(:,1) - pred(:,1) )
            
            call this%compute_loss(losses, diff, dholder%n_samples)
            avg_loss = sum(losses * this%sample_probas)
            beta = avg_loss/(1d0-avg_loss)
            this%betas = [this%betas, beta]
            this%estimator_weights = [this%estimator_weights, log(1d0/beta)]
            
            ! print*, "round=", int(e), "  sum(diff)=", real(sum(diff)), "  sum(losses)=", real(sum(losses)), &
            !     "  sum(pred)=", real(sum(pred)),  &
            !     "  mse=", real(met%mean_square_error(dholder%y_ptr%y_r8_ptr(:,1), pred(:,1))), &
            !     "  avg_loss=", real(avg_loss), &
            !     "  beta=", real(beta)
            call this%update_sample_weights(beta, losses, dholder%n_samples)
        end do
    end subroutine fit_adaboost_regressor


    function predict_adaboost_regressor(this, x)
        implicit none
        class(adaboost_regressor) :: this
        real(kind=8), intent(in)  :: x(:,:)
        real(kind=8), allocatable :: predict_adaboost_regressor(:,:)

        real(kind=8), allocatable :: preds(:, :, :), estimator_weights(:)

        integer(kind=8) :: x_shape(2), n_samples, n_columns, e, n, o

        x_shape = shape(x)
        n_samples = x_shape(1)
        allocate(predict_adaboost_regressor(n_samples, 1))
        allocate(preds(n_samples, 1, this%hparam%n_estimators))
        allocate(estimator_weights(this%hparam%n_estimators))

        do e=1, this%hparam%n_estimators, 1
            preds(:, :, e) = this%estimators(e)%predict(x)
        end do

        do n=1, n_samples, 1
            estimator_weights(:) = this%estimator_weights(:)
            predict_adaboost_regressor(n,1) = weighted_median(preds(n,1,:), estimator_weights, this%hparam%n_estimators)
            ! predict_adaboost_regressor(n,1) = mean(preds(n,1,:), this%hparam%n_estimators)
        end do
    end function predict_adaboost_regressor


    !> A subroutine to dump trained model.
    !! \param file_name output file name.
    subroutine dump_adaboost_regressor(this, file_name)
        implicit none
        class(adaboost_regressor) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit, i
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        write(newunit) this%hparam%n_estimators ! dump fail
        write(newunit) this%n_outputs ! dump fail
        do i=1, size(this%estimators(:)), 1
            call this%estimators(i)%dump_base_tree(newunit)
        end do
        close(newunit)
    end subroutine dump_adaboost_regressor


    !> A subroutine to load trained model.
    !! \param file_name load file name.
    subroutine load_adaboost_regressor(this, file_name)
        implicit none
        class(adaboost_regressor) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit, i, n_outputs
        open(newunit=newunit, file=file_name, form="unformatted")
        read(newunit) this%n_estimators_; allocate(this%estimators(this%n_estimators_))
        read(newunit) this%n_outputs
        do i=1, size(this%estimators(:)), 1
            call this%estimators(i)%load_base_tree(newunit)
        end do
        close(newunit)
    end subroutine load_adaboost_regressor


end module mod_gradient_boost_adaboost