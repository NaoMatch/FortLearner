module mod_product_quantization
    use mod_timer
    use mod_const
    use mod_common
    use mod_common_type
    use mod_kmeans
    use mod_hyperparameter
    use mod_threshold_tree
    use mod_stats
    use mod_data_holder    
    use mod_nearest_neighbour, only: neighbor_indices
    implicit none

    type sample_enc
        integer(kind=8) :: idx
        integer(kind=8), allocatable :: codes(:)
    end type sample_enc

    type jagged_matrix_i8
        integer(kind=8), allocatable :: idx(:)
        integer(kind=8), allocatable :: mat(:,:)
    end type jagged_matrix_i8

    type product_quantization
        integer(kind=8) :: n_subspace=5_8
        integer(kind=8) :: n_clusters=256_8
        logical(kind=4) :: use_threshold_tree = f_

        integer(kind=8) :: n_samples, n_columns

        integer(kind=8), allocatable :: dim_ranges(:,:)
        type(kmeans) :: km_cq ! coarse quantizer
        type(kmeans), allocatable :: kms_pq(:) ! product quantizer
        type(threshold_tree), allocatable :: tts(:)

        integer(kind=8), allocatable :: x_cq_enc(:,:) ! encoded data by coarse quantizer 
        integer(kind=8), allocatable :: x_pq_enc(:,:) ! encoded data by product quantizer

        type(jagged_vector_r8), allocatable :: cq_cls_sq_sums(:)
        type(jagged_vector_r8), allocatable :: pq_cls_sq_sums(:)

        integer(kind=8), allocatable :: counter(:)
        type(jagged_matrix_i8), allocatable :: mat(:)

        type(sample_enc), allocatable :: code_book(:)
    contains
        procedure, pass :: fit_product_quantization_x
        procedure, pass :: fit_product_quantization_dholder
        generic :: fit => fit_product_quantization_x, fit_product_quantization_dholder
        procedure :: encode => encode_product_quantization
        procedure :: query  => query_product_quantization
    end type product_quantization

    interface product_quantization
        module procedure :: new_product_quantization
    end interface product_quantization

contains

    function new_product_quantization(n_subspace, n_clusters)
        implicit none
        type(product_quantization) :: new_product_quantization
        type(product_quantization) :: tmp

        integer(kind=8), optional :: n_subspace
        integer(kind=8), optional :: n_clusters

        if(present(n_subspace)) tmp%n_subspace = n_subspace
        if(present(n_clusters)) tmp%n_clusters = n_clusters

        new_product_quantization = tmp
    end function new_product_quantization


    subroutine fit_product_quantization_x(this, x)
        implicit none
        class(product_quantization) :: this
        real(kind=8), intent(in)  :: x(:,:)
        integer(kind=8) :: c, s, n, dim_step, ini, fin, idx
        integer(kind=8) :: n_samples, n_columns, x_shape(2)

        integer(kind=8), allocatable :: pred_idxs(:)
        real(kind=8), allocatable :: x_center(:,:)
        real(kind=8), allocatable :: x_residual(:,:)

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)
        allocate(pred_idxs(n_samples))

        ! fit coarse quantizer
        ! print*, "fit coarse quantizer"
        this%km_cq = kmeans(n_clusters=this%n_clusters)
        call this%km_cq%fit_dgemm(x)
        pred_idxs = this%km_cq%predict(x)

        ! compute residual
        ! print*, "compute residual"
        allocate(x_residual(n_samples, n_columns))
        x_residual(:,:) = x(:,:) - transpose(this%km_cq%cluster_centers(:,pred_idxs))

        ! fit product quantizer
        ! print*, "fit product quantizer"
        dim_step = n_columns / this%n_subspace
        allocate(this%dim_ranges(this%n_subspace, 2))
        allocate(this%kms_pq(this%n_subspace))
        allocate(this%pq_cls_sq_sums(this%n_subspace))
        ini = 1
        do s=1, this%n_subspace, 1
            ! Set Feature Range
            ! print*, "Set Feature Range"
            fin = minval([ini + dim_step-1, n_columns])
            this%dim_ranges(s,:) = [ini, fin]

            ! Fit Product Quantizer
            ! print*, "Fit Product Quantizer"
            this%kms_pq(s) = kmeans(n_clusters=this%n_clusters)
            call this%kms_pq(s)%fit(x_residual(:, ini:fin))

            ! Shift
            ! print*, "Shift"
            ini = fin + 1
        end do
    end subroutine fit_product_quantization_x

    
    subroutine fit_product_quantization_dholder(this, dholder)
        implicit none
        class(product_quantization) :: this
        type(data_holder), intent(in) :: dholder
        call this%fit_product_quantization_x(dholder%x_ptr%x_r8_ptr)
    end subroutine fit_product_quantization_dholder


    subroutine encode_product_quantization(this, x, add_new_data)
        implicit none
        class(product_quantization) :: this
        real(kind=8), intent(in)    :: x(:,:)
        logical(kind=4), optional, intent(in) :: add_new_data

        integer(kind=8) :: c, s, ini, fin, n, idx
        integer(kind=8) :: n_samples, n_columns, x_shape(2)

        integer(kind=8), allocatable :: indices_coarse(:), indices(:)
        integer(kind=8), allocatable :: index_products(:,:), indices_sample(:), index_cq(:), counter_cq(:)
        real(kind=8), allocatable :: x_residual(:,:)
        integer(kind=8)        :: date_value1(8), date_value2(8)
    
        if (present(add_new_data)) stop "Not Implemented Error, 'add_new_data'."

        x_shape = shape(x)
        this%n_samples = x_shape(1)
        this%n_columns = x_shape(2)

        ! Predict Coarse Quantizer Indices
        ! print*, "Predict Coarse Quantizer Indices"
        allocate(indices_coarse(this%n_samples))
        indices_coarse(:) = this%km_cq%predict(x)
        
        ! Compute Residual
        ! print*, "Compute Residual"
        allocate(x_residual(this%n_samples, this%n_columns))
        do n=1, this%n_samples, 1
            x_residual(n,:) = x(n,:) - this%km_cq%cluster_centers_t(indices_coarse(n),:)
        end do

        ! Predict Product Quantizer Indices
        ! print*, "Predict Product Quantizer Indices"
        allocate(index_products(this%n_samples, this%n_subspace))
        do s=1, this%n_subspace, 1
            ini = this%dim_ranges(s,1)
            fin = this%dim_ranges(s,2)
            index_products(:,s) = this%kms_pq(s)%predict(x_residual(:,ini:fin))
        end do

        ! Sort by indices of coarse quantizer cluster center
        ! print*, "Sort by indices of coarse quantizer cluster center"
        allocate(indices_sample(this%n_samples))
        do n=1, this%n_samples, 1
            indices_sample(n) = n
        end do
        call quick_argsort(indices_coarse, indices_sample, this%n_samples)
        call groupby_count(index_cq, counter_cq, indices_coarse, this%n_samples)
        indices_coarse(:) = indices_coarse(indices_sample)
        x_residual(:,:) = x_residual(indices_sample,:)
        index_products(:,:) = index_products(indices_sample,:)

        ! Stored per coarse quantizer cluster center
        ! print*, "Stored per coarse quantizer cluster center"
        allocate(this%mat(this%n_clusters), this%counter(this%n_clusters))
        ini = 1
        do c=1, this%n_clusters, 1
            fin = ini + counter_cq(c)-1
            allocate(this%mat(c)%mat(counter_cq(c), this%n_subspace))
            allocate(this%mat(c)%idx(counter_cq(c)))
            this%mat(c)%mat(:,:) = index_products(ini:fin,:)
            this%mat(c)%idx(:) = indices_sample(ini:fin)
            this%counter(c) = counter_cq(c)
            ini = fin + 1
        end do

        ! Compute PQ Cluster Square Sum
        do s=1, this%n_subspace, 1
            ini = this%dim_ranges(s,1)
            fin = this%dim_ranges(s,2)
            allocate(this%pq_cls_sq_sums(s)%vector(this%n_clusters))
            do c=1, this%n_clusters, 1
                this%pq_cls_sq_sums(s)%vector(c) = sum(this%kms_pq(s)%cluster_centers_t(c,:)**2d0)
            end do
        end do
        deallocate(x_residual)
    end subroutine encode_product_quantization


    function query_product_quantization(this, query, n_neighbors) result(res)
        implicit none
        class(product_quantization) :: this
        real(kind=8), intent(in)    :: query(:,:)
        integer(kind=8), intent(in) :: n_neighbors
        type(neighbor_indices) :: res
        integer(kind=8)        :: date_value1(8), date_value2(8)
        integer(kind=8), save        :: time_prep, time_dtable, time_get

        real(kind=8), allocatable    :: dist_qtoc(:,:) ! distance table from query to codes
        integer(kind=8), allocatable :: c_indices(:)
        integer(kind=8) :: c, s, i, f, n, label, n_queries, q, cluster_idx, min_loc, min_count, n_probes_spaces
        integer(kind=8) :: n_samples_in_cluster, sc, n_samples_in_cluster_sc
        real(kind=8), allocatable :: q_residual(:)
        real(kind=8), allocatable :: q_sq_sums(:), distance_table(:,:), distances(:), distances_sc(:), dst_q2c(:)
        real(kind=8) :: tmp_dist

        integer(kind=8), allocatable :: idx_cq(:), indices(:), indices_out(:), indices_out_sc(:), search_clusters(:)
        integer(kind=8), allocatable :: indices_cls_hold(:), indices_cls(:)

        ! if (loop_idx==1_8) then
        !     time_prep = 0
        !     time_dtable = 0
        !     time_get = 0
        ! end if

        n_queries = size(query, dim=1)
        idx_cq = this%km_cq%predict(query, hold_distance_table=t_)

        allocate(distance_table(this%n_subspace, this%n_clusters))
        allocate(q_sq_sums(this%n_subspace))
        allocate(q_residual(this%n_columns))
        allocate(indices_cls_hold(this%n_clusters), indices_cls(this%n_clusters), dst_q2c(this%n_clusters))
        do c=1, this%n_clusters, 1
            indices_cls_hold(c) = c
        end do
        if (allocated(res%indices)) deallocate(res%indices)
        allocate(res%indices(n_queries))
        do q=1, n_queries, 1
            ! print*, '*********************************************************************************************'
            ! print*, '*********************************************************************************************'
            ! print*, '*********************************************************************************************'
            ! Set Cluster Indices
            cluster_idx = idx_cq(q)
            n_samples_in_cluster = this%counter(cluster_idx)

            ! Check Number of samples in 'cluster_idx'
            ! If 'n_samples_in_cluster' < 'n_neighbors', neighboring cluster(s) are also searched.
            allocate(search_clusters(0))
            search_clusters = [search_clusters, cluster_idx]
            if (n_samples_in_cluster < n_neighbors) then
                indices_cls(:) = indices_cls_hold(:)
                dst_q2c(:) = this%km_cq%distance_table(q,:)
                call quick_argsort(dst_q2c, indices_cls, this%n_clusters)
                do c=2, this%n_clusters, 1
                    search_clusters = [search_clusters, indices_cls(c)]
                    n_samples_in_cluster = n_samples_in_cluster + this%counter(indices_cls(c))
                    if (n_samples_in_cluster >= n_neighbors) exit
                end do
            end if
            n_probes_spaces = size(search_clusters)

            allocate(distances(0), indices_out(0))
            do sc=1, n_probes_spaces, 1
                ! Set Cluster Index
                cluster_idx = search_clusters(sc)
                n_samples_in_cluster_sc = this%counter(cluster_idx)

                ! Reset Distance Table
                distance_table(:,:) = 0d0

                ! Compute Residual
                q_residual(:) = query(q, :) - this%km_cq%cluster_centers_t(cluster_idx,:)

                ! Preprocess
                do s=1, this%n_subspace, 1
                    i = this%dim_ranges(s,1)
                    f = this%dim_ranges(s,2)
                    q_sq_sums(s) = sum(q_residual(i:f)**2d0)
                end do

                ! Compute Distance Table
                do c=1, this%n_clusters, 1
                    do s=1, this%n_subspace, 1
                        i = this%dim_ranges(s,1)
                        f = this%dim_ranges(s,2)
                        distance_table(s,c) &
                            = q_sq_sums(s) + this%pq_cls_sq_sums(s)%vector(c) & 
                            - 2d0 * sum(q_residual(i:f)*this%kms_pq(s)%cluster_centers(:,c))
                    end do
                end do

                ! Compute Distance from query to samples
                allocate(distances_sc(n_samples_in_cluster_sc), indices_out_sc(n_samples_in_cluster_sc))
                do n=1, n_samples_in_cluster_sc, 1
                    tmp_dist = 0d0
                    do s=1, this%n_subspace, 1
                        label = this%mat(cluster_idx)%mat(n,s)
                        tmp_dist = tmp_dist + distance_table(s,label)
                    end do
                    distances_sc(n) = tmp_dist
                end do
                indices_out_sc(:) = this%mat(cluster_idx)%idx(:)

                distances   = [distances,   distances_sc]
                indices_out = [indices_out, indices_out_sc]
                deallocate(distances_sc, indices_out_sc)
            end do
            ! call quick_argselect(distances, indices_out, n_samples_in_cluster, n_neighbors)
            call quick_argsort(distances, indices_out, n_samples_in_cluster)
            allocate(res%indices(q)%idx(n_neighbors))
            res%indices(q)%idx = [indices_out(1:n_neighbors)]
            deallocate(distances, indices_out, search_clusters)

            ! print*, '*********************************************************************************************'
            ! print*, '*********************************************************************************************'
            ! print*, '*********************************************************************************************'
            ! call date_and_time(values=date_value1)
            ! cluster_idx = idx_cq(q)
            ! n_samples_in_cluster = this%counter(cluster_idx)

            ! ! Reset Distance Table
            ! distance_table(:,:) = 0d0

            ! ! Compute Residual
            ! q_residual(:) = query(q, :) - this%km_cq%cluster_centers_t(cluster_idx,:)

            ! ! Preprocess
            ! do s=1, this%n_subspace, 1
            !     i = this%dim_ranges(s,1)
            !     f = this%dim_ranges(s,2)
            !     q_sq_sums(s) = sum(q_residual(i:f)**2d0)
            ! end do
            ! call date_and_time(values=date_value2)
            ! time_prep = time_prep + time_diff(date_value1, date_value2)
            
            ! ! Compute Distance Table
            ! call date_and_time(values=date_value1)
            ! ! do c=1, this%n_clusters, 1
            ! !     do s=1, this%n_subspace, 1
            ! !         distance_table(s,c) = q_sq_sums(s) + this%pq_cls_sq_sums(s)%vector(c)
            ! !     end do
            ! ! end do
            ! ! do c=1, this%n_clusters, 1
            ! !     do s=1, this%n_subspace, 1
            ! !         i = this%dim_ranges(s,1)
            ! !         f = this%dim_ranges(s,2)
            ! !         distance_table(s,c) &
            ! !             = distance_table(s,c) & 
            ! !             - 2d0 * sum(q_residual(i:f)*this%kms_pq(s)%cluster_centers(:,c))
            ! !     end do
            ! ! end do
            ! do c=1, this%n_clusters, 1
            !     do s=1, this%n_subspace, 1
            !         i = this%dim_ranges(s,1)
            !         f = this%dim_ranges(s,2)
            !         distance_table(s,c) &
            !             = q_sq_sums(s) + this%pq_cls_sq_sums(s)%vector(c) & 
            !             - 2d0 * sum(q_residual(i:f)*this%kms_pq(s)%cluster_centers(:,c))
            !     end do
            ! end do
            ! call date_and_time(values=date_value2)
            ! time_dtable = time_dtable + time_diff(date_value1, date_value2)
            
            ! ! Compute Distance from query to samples
            ! call date_and_time(values=date_value1)
            ! allocate(distances(n_samples_in_cluster), indices_out(n_samples_in_cluster))
            ! min_count = minval([n_samples_in_cluster, n_neighbors])
            ! do n=1, n_samples_in_cluster, 1
            !     tmp_dist = 0d0
            !     do s=1, this%n_subspace, 1
            !         label = this%mat(cluster_idx)%mat(n,s)
            !         tmp_dist = tmp_dist + distance_table(s,label)
            !     end do
            !     distances(n) = tmp_dist
            ! end do
            ! indices_out(:) = this%mat(cluster_idx)%idx(:)
            ! call quick_argselect(distances, indices_out, n_samples_in_cluster, min_count)
            ! res(q)%indices = [indices_out(1:min_count)]
            ! deallocate(distances, indices_out)
            ! call date_and_time(values=date_value2)
            ! time_get = time_get + time_diff(date_value1, date_value2)
        end do

        ! if (loop_idx==100_8) then
        !     print*, "time_prep:   ", time_prep
        !     print*, "time_dtable: ", time_dtable
        !     print*, "time_get:    ", time_get
        ! end if
    end function query_product_quantization


end module mod_product_quantization