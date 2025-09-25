module mod_kmeans_fit_single_iter_dense

    use :: mod_kinds
    use :: mod_program_limits
    use :: mod_euclidean_distance
    use :: mod_random_seed
    use :: mod_warn_clusters_non_empty, only: warn_clusters_non_empty
    use :: mod_timer

    implicit none
    
contains


    function fit_single_iter_whole_dense(&
        n_rows, n_cols, n_clusters, &
        print_log, tol, class_name, itr, &
        X, X_sqsum, &
        mat_dist_whole, vec_min_dist, &
        centroids_new, centroids_old, C_row_sq_norm, &
        labels_new, labels_old, labels_diff, labels_counter_new, n_iter_) result(reach_tol)
        implicit none
        ! Shapes
        integer(i64), intent(in)  :: n_rows, n_cols                     ! shape(X)
        integer(i64), intent(in)  :: n_clusters                         ! size(centroids, dim=1)

        ! Parameter
        logical, intent(in)       :: print_log
        real(r64), intent(in)     :: tol
        character(*), intent(in)  :: class_name
        integer(i64), intent(in)  :: itr             ! iteration id

        ! Arrays
        real(r64), intent(in)     :: X(n_rows, n_cols)                      ! input matrix
        real(r64), intent(in)     :: X_sqsum                                ! sum of squared norm
        real(r64), intent(out)    :: mat_dist_whole(n_rows, n_clusters)     ! full distance matrix
        real(r64), intent(out)    :: vec_min_dist(n_rows)                   ! full distance matrix
        real(r64), intent(out)    :: centroids_new(n_clusters, n_cols)      ! new centroids
        real(r64), intent(out)    :: centroids_old(n_clusters, n_cols)      ! old centroids
        real(r64), intent(out)    :: C_row_sq_norm(n_clusters)              ! squared norm per row
        integer(i64), intent(out) :: labels_new(n_rows), labels_old(n_rows) ! new and old labels
        logical, intent(out)      :: labels_diff(n_rows)                    ! label diff, between new and old
        integer(i64), intent(out) :: labels_counter_new(n_clusters)         ! count by cluster
        integer(i64), intent(out) :: n_iter_         ! number of iteration

        ! Result
        logical :: reach_tol

        ! ------------------------------------------
        integer(i64) :: i, c                 ! loop counters, n_rows and n_clusters
        integer(i64) :: label                ! cluster label
        integer(i64) :: label_old, label_new ! cluster label
        integer(i64) :: count_diff           ! #diff centroid update
        real(r64)    :: best_score, score    ! Sum of Squared Error
        logical      :: relocation_happened
        integer(i64) :: i_star
        integer(i64) :: date_value1(8), date_value2(8)
        integer(i64) :: time_dist, time_upd8

        time_dist = 0
        time_upd8 = 0
        reach_tol = .false.


        ! call date_and_time(values=date_value1)
        call euclidean_distance_mm_bnorm(    &
            n_rows, n_cols, n_clusters, &
            mat_dist_whole,                        &
            X,                               &
            centroids_new, C_row_sq_norm)
        ! Cluster label
        labels_new(:) = minloc(mat_dist_whole, dim=2)
        ! call date_and_time(values=date_value2)
        ! time_dist = time_dist + time_diff(date_value1, date_value2)

        if (print_log) print*, itr, sum(minval(mat_dist_whole, dim=2)) + X_sqsum
        labels_diff(:) = labels_new /= labels_old
        count_diff = count(labels_diff)

        ! Not change
        if (count_diff == 0_i64) then
            reach_tol = .true.
            return
        end if

        ! Update Centroids
        relocation_happened = .false.
        if ((itr == 1_i64) .or. (count_diff >= n_rows / 4_i64)) then
            labels_counter_new(:) = 0_i64
            centroids_new(:,:) = 0.0_r64

            ! 足し上げ
            do i=1, n_rows, 1
                label = labels_new(i)
                labels_counter_new(label) = labels_counter_new(label) + 1
                centroids_new(label,:) = centroids_new(label,:) + X(i,:)
            end do

        else
            ! 復元
            centroids_old(:,:) = centroids_new(:,:)
            do c=1, n_clusters, 1
                centroids_new(c,:) = centroids_old(c,:) * labels_counter_new(c)
            end do

            ! 差分のみアップデート
            do i=1, n_rows, 1
                if (labels_diff(i)) then
                    ! 古いほうから取り出す
                    label_old = labels_old(i)
                    centroids_new(label_old,:) = centroids_new(label_old,:) - X(i,:)
                    labels_counter_new(label_old)  = labels_counter_new(label_old)  - 1_i64

                    ! 新しいほうに入れる
                    label_new = labels_new(i)
                    centroids_new(label_new,:) = centroids_new(label_new,:) + X(i,:)
                    labels_counter_new(label_new)  = labels_counter_new(label_new)  + 1_i64                        
                end if
            end do
        end if
        ! call date_and_time(values=date_value2)
        ! time_upd8 = time_upd8 + time_diff(date_value1, date_value2)

        ! Warn Empty Cluster
        relocation_happened = any(labels_counter_new == 0_i64)
        call warn_clusters_non_empty(labels_counter_new, &
                file=__FILE__,                      &
                class_name=class_name,              &
                value_name="")

        ! Empty cluster column: mark as unused until next full recomputation
        if (relocation_happened) then
            ! Mask Distance Matrix
            do c=1, n_clusters, 1
                if (labels_counter_new(c) == 0_i64) then
                    mat_dist_whole(:,c) = HUGE_R64
                end if
            end do

            ! Relocate Empty Cluster
            vec_min_dist(:) = minval(mat_dist_whole, dim=2)
            do c=1, n_clusters, 1
                if (labels_counter_new(c) == 0_i64) then
                    i_star = maxloc(vec_min_dist, dim=1)
                    centroids_new(c,:) = X(i_star,:)
                    vec_min_dist(i_star) = 0.0_r64
                end if
            end do
        end if

        ! Compute Center
        do c=1, n_clusters, 1
            if (labels_counter_new(c) == 0_8) cycle
            centroids_new(c,:) = centroids_new(c,:) / dble(labels_counter_new(c))
        end do

        C_row_sq_norm(:) = sum(centroids_new**2.0_r64, dim=2)
        n_iter_ = itr
        if (sum((centroids_new - centroids_old)**2.0_r64) <= tol)  then
            reach_tol = .true.
            return
        end if
        labels_old(:)      = labels_new(:)    
        centroids_old(:,:) = centroids_new(:,:)
        ! print*, "-----------------------------------------------------------------------------"
        ! print*, time_dist, time_upd8
        ! print*, "-----------------------------------------------------------------------------"
    end function fit_single_iter_whole_dense


    function fit_single_iter_chunk_dense(&
        n_rows, n_cols, n_clusters, chunk_size, &
        print_log, tol, class_name, itr, &
        X, X_sqsum, &
        mat_dist_chunk, vec_min_dist, &
        centroids_new, centroids_old, C_row_sq_norm, &
        labels_new, labels_old, labels_diff, labels_counter_new, n_iter_) result(reach_tol)
        implicit none
        ! Shapes
        integer(i64), intent(in)  :: n_rows, n_cols                     ! shape(X)
        integer(i64), intent(in)  :: n_clusters                         ! size(centroids, dim=1)
        integer(i64), intent(in)  :: chunk_size                         ! size(centroids, dim=1)

        ! Parameter
        logical, intent(in)       :: print_log
        real(r64), intent(in)     :: tol
        character(*), intent(in)  :: class_name
        integer(i64), intent(in)  :: itr             ! iteration id

        ! Arrays
        real(r64), intent(in)     :: X(n_rows, n_cols)                      ! input matrix
        real(r64), intent(in)     :: X_sqsum                                ! sum of squared norm
        real(r64), intent(out)    :: mat_dist_chunk(chunk_size, n_clusters)     ! full distance matrix
        real(r64), intent(out)    :: vec_min_dist(n_rows)                   ! full distance matrix
        real(r64), intent(out)    :: centroids_new(n_clusters, n_cols)      ! new centroids
        real(r64), intent(out)    :: centroids_old(n_clusters, n_cols)      ! old centroids
        real(r64), intent(out)    :: C_row_sq_norm(n_clusters)              ! squared norm per row
        integer(i64), intent(out) :: labels_new(n_rows), labels_old(n_rows) ! new and old labels
        logical, intent(out)      :: labels_diff(n_rows)                    ! label diff, between new and old
        integer(i64), intent(out) :: labels_counter_new(n_clusters)         ! count by cluster
        integer(i64), intent(out) :: n_iter_         ! number of iteration

        ! Result
        logical :: reach_tol

        ! ------------------------------------------
        integer(i64) :: i, c                 ! loop counters, n_rows, n_clusters
        integer(i64) :: label                ! cluster label
        integer(i64) :: label_old, label_new ! cluster label
        integer(i64) :: count_diff           ! #diff centroid update
        real(r64)    :: best_score, score    ! Sum of Squared Error
        logical      :: relocation_happened
        integer(i64) :: i_star
        integer(i64) :: i_start, i_end, cs
        real(r64)    :: sse_sum

        reach_tol = .false.

        sse_sum = 0.0_r64
        do i_start = 1, n_rows, chunk_size
            i_end = min(i_start + chunk_size - 1, n_rows)
            cs    = i_end - i_start + 1

            ! ---- ② 距離計算 ----------------------------------------------------
            call euclidean_distance_mm_bnorm_chunk(  &
                    n_rows, n_cols, n_clusters,          & 
                    i_start, i_end, cs,              &
                    mat_dist_chunk,                  & 
                    X,                               & 
                    centroids_new, C_row_sq_norm)      
            ! ---- ③ ラベル更新 --------------------------------------------------
            ! minloc(dim=2) は形状 (cs) の整数配列を返す
            sse_sum = sse_sum + sum(minval(mat_dist_chunk, dim=2))
            labels_new(i_start:i_end) = int(minloc(mat_dist_chunk(:, :), dim=2), kind=i64)
        end do

        if (print_log) print*, itr, sse_sum + X_sqsum
        labels_diff(:) = labels_new /= labels_old
        count_diff = count(labels_diff)

        ! Not change
        if (count_diff == 0_i64) return

        ! Store Old centroids
        centroids_old(:,:) = centroids_new(:,:)

        ! Update Centroids
        if ( (itr == 1_i64) .or. (relocation_happened)) then
            labels_counter_new(:) = 0_i64
            centroids_new(:,:) = 0.0_r64

            ! 足し上げ    
            do i=1, n_rows, 1
                label = labels_new(i)
                labels_counter_new(label) = labels_counter_new(label) + 1
                centroids_new(label,:) = centroids_new(label,:) + X(i,:)
            end do

        else
            ! 復元    
            do c=1, n_clusters, 1
                centroids_new(c,:) = centroids_new(c,:) * labels_counter_new(c)
            end do

            ! 差分のみアップデート    
            do i=1, n_rows, 1
                if (labels_diff(i)) then
                    ! 古いほうから取り出す            
                    label_old = labels_old(i)
                    centroids_new(label_old,:) = centroids_new(label_old,:) - X(i,:)
                    labels_counter_new(label_old)  = labels_counter_new(label_old)  - 1_i64

                    ! 新しいほうに入れる            
                    label_new = labels_new(i)
                    centroids_new(label_new,:) = centroids_new(label_new,:) + X(i,:)
                    labels_counter_new(label_new)  = labels_counter_new(label_new)  + 1_i64                        
                end if
            end do

        end if

        ! Warn Empty Cluster
        relocation_happened = any(labels_counter_new == 0_i64)
        call warn_clusters_non_empty(labels_counter_new, &
                file=__FILE__,                      &
                class_name=class_name,              &
                value_name="")

        ! Empty cluster column: mark as unused until next full recomputation
        if (relocation_happened) then
            ! Mask Distance Matrix

            do i_start = 1, n_rows, chunk_size
                i_end = min(i_start + chunk_size - 1, n_rows)
                cs    = i_end - i_start + 1

                call euclidean_distance_mm_bnorm_chunk(  &
                        n_rows, n_cols, n_clusters,          & 
                        i_start, i_end, cs,              &
                        mat_dist_chunk,                  & 
                        X,                               & 
                        centroids_new, C_row_sq_norm)      

                do c=1, n_clusters, 1
                    if (labels_counter_new(c) == 0_i64) then
                        mat_dist_chunk(:,c) = HUGE_R64
                    end if
                end do
                vec_min_dist(i_start:i_end) = minval(mat_dist_chunk, dim=2)
            end do

            ! Relocate Empty Cluster    
            do c=1, n_clusters, 1
                if (labels_counter_new(c) == 0_i64) then
                    i_star = maxloc(vec_min_dist, dim=1)
                    centroids_new(c,:) = X(i_star,:)
                    vec_min_dist(i_star) = 0.0_r64
                end if
            end do
        end if

        ! Compute Center
        do c=1, n_clusters, 1
            if (labels_counter_new(c) == 0_8) cycle
            centroids_new(c,:) = centroids_new(c,:) / dble(labels_counter_new(c))
        end do

        C_row_sq_norm(:) = sum(centroids_new**2.0_r64, dim=2)
        n_iter_ = itr
        if (sum((centroids_new - centroids_old)**2.0_r64) <= tol) then
            reach_tol = .true.
            return
        end if
        labels_old(:)         = labels_new(:)    
    end function fit_single_iter_chunk_dense


end module mod_kmeans_fit_single_iter_dense
