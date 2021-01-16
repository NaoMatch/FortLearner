if (n_samples .eq. 0) return ! Fast Return

vector_min = minval(vector)
vector_max = maxval(vector)
if (vector_min .eq. vector_max) return ! Fast Return

rec_depth_opt = 0
if (present(rec_depth)) rec_depth_opt = rec_depth

! -----------------------------------------------------------------------
n_sampling = sqrt(real(n_samples, kind=kind(n_sampling)))
allocate(sampling_indices(n_sampling))
allocate(values(n_sampling))
call rand_integer(one, n_samples, sampling_indices, n_sampling)
call quick_sort(sampling_indices, n_sampling)
do i=1, n_sampling, 1
    idx = sampling_indices(i)
    values(i) = vector(idx)
end do
call quick_sort(values, n_sampling)
n_unique = count_unique(values, n_sampling)
if (n_unique .le. 10) then
    call quick_sort(vector, n_samples)
    return
elseif (n_unique .gt. int(n_samples * 0.98, kind=kind(n_unique))) then
    left_posi = most_left_bit_position(n_unique)
    n_buckets = ibset(zero, left_posi+one)
else
    n_buckets = n_unique
end if

! -----------------------------------------------------------------------
allocate(works(n_buckets))
allocate(counters(n_buckets))
allocate(sizes(n_buckets))
min_bucket_size = maxval((/ thousand, n_samples/n_buckets /))
counters = 1
sizes = min_bucket_size
do b=1, n_buckets, 1
    allocate( works(b)%x(min_bucket_size) )
    allocate( works(b)%y(min_bucket_size) )
end do
diff = 1.0 / (vector_max-vector_min)
max_val = n_buckets-1.0
factor = diff * max_val
diff = - vector_min * factor + 1
n_samples_unroll = n_samples - mod(n_samples, 63)

do i=1, n_samples_unroll, 63
    do k=0, 63-1, 1
        tmp_v = vector(i+k)
        tmp_i = indices(i+k)
        buffer_v(k+1) = tmp_v
        buffer_b(k+1) = int( tmp_v * factor + diff, kind=kind(bucket_idx))
        buffer_i(k+1) = tmp_i
    end do

    do k=0, 63-1, 1
        tmp_v = buffer_v(k+1)
        tmp_i = buffer_i(k+1)
        bucket_idx = buffer_b(k+1)
        cnt_b = counters(bucket_idx)
        size_b = sizes(bucket_idx)
        if ( cnt_b .gt. size_b ) then
            allocate(tmp_x(size_b), tmp_y(size_b))
            do j=1, size_b, 1
                tmp_x(j) = works(bucket_idx) % x(j)
                tmp_y(j) = works(bucket_idx) % y(j)
            end do
            deallocate(works(bucket_idx) % x, works(bucket_idx) % y)
            allocate(works(bucket_idx) % x(size_b*two))
            allocate(works(bucket_idx) % y(size_b*two))
            do j=1, size_b, 1
                works(bucket_idx) % x(j) = tmp_x(j)
                works(bucket_idx) % y(j) = tmp_y(j)
            end do
            deallocate(tmp_x, tmp_y)
            sizes(bucket_idx) = size_b*two
        end if
        works(bucket_idx) % x(cnt_b) = tmp_v
        works(bucket_idx) % y(cnt_b) = tmp_i
        counters(bucket_idx) = cnt_b + 1
    end do
end do

do i=n_samples_unroll+1, n_samples, 1
    tmp_v = vector(i)
    tmp_i = indices(i)
    bucket_idx = int( tmp_v * factor + diff, kind=kind(bucket_idx))
    cnt_b = counters(bucket_idx)
    size_b = sizes(bucket_idx)

    if ( cnt_b .gt. size_b ) then
        allocate(tmp_x(size_b), tmp_y(size_b))
        do j=1, size_b, 1
            tmp_x(j) = works(bucket_idx) % x(j)
            tmp_y(j) = works(bucket_idx) % y(j)
        end do
        deallocate(works(bucket_idx) % x, works(bucket_idx) % y)
        allocate(works(bucket_idx) % x(size_b*two), works(bucket_idx) % y(size_b*two))
        do j=1, size_b, 1
            works(bucket_idx) % x(j) = tmp_x(j)
            works(bucket_idx) % y(j) = tmp_y(j)
        end do
        deallocate(tmp_x, tmp_y)
        sizes(bucket_idx) = size_b*two
    end if
    works(bucket_idx) % x(cnt_b) = tmp_v
    works(bucket_idx) % y(cnt_b) = tmp_i

    counters(bucket_idx) = cnt_b + 1
end do

if (rec_depth_opt .le. max_rec_depth) then ! 0(Only One Time Recursion) is Best?
    ini = 1
    do b=1, n_buckets
        fin = ini + counters(b) - 2
        vector(ini:fin) = works(b) % x(1:counters(b)-1)
        indices(ini:fin) = works(b) % y(1:counters(b)-1)
        call pbucket_argsort(vector(ini:fin), indices(ini:fin), fin-ini+1, rec_depth_opt+1)
        ini = fin + 1
    end do
else
    ! Sort Arrays
    ini = 1
    do b=1, n_buckets
        fin = ini + counters(b) - 2
        call quick_argsort(works(b) % x(1:counters(b)-1), works(b) % y(1:counters(b)-1), counters(b)-1)
        vector(ini:fin) = works(b) % x(1:counters(b)-1)
        indices(ini:fin) = works(b) % y(1:counters(b)-1)
        ini = fin + 1
    end do
end if
