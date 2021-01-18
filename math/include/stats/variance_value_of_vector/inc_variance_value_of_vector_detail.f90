if ( present(mean_of_vector) ) then
    mean_of_vector_opt = mean_of_vector
else
    mean_of_vector_opt = mean(vector, n_samples)
end if
tmp_sq_sum = 0
n_samples_unroll = n_samples - mod(n_samples, len_buffer)

do i=1, n_samples_unroll, len_buffer
    do j=0, len_buffer-1, 1
        buffer(j+1) = real(vector(i+j), kind=kind(val))-mean_of_vector_opt
    end do
    
    do j=0, len_buffer-1, 1
        tmp_sq_sum = tmp_sq_sum + buffer(j+1) * buffer(j+1)
    end do
end do

do i=n_samples_unroll+1, n_samples
    val = real(vector(i), kind=kind(val))-mean_of_vector_opt
    tmp_sq_sum = tmp_sq_sum +  val*val
end do
