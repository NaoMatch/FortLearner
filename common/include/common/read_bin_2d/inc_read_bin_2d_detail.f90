if ( allocated(matrix) ) deallocate(matrix)
print_log_opt = t_
if (present(print_log)) then
    print_log_opt = print_log
end if

if (print_log_opt) then
    print*, "Input File Name is", trim(file_name)
end if
open(10, file=file_name, form='unformatted')

read(10) nd_file
if (nd .ne. nd_file) goto 111

read(10) dtype
if (dtype .ne. dtype_set) goto 222

read(10) n_samples, n_columns
read(10) bit_size_file
if (bit_size .ne. bit_size_file) goto 333

if (print_log_opt) then
    print*, "Load Data ..."
    print*, "         ", trim(file_name)
end if
allocate(matrix(n_samples, n_columns))
read(10) matrix
close(10)
return

111 continue
print*, "File Data Shape and Input Array Data Shape missmatch."
print*, "    size(shape(array))"
print*, "    file  : ", nd_file
print*, "    array : ", nd
stop

222 continue
print*, "File Data type and Input Array Data type missmatch."
print*, "    r=real, i=integer"
print*, "    file  : ", dtype
print*, "    array : ", dtype_set
stop

333 continue
print*, "File bit size and Input Array bit size missmatch."
print*, "    32 or 64"
print*, "    file  : ", bit_size_file
print*, "    array : ", bit_size
stop
