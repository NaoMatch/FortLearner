if (mod(loop_index, step_size_mod) .ne. 0) return
print'(a,$)',' ['
do j=1, loop_index * ndigit / loop_max_index
    print'(a,$)','='
enddo
print'(a,$)',''

do j=loop_index * ndigit / loop_max_index+1, ndigit
    print'(a,$)',' '
enddo
print'(a,$)',']'
print'(f6.1,a,a,$)', 100d0 * loop_index / loop_max_index,'%',char(13)
if(loop_index .eq. loop_max_index)print*,''
