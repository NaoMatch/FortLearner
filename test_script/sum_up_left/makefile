build:
	make rm
	gfortran -w -c ../../common/mod_const.f90
	gfortran -w -c ../../common/mod_common.f90
	gfortran -w -c ../../common/mod_timer.f90
	gfortran -w -c ../../common/mod_random.f90
	gfortran -w -c ../../common/mod_sort.f90
	gfortran -w -c ../../math/mod_stats.f90

rm:
	rm *.o
	rm *.out

run:
	gcc -c ../../math/include/stats/sum_up/inc_sum_up.c -O3 -o inc_sum_up_c.out
	gcc -c sum_up_left.c -O3
	gfortran -w -c main_sum_up_left.f90 -O3
	gfortran \
	mod_const.o \
	mod_common.o \
	mod_timer.o \
	mod_random.o \
	mod_sort.o \
	mod_stats.o \
	./inc_sum_up_c.out \
	main_sum_up_left.o  sum_up_left.o \
	-o main_sum_up_left.out
	./main_sum_up_left.out

