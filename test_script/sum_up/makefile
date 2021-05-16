build:
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
	gcc -c sum.c -O3
	gfortran -w -c main_sum_up_vector.f90 -O3
	gfortran \
	mod_const.o \
	mod_common.o \
	mod_timer.o \
	mod_random.o \
	mod_sort.o \
	mod_stats.o \
	main_sum_up_vector.o  sum.o \
	-o main_sum_up_vector.out
	./main_sum_up_vector.out

