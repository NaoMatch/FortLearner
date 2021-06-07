build:
	gfortran -w -c ../../common/mod_const.f90
	gfortran -w -c ../../common/mod_common.f90
	gfortran -w -c ../../common/mod_timer.f90
	gfortran -w -c ../../common/mod_random.f90
	gfortran -w -c ../../common/mod_sort.f90
	gfortran -w -c ../../math/mod_stats.f90
	gcc -c ../../math/include/stats/sum_up/inc_sum_up.c           -O3 -o inc_sum_up_c.out
	gcc -c ../../math/include/stats/sum_up_left/inc_sum_up_left.c -O3 -o inc_sum_up_left_c.out

rm:
	rm *.o
	rm *.out

run:
	gcc -c minmax.c -O3 -o minmax.out
	gfortran -w -c main_minmax.f90 -O3
	gfortran \
	mod_const.o \
	mod_common.o \
	mod_timer.o \
	mod_random.o \
	mod_sort.o \
	mod_stats.o \
	main_minmax.o \
	./inc_sum_up_c.out ./inc_sum_up_left_c.out ./minmax.out \
	-o main_minmax.out
	./main_minmax.out
