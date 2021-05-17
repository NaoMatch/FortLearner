build:
	gfortran -w -c -O3 ../../common/mod_const.f90 
	gfortran -w -c -O3 ../../common/mod_common.f90 
	gfortran -w -c -O3 ../../common/mod_timer.f90 
	gfortran -w -c -O3 ../../common/mod_random.f90 
	gfortran -w -c -O3 ../../math/mod_math.f90 
	gfortran -w -c -O3 ../../common/mod_sort.f90

run:
	gcc -c insertion_sort.c -O3
	gfortran -c main.f90 -O3
	gfortran -o main.out mod_const.o mod_common.o mod_timer.o mod_random.o mod_math.o mod_sort.o main.o insertion_sort.o -O3
	./main.out
