asm := ./inc_sum_up_c.out ./inc_sum_up_left_c.out  ./inc_get_minmax.out

compiler_fortran := gfortran -fbounds-check

compiler_clang := gcc

option := -cpp -D_x86_64 
# option := -cpp -D_default 

build:
	gfortran -O3 -w -c ../../common/mod_const.f90
	gfortran -O3 -w -c ../../common/mod_common.f90
	gfortran -O3 -w -c ../../common/mod_timer.f90
	gfortran -O3 -w -c ../../common/mod_random.f90
	gfortran -O3 -w -c ../../common/mod_sort.f90
	gfortran -O3 -w -c ../../math/mod_stats.f90
	$(compiler_clang) $(option) -c ../../math/include/stats/sum_up/inc_sum_up.c           -O3 -o inc_sum_up_c.out
	$(compiler_clang) $(option) -c ../../math/include/stats/sum_up_left/inc_sum_up_left.c -O3 -o inc_sum_up_left_c.out
	$(compiler_clang) $(option) -c ../../math/include/stats/get_minmax/inc_get_minmax.c   -O3 -o inc_get_minmax.out

rm:
	rm *.o
	rm *.out

run_m:
	gcc $(option) -c sum.c -O3
	gfortran -w -c main_sum_up_matrix.f90 -O3
	gfortran \
	mod_const.o \
	mod_common.o \
	mod_timer.o \
	mod_random.o \
	mod_sort.o \
	mod_stats.o \
	main_sum_up_matrix.o  $(asm) sum.o \
	-o main_sum_up_matrix.out
	./main_sum_up_matrix.out

run_v:
	gcc $(option) -c sum.c -O3
	gfortran -w -c main_sum_up_vector.f90 -O3
	gfortran \
	mod_const.o \
	mod_common.o \
	mod_timer.o \
	mod_random.o \
	mod_sort.o \
	mod_stats.o \
	main_sum_up_vector.o  $(asm) sum.o \
	-o main_sum_up_vector.out
	./main_sum_up_vector.out

