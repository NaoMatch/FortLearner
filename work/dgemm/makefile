compiler_fortran := gfortran
compiler_clang   := gcc
mod              := -J mod/
opt              := -O3
option           := -cpp -D_x86_64 -fopenmp
option_blas      := -lopenblas

build:
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_const.f90                                                                -o ./mod/mod_const.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_common.f90                                                               -o ./mod/mod_common.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_timer.f90                                                                -o ./mod/mod_timer.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../error/mod_error.f90                                                                 -o ./mod/mod_error.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_random.f90                                                               -o ./mod/mod_random.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_sort.f90                                                                 -o ./mod/mod_sort.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../math/mod_math.f90                                                                   -o ./mod/mod_math.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../math/mod_stats.f90                                                                  -o ./mod/mod_stats.out

main:
	$(compiler_clang)   $(mod) $(opt) $(option) -c    ./mod_dgemm.c -o ./mod/mod_dgemm_c.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ./mod_dgemm.f90 -o ./mod/mod_dgemm.out
	$(compiler_fortran) -w -unroll \
	./mod/mod_dgemm_c.out ./mod/mod_dgemm.out \
	$(option) $(mod) $(opt) \
	main.f90 -o main.out \
	$(option_blas)
	./main.out
