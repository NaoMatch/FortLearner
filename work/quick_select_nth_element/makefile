compiler_fortran := gfortran
compiler_clang   := gcc
mod              := -J mod/
opt              := -O3
option           := -cpp -D_x86_64 -fopenmp -fbounds-check
option_blas      := -lopenblas

common           := 	./mod/mod_const.out \
						./mod/mod_common.out \
						./mod/mod_timer.out \
						./mod/mod_error.out \
						./mod/mod_random.out \
						./mod/mod_sort.out \

build:
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_const.f90                                                                -o ./mod/mod_const.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_common.f90                                                               -o ./mod/mod_common.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_timer.f90                                                                -o ./mod/mod_timer.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../error/mod_error.f90                                                                 -o ./mod/mod_error.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_random.f90                                                               -o ./mod/mod_random.out
	$(compiler_fortran) $(mod) $(opt) $(option) -w -c ../../common/mod_sort.f90                                                                 -o ./mod/mod_sort.out

main:
	$(compiler_fortran) -w -unroll \
	$(common) \
	$(option) $(mod) $(opt) \
	main.f90 -o main.out \
	$(option_blas)
	./main.out
