asm              := ./mod/inc_sum_up_vector_c.out \
					./mod/inc_sum_up_matrix_c.out \
                    ./mod/inc_sum_up_left_c.out   \
					./mod/inc_get_minmax.out

compiler_fortran := gfortran
compiler_clang   := gcc
mod              := -J mod/

# Preprocessor
# -D_default: use Fortran only
# -D_x86_64:  use Fortran & inline assembler with x86_64
option := -cpp -D_x86_64 -fopenmp

run:
	make build
	make main_extract_m2v

build:
	$(compiler_clang) $(mod) $(option) -c ../../math/include/stats/sum_up/inc_sum_up_vector.c    -O3 -o ./mod/inc_sum_up_vector_c.out
	$(compiler_clang) $(mod) $(option) -c ../../math/include/stats/sum_up/inc_sum_up_matrix.c    -O3 -o ./mod/inc_sum_up_matrix_c.out ./mod/inc_sum_up_vector_c.out
	$(compiler_clang) $(mod) $(option) -c ../../math/include/stats/sum_up_left/inc_sum_up_left.c -O3 -o ./mod/inc_sum_up_left_c.out
	$(compiler_clang) $(mod) $(option) -c ../../math/include/stats/get_minmax/inc_get_minmax.c   -O3 -o ./mod/inc_get_minmax.out


main_extract_m2v:
	$(compiler_fortran) -w -unroll -O3 \
	../../common/mod_const.f90 \
	../../common/mod_common.f90 \
	../../common/mod_timer.f90 \
	../../common/mod_random.f90 \
	../../common/mod_sort.f90 \
	../../math/mod_stats.f90 \
	$(asm)  $(option) $(mod) \
	main_extract_m2v.f90 -o main_extract_m2v.out
	./main_extract_m2v.out




