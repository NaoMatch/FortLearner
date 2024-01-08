compiler_fortran := gfortran-7
compiler_clang   := gcc-8
opt              := -O2
option           := -cpp -fopenmp
option_degug     := 
option_blas      := -lopenblas -ldl
mod              := -J mod/

common           := 	./mod/mod_const.out \
						./mod/mod_common.out \
						./mod/mod_common_type.out \
						./mod/mod_heap.out \
						./mod/mod_timer.out \
						./mod/mod_error.out \
						./mod/mod_random.out

build:
	$(compiler_fortran) $(mod) $(opt) $(option) -g -w -c ../../common/mod_const.f90 -o ./mod/mod_const.out
	$(compiler_fortran) $(mod) $(opt) $(option) -g -w -c ../../common/mod_common.f90 -o ./mod/mod_common.out
	$(compiler_fortran) $(mod) $(opt) $(option) -g -w -c ../../common/mod_common_type.f90 -o ./mod/mod_common_type.out
	$(compiler_fortran) $(mod) $(opt) $(option) -g -w -c ../../common/mod_heap.f90 -o ./mod/mod_heap.out
	$(compiler_fortran) $(mod) $(opt) $(option) -g -w -c ../../common/mod_timer.f90 -o ./mod/mod_timer.out
	$(compiler_fortran) $(mod) $(opt) $(option) -g -w -c ../../error/mod_error.f90 -o ./mod/mod_error.out
	$(compiler_fortran) $(mod) $(opt) $(option) -g -w -c ../../common/mod_random.f90 -o ./mod/mod_random.out
	$(compiler_fortran) $(mod) $(opt) $(option) -g -w -c ../../common/mod_hash.f90 -o ./mod/mod_hash.out
	$(compiler_fortran) $(mod) $(opt) $(option) -g -w -c ../../common/mod_sort.f90 -o ./mod/mod_sort.out


del:
	rm main.out
	rm ./mod/mod_*.out
	rm ./mod/mod_*.mod

main:
	# make build

	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-0_Naive.c -o ./mod/small_dgemv_naive_c_implementation_ver0-0_Naive.out

	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-1.c -o ./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-1.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-2.c -o ./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-2.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-3.c -o ./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-3.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-4.c -o ./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-4.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-5.c -o ./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-5.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-6.c -o ./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-6.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-7.c -o ./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-7.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-8.c -o ./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-8.out

	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-1.c -o ./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-1.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-2.c -o ./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-2.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-3.c -o ./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-3.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-4.c -o ./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-4.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-5.c -o ./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-5.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-6.c -o ./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-6.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-7.c -o ./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-7.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-8.c -o ./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-8.out

	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-4_Naive-Unroll-Complete.c -o ./mod/small_dgemv_naive_c_implementation_ver0-4_Naive-Unroll-Complete.out
	# $(compiler_clang) -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver0-5_Naive-Unroll-16vars.c -o ./mod/small_dgemv_naive_c_implementation_ver0-5_Naive-Unroll-16vars.out

	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-0_XmmNaive.c -o ./mod/small_dgemv_naive_c_implementation_ver1-0_XmmNaive.out

	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-1.c -o ./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-1.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-2.c -o ./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-2.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-3.c -o ./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-3.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-4.c -o ./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-4.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-5.c -o ./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-5.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-6.c -o ./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-6.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-7.c -o ./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-7.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-8.c -o ./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-8.out

	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-1.c -o ./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-1.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-2.c -o ./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-2.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-3.c -o ./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-3.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-4.c -o ./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-4.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-5.c -o ./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-5.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-6.c -o ./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-6.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-7.c -o ./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-7.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-8.c -o ./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-8.out

	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1_4.c -o ./mod/small_dgemv_naive_c_implementation_ver1_4.out
	# $(compiler_clang) -msse -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver1_5.c -o ./mod/small_dgemv_naive_c_implementation_ver1_5.out

	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-0_YmmNaive.c -o ./mod/small_dgemv_naive_c_implementation_ver2-0_YmmNaive.out

	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-1.c -o ./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-1.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-2.c -o ./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-2.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-3.c -o ./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-3.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-4.c -o ./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-4.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-5.c -o ./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-5.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-6.c -o ./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-6.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-7.c -o ./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-7.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-8.c -o ./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-8.out

	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-1.c -o ./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-1.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-2.c -o ./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-2.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-3.c -o ./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-3.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-4.c -o ./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-4.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-5.c -o ./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-5.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-6.c -o ./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-6.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-7.c -o ./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-7.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-8.c -o ./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-8.out

	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2_4.c -o ./mod/small_dgemv_naive_c_implementation_ver2_4.out
	# $(compiler_clang) -mavx -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver2_5.c -o ./mod/small_dgemv_naive_c_implementation_ver2_5.out

	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-0_ZmmNaive.c -o ./mod/small_dgemv_naive_c_implementation_ver3-0_ZmmNaive.out

	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-1.c -o ./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-1.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-2.c -o ./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-2.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-3.c -o ./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-3.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-4.c -o ./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-4.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-5.c -o ./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-5.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-6.c -o ./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-6.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-7.c -o ./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-7.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-8.c -o ./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-8.out

	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-1.c -o ./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-1.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-2.c -o ./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-2.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-3.c -o ./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-3.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-4.c -o ./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-4.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-5.c -o ./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-5.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-6.c -o ./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-6.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-7.c -o ./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-7.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-8.c -o ./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-8.out

	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3_4.c -o ./mod/small_dgemv_naive_c_implementation_ver3_4.out
	# $(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./small_dgemv_naive_c_implementation_ver3_5.c -o ./mod/small_dgemv_naive_c_implementation_ver3_5.out

	$(compiler_clang) -mavx512f -mavx512dq -unroll -O3 -J mod/ -c ./mydgemv_verX.c -o ./mod/mydgemv_verX.out

	$(compiler_fortran) -J mod/ -w -c ./mod_small_gemv.f90 -o ./mod/mod_small_gemv.out

	$(compiler_fortran) -w \
		./mod/small_dgemv_naive_c_implementation_ver0-0_Naive.out \
		./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-1.out \
		./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-2.out \
		./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-4.out \
		./mod/small_dgemv_naive_c_implementation_ver0-1_Naive-Unroll-LDA-8.out \
		./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-1.out \
		./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-2.out \
		./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-4.out \
		./mod/small_dgemv_naive_c_implementation_ver0-2_Naive-Unroll-LDX-8.out \
		./mod/small_dgemv_naive_c_implementation_ver0-4_Naive-Unroll-Complete.out \
		./mod/small_dgemv_naive_c_implementation_ver0-5_Naive-Unroll-16vars.out \
		./mod/small_dgemv_naive_c_implementation_ver1-0_XmmNaive.out \
		./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-1.out \
		./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-2.out \
		./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-4.out \
		./mod/small_dgemv_naive_c_implementation_ver1-1_XmmNaive_Unroll-InnerLoop-8.out \
		./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-1.out \
		./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-2.out \
		./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-4.out \
		./mod/small_dgemv_naive_c_implementation_ver1-2_XmmNaive_Unroll-OuterLoop-8.out \
		./mod/small_dgemv_naive_c_implementation_ver1_4.out \
		./mod/small_dgemv_naive_c_implementation_ver1_5.out \
		./mod/small_dgemv_naive_c_implementation_ver2-0_YmmNaive.out \
		./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-1.out \
		./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-2.out \
		./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-4.out \
		./mod/small_dgemv_naive_c_implementation_ver2-1_YmmNaive_Unroll-InnerLoop-8.out \
		./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-1.out \
		./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-2.out \
		./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-4.out \
		./mod/small_dgemv_naive_c_implementation_ver2-2_YmmNaive_Unroll-OuterLoop-8.out \
		./mod/small_dgemv_naive_c_implementation_ver2_4.out \
		./mod/small_dgemv_naive_c_implementation_ver2_5.out \
		./mod/small_dgemv_naive_c_implementation_ver3-0_ZmmNaive.out \
		./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-1.out \
		./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-2.out \
		./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-4.out \
		./mod/small_dgemv_naive_c_implementation_ver3-1_ZmmNaive_Unroll-InnerLoop-8.out \
		./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-1.out \
		./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-2.out \
		./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-4.out \
		./mod/small_dgemv_naive_c_implementation_ver3-2_ZmmNaive_Unroll-OuterLoop-8.out \
		./mod/small_dgemv_naive_c_implementation_ver3_4.out \
		./mod/small_dgemv_naive_c_implementation_ver3_5.out \
		./mod/mydgemv_verX.out \
		-J mod/ $(common) ./mod/mod_small_gemv.out ./main.f90 -o ./main.out -lopenblas -ldl

	./main.out