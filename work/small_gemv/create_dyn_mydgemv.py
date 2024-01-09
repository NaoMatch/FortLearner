import re
import sys
import copy

args = sys.argv

hash_val = args[1]
n_columns = int(args[2])

if n_columns % 8 != 0:
    sys.exit("カラム数は８の倍数以外は受け付けない")

if n_columns <= 0:
    sys.exit("カラム数は０より大きい８の倍数以外は受け付けない")

def find_true_indices(lst, N):
    # lst 内のTrueになっているインデックスをN個取得
    true_indices = [i for i, val in enumerate(lst) if val]
    return true_indices[:N]

def remove_spaces_from_blank_lines(text):
    # 各行を処理し、スペースのみの行を空行に置き換える
    return re.sub(r'^\s+$', '', text, flags=re.MULTILINE)

def replace_multiple_newlines(text):
    # 3つ以上の連続する改行を2つの改行に置換
    return re.sub(r'\n{2,}', '\n\n', remove_spaces_from_blank_lines(text))

def select_usable_zmm(usable_var):
    # usable_var (True/Falseが格納)内部から、利用可能なZMMインデックスを返し、また利用したインデックスをFalseとしたusable_varも同時に返す
    count_true = sum(usable_var)
    count_mod2 = (count_true>>1)<<1
    count_half = count_true>>1
    indices = find_true_indices(usable_var, count_mod2)
    
    vars1 = [f"zmm{idx}" for idx in indices[:count_half]]
    vars2 = [f"zmm{idx}" for idx in indices[count_half:]]
    for idx in indices:
        usable_var[idx] = False
    return vars1, vars2, usable_var 

def accumulator_simd(accm_op, variables, indent_level=1):
    indent = " " * 4 * indent_level
    
    accm_op.append("\n")
    if len(variables) == 1:
        accm_op.append(variables[0])
        return 

    variables_next_ = []
    variables_ = variables[:-1] if len(variables) % 2 == 1 else variables
    for i in range(0, len(variables_), 2):
        accm_op.append(f"{indent}{variables_[i]} = _mm512_add_pd({variables_[i]}, {variables_[i+1]});")
        variables_next_.append(f"{variables_[i]}")
        
    if len(variables) % 2 == 1:
        variables_next_.append(f"{variables[-1]}")
        
    accumulator_simd(accm_op, variables_next_, indent_level = indent_level)


def main():
    with open(f"./mydgemv_{hash_val}.c", "w") as p:
        p.write("#include <stddef.h>\n")
        p.write("#include <stdio.h>\n")
        p.write("#include <stdlib.h>\n")
        p.write("#include <stdint.h>\n")
        p.write("#include <math.h>\n")
        p.write("#include <omp.h>\n")
        p.write("#include <immintrin.h>\n") 
        p.write(f"""
double sum_zmm_elements_{hash_val}(__m512d zmmX){{
    __m256d ymm_upper = _mm512_extractf64x4_pd(zmmX, 1);
    __m256d ymm_lower = _mm512_extractf64x4_pd(zmmX, 0);

    __m256d ymm_sum = _mm256_add_pd(ymm_upper, ymm_lower);

    __m128d xmm_upper = _mm256_extractf128_pd(ymm_sum, 1);
    __m128d xmm_lower = _mm256_extractf128_pd(ymm_sum, 0);

    __m128d xmm_sum = _mm_add_pd(xmm_upper, xmm_lower);

    xmm_sum = _mm_hadd_pd(xmm_sum, xmm_sum);

    double result;
    _mm_store_sd(&result, xmm_sum);

    return result;
}}  
        """)


        ldx_max = n_columns // 4
        declare_zmm = "__m512d " + ", ".join([f"zmm{ld}" for ld in range(ldx_max)])
        variables = [f"zmm{ld}" for ld in range(ldx_max)]

        usable_var = [True] * ldx_max
        
        vars1, vars2, usable_var = select_usable_zmm(usable_var)
    
        load_zmm = ""
        op_concat = ""

        
        for i, var in enumerate(vars2):
            load_zmm += f"""
    {var} = _mm512_loadu_pd(&x[{i*8}]);"""
        
        for i, var in enumerate(vars1):
            op_concat += f"""
        {var} = _mm512_loadu_pd(&a_t[{i*8}]);"""

        op_concat += "\n"
        for var1, var2 in zip(vars1, vars2):
            op_concat += f"""
        {var1} = _mm512_mul_pd({var1}, {var2});"""

        accm_variables = vars1
        accm_op = []
        accumulator_simd(accm_op, accm_variables, indent_level=2)
        op_concat += "\n".join(accm_op[:-1])
        op_concat += f"\n        y[i] = sum_zmm_elements_{hash_val}({accm_op[-1]}); a_t += {n_columns};"
        
        base = f"""
void mydgemv_{hash_val}(double a_t[], double x[], double y[], int64_t lda, int64_t ldx, int64_t ldy){{
    {declare_zmm};
    {load_zmm}
    
    for (int i=0; i<lda; i++){{
        {op_concat}
        
    }}
}}
        """
        p.write(replace_multiple_newlines(base))









if __name__ == "__main__":
    main()



