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
    true_indices = [i for i, val in enumerate(lst) if val]
    return true_indices[:N]


import re

def replace_multiple_newlines(text):
    # 3つ以上の連続する改行を2つの改行に置換
    return re.sub(r'\n{2,}', '\n\n', remove_spaces_from_blank_lines(text))

def remove_spaces_from_blank_lines(text):
    # 各行を処理し、スペースのみの行を空行に置き換える
    return re.sub(r'^\s+$', '', text, flags=re.MULTILINE)
def select_usable_zmm(usable_var):
    count_true = sum(usable_var)
    count_mod2 = (count_true>>1)<<1
    count_half = count_true>>1
    indices = find_true_indices(usable_var, count_mod2)
    
    vars1 = [f"zmm{idx}" for idx in indices[:count_half]]
    vars2 = [f"zmm{idx}" for idx in indices[count_half:]]
    for idx in indices:
        usable_var[idx] = False
    return vars1, vars2, usable_var 

def find_true_indices(lst, N):
    true_indices = [i for i, val in enumerate(lst) if val]
    return true_indices[:N]

def select_usable_var(usable_var, prefix="r", set_second=False):
    var1, var2 = "", ""
    indices = find_true_indices(usable_var, 2)
    
    var1 = f"{prefix}{indices[0]}"
    var2 = f"{prefix}{indices[1]}"
    usable_var[indices[0]] = False
    if set_second: usable_var[indices[1]] = False
    return var1, var2, usable_var

def accumulator_naive_c(accm_op, variables, indent_level=1):
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
        
    accumulator_naive_c(accm_op, variables_next_, indent_level = indent_level)


with open(f"./mydgemv_{hash_val}.c", "w") as p:
    p.write("#include <stddef.h>\n")
    p.write("#include <stdio.h>\n")
    p.write("#include <stdlib.h>\n")
    p.write("#include <stdint.h>\n")
    p.write("#include <math.h>\n")
    p.write("#include <omp.h>\n")
    p.write("#include <immintrin.h>\n") 

    ldx_max = min([n_columns//4, 16-1])
    declare_zmm = "__m512d " + ", ".join([f"zmm{ld}" for ld in range(ldx_max)])
    variables = [f"zmm{ld}" for ld in range(ldx_max)]

    usable_var = [True] * ldx_max
    
    vars1, vars2, usable_var = select_usable_zmm(usable_var)

    load_zmm = ""
    op_concat = ""
    
    if True:
        if n_columns <=16:
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
            accumulator_naive_c(accm_op, accm_variables, indent_level=2)
            op_concat += "\n".join(accm_op[:-1])

            op_concat += f"\n        y[i] = sum_zmm_elements_v3_5({accm_op[-1]}); a_t += {n_columns};"

        else:
            for i, var in enumerate(vars2):
                op_concat += f"""
        {var} = _mm512_loadu_pd(&x[{i*8}]);"""
            for i, var in enumerate(vars1):
                op_concat += f"""
        {var} = _mm512_loadu_pd(&a_t[{i*8}]);"""
                
            for var1, var2 in zip(vars1, vars2):
                op_concat += f"""
        {var1} = _mm512_mul_pd({var1}, {var2});"""

            accm_variables = vars1
            accm_op = []
            accumulator_naive_c(accm_op, accm_variables, indent_level=2)
            op_concat += "\n".join(accm_op[:-1])
            for var1, var2 in zip(vars1, vars2):
                usable_var[int(var1[3:])] = True
                usable_var[int(var2[3:])] = True
            usable_var[0] = False

            collect_var1 = []
            collect_var2 = []
            collect_i = []
            for i in range(len(vars1)*8, n_columns, 8):
                var1, var2, usable_var = select_usable_var(usable_var, prefix="zmm", set_second=True)
                collect_var1.append(var1)
                collect_var2.append(var2)
                collect_i.append(i)

                if sum(usable_var)<=1:
                    for iii, var1 in enumerate(collect_var1):
                        op_concat += f"""
                        {var1} = _mm512_loadu_pd(&x[{collect_i[iii]}]);"""
                    for iii, var2 in enumerate(collect_var2):
                        op_concat += f"""
                        {var2} = _mm512_loadu_pd(&a_t[{collect_i[iii]}]);"""
                    for var1, var2 in zip(collect_var1, collect_var2):
                        op_concat += f"""
                        {var1} = _mm512_mul_pd({var1}, {var2});"""

                    accm_op = []
                    accm_variables = ["zmm0"] + collect_var1

                    accumulator_naive_c(accm_op, accm_variables, indent_level=4)
                    op_concat += "\n".join(accm_op[:-1])
                    for var in accm_variables[1:]:
                        idx = int(var[3:])
                        usable_var[idx] = True        
                    collect_var1 = []
                    collect_var2 = []
                    collect_i = []

            if len(collect_var1) != 0:
                for iii, var1 in enumerate(collect_var1):
                    op_concat += f"""
                    {var1} = _mm512_loadu_pd(&x[{collect_i[iii]}]);"""
                for iii, var2 in enumerate(collect_var2):
                    op_concat += f"""
                    {var2} = _mm512_loadu_pd(&a_t[{collect_i[iii]}]);"""
                for var1, var2 in zip(collect_var1, collect_var2):
                    op_concat += f"""
                    {var1} = _mm512_mul_pd({var1}, {var2});"""

                accm_op = []
                accm_variables = ["zmm0"] + collect_var1

                accumulator_naive_c(accm_op, accm_variables, indent_level=4)
                op_concat += "\n".join(accm_op[:-1])
                for var in accm_variables[1:]:
                    idx = int(var[3:])
                    usable_var[idx] = True        
                collect_var1 = []
                collect_var2 = []
                collect_i = []

            op_concat += f"\n        y[i] -= 2.0 * _mm512_reduce_add_pd({accm_op[-1]}); a_t += {n_columns};"
        
    base = f"""
void mydgemv_{hash_val}(double a_t[], double x[], double y[], int64_t lda, int64_t ldx, int64_t ldy){{
{declare_zmm};
{load_zmm}

for (int64_t i=0; i<lda; i++){{
    {op_concat}
    
}}
}}
    """
    
    p.write(base)