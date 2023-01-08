import pandas as pd
import numpy as np
from sklearn.datasets import make_regression, make_classification, make_blobs
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import DBSCAN
from sklearn.neighbors import KDTree
import subprocess


# n_samples_list = [1000]
# n_columns_list = [5, 100]
n_samples_list = [100, 1000, 10000, 100000, 1000000]
n_columns_list = [5, 10, 50, 100, 200, 400]
ratio_test  = 0.2
ratio_valid = 0.2

subprocess.run("make main_csv2bin", shell=True, check=True)


dtype_in = "r"
dtype_out = "r"
for n_samples_train in n_samples_list:
    n_samples_valid = int(n_samples_train*ratio_valid)
    n_samples_test  = int(n_samples_train*ratio_test)
    for n_columns in n_columns_list:
        print("Creating Regression Datasets ***************************************************************************************")
        print("     Tarin: #Samples={}, #Columns={}".format(n_samples_train, n_columns))
        print("     Valid: #Samples={}, #Columns={}".format(n_samples_valid, n_columns))
        print("     Test:  #Samples={}, #Columns={}".format(n_samples_test, n_columns))
        X, y = make_regression(n_samples=n_samples_train+n_samples_valid+n_samples_test, n_features=n_columns)

        list_data_seps = ["train", "valid", "test"]
        list_n_samples = [n_samples_train, n_samples_valid, n_samples_test]
        n_s = 0
        n_e = 0
        for d_sep, n_sam in zip(list_data_seps, list_n_samples):
            n_e = n_s + n_sam

            file_name_x_out = "../sample_data/make_regression_X_{0}_{1:0=10}x{2:0=5}.csv".format(d_sep, n_samples_train, n_columns)
            file_name_y_out = "../sample_data/make_regression_y_{0}_{1:0=10}x{2:0=5}.csv".format(d_sep, n_samples_train, n_columns)
            pd.DataFrame(X[n_s:n_e,:]).to_csv(file_name_x_out, index=False)
            pd.DataFrame(y[n_s:n_e]).to_csv(file_name_y_out, index=False)

            subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {n_columns} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {dtype_in} {dtype_out}", shell=True, check=True)
            subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {1}         {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} {dtype_in} {dtype_out}", shell=True, check=True)
            n_s = n_e



n_classes = 2
dtype_in = "r"
dtype_out = "r"
for n_samples_train in n_samples_list:
    n_samples_valid = int(n_samples_train*ratio_valid)
    n_samples_test  = int(n_samples_train*ratio_test)
    for n_columns in n_columns_list:
        print("Creating Classification Datasets ***************************************************************************************")
        print("     Tarin: #Samples={}, #Columns={}".format(n_samples_train, n_columns))
        print("     Valid: #Samples={}, #Columns={}".format(n_samples_valid, n_columns))
        print("     Test:  #Samples={}, #Columns={}".format(n_samples_test, n_columns))
        X, y = make_classification(n_samples=n_samples_train+n_samples_valid+n_samples_test, n_features=n_columns)

        list_data_seps = ["train", "valid", "test"]
        list_n_samples = [n_samples_train, n_samples_valid, n_samples_test]
        n_s = 0
        n_e = 0
        for d_sep, n_sam in zip(list_data_seps, list_n_samples):
            n_e = n_s + n_sam

            file_name_x_out = "../sample_data/make_classification_X_{0}_{1:0=10}x{2:0=5}_class={3:0=3}.csv".format(d_sep, n_samples_train, n_columns, n_classes)
            file_name_y_out = "../sample_data/make_classification_y_{0}_{1:0=10}x{2:0=5}_class={3:0=3}.csv".format(d_sep, n_samples_train, n_columns, n_classes)
            pd.DataFrame(X[n_s:n_e,:]).to_csv(file_name_x_out, index=False)
            pd.DataFrame(y[n_s:n_e]).to_csv(file_name_y_out, index=False)

            subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {n_columns} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {dtype_in} {dtype_out}", shell=True, check=True)
            subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {1}         {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} {dtype_in} i", shell=True, check=True)
            n_s = n_e


n_classes = 3
dtype_in = "r"
dtype_out = "r"
for n_samples_train in n_samples_list:
    n_samples_valid = int(n_samples_train*ratio_valid)
    n_samples_test  = int(n_samples_train*ratio_test)
    for n_columns in n_columns_list:
        print("Creating Classification Datasets ***************************************************************************************")
        print("     Tarin: #Samples={}, #Columns={}".format(n_samples_train, n_columns))
        print("     Valid: #Samples={}, #Columns={}".format(n_samples_valid, n_columns))
        print("     Test:  #Samples={}, #Columns={}".format(n_samples_test, n_columns))
        X, y = make_classification(n_samples=n_samples_train+n_samples_valid+n_samples_test, n_features=n_columns, n_classes=n_classes)

        list_data_seps = ["train", "valid", "test"]
        list_n_samples = [n_samples_train, n_samples_valid, n_samples_test]
        n_s = 0
        n_e = 0
        for d_sep, n_sam in zip(list_data_seps, list_n_samples):
            n_e = n_s + n_sam

            file_name_x_out = "../sample_data/make_classification_X_{0}_{1:0=10}x{2:0=5}_class={3:0=3}.csv".format(d_sep, n_samples_train, n_columns, n_classes)
            file_name_y_out = "../sample_data/make_classification_y_{0}_{1:0=10}x{2:0=5}_class={3:0=3}.csv".format(d_sep, n_samples_train, n_columns, n_classes)
            pd.DataFrame(X[n_s:n_e,:]).to_csv(file_name_x_out, index=False)
            pd.DataFrame(y[n_s:n_e]).to_csv(file_name_y_out, index=False)

            subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {n_columns} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {dtype_in} {dtype_out}", shell=True, check=True)
            subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {1}         {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} {dtype_in} i", shell=True, check=True)
            n_s = n_e

# n_classes = 3
# dtype_in = "r"
# dtype_out = "r"
# for n_samples_train in n_samples_list:
#     n_samples_valid = int(n_samples_train*ratio_valid)
#     n_samples_test  = int(n_samples_train*ratio_test)
#     for n_columns in n_columns_list:
#         print("     Tarin: #Samples={}, #Columns={}".format(n_samples_train, n_columns))
#         print("     Valid: #Samples={}, #Columns={}".format(n_samples_valid, n_columns))
#         print("     Test:  #Samples={}, #Columns={}".format(n_samples_test, n_columns))
#         X, y = make_classification(n_samples=n_samples_train+n_samples_valid+n_samples_test, n_features=n_columns, n_informative=3, n_classes=n_classes)

#         list_data_seps = ["train", "valid", "test"]
#         list_n_samples = [n_samples_train, n_samples_valid, n_samples_test]
#         n_s = 0
#         n_e = 0
#         for d_sep, n_sam in zip(list_data_seps, list_n_samples):
#             n_e = n_s + n_sam

#             file_name_x_out = "../sample_data/make_classification_X_{0}_{1:0=10}x{2:0=5}_class={3:0=3}.csv".format(d_sep, n_samples_train, n_columns, n_classes)
#             file_name_y_out = "../sample_data/make_classification_y_{0}_{1:0=10}x{2:0=5}_class={3:0=3}.csv".format(d_sep, n_samples_train, n_columns, n_classes)
#             pd.DataFrame(X[n_s:n_e,:]).to_csv(file_name_x_out, index=False)
#             pd.DataFrame(y[n_s:n_e]).to_csv(file_name_y_out, index=False)

#             subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {n_columns} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {dtype_in} {dtype_out}", shell=True, check=True)
#             subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {1}         {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} {dtype_in} i", shell=True, check=True)
#             n_s = n_e


# print("Create Clustering Datasets")
# centers = [[1, 1], [-1, -1], [1, -1]]
# X, labels_true = make_blobs(
#         n_samples=750, centers=centers, cluster_std=0.4, random_state=0
#     )
# X = StandardScaler().fit_transform(X)
# pd.DataFrame(X).to_csv("../sample_data/make_brobs_X_750x2.csv", index=False)
# pd.DataFrame(labels_true).to_csv("../sample_data/make_brobs_Y_750x2.csv", index=False)
# db = DBSCAN(eps=0.25, min_samples=8).fit(X)
# pd.DataFrame([x+1 for x in db.labels_]).to_csv("../sample_data/make_brobs_DBSCAN_750x2.csv", index=False)


# print("Create Nearest Neighbor Search Datasets")
# n_samples_list = [100000 + 100] # Sample + Query
# n_columns_list = [100]
# for n_samples in n_samples_list:
#     for n_columns in n_columns_list:
#         print("     #Samples={}, #Columns={}".format(n_samples, n_columns))
#         X, _ = make_regression(n_samples=n_samples, n_features=n_columns)
        
#         pd.DataFrame(X[:-100,:]).to_csv("../sample_data/nnsearch_X_{0:0=10}x{1:0=5}.csv".format(len(X[:-100,:]), n_columns), index=False)
#         pd.DataFrame(X[-100:,:]).to_csv("../sample_data/nnsearch_Q_{0:0=10}x{1:0=5}.csv".format(len(X[-100:,:]), n_columns), index=False)

#         tree = KDTree(X[:-100,:], leaf_size=32)
#         dst, ind = tree.query(X[-100:,:], k=100000)

#         ind = ind + 1
#         pd.DataFrame(dst).to_csv("../sample_data/nnsearch_D_{0:0=10}x{1:0=5}.csv".format(100, 100000), index=False)
#         pd.DataFrame(ind).to_csv("../sample_data/nnsearch_I_{0:0=10}x{1:0=5}.csv".format(100, 100000), index=False)

print("Create Hard-Margin SVM Dataset (small)")
np.random.seed(0)
x0 = np.random.randn(50, 2)
x1 = np.random.randn(50, 2) + np.array([5,5])
y = np.array([1]*50 + [0]*50)

x = np.r_[x0,x1]
d_sep = "train"
n_sam = 100
n_columns = 2
file_name_x_out = "../sample_data/svm_hard_x_100x2.csv"
file_name_y_out = "../sample_data/svm_hard_y_100x1.csv"
pd.DataFrame(x).to_csv(file_name_x_out, index=False)
pd.DataFrame(y).to_csv(file_name_y_out, index=False)

subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {n_columns} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {'r'} {'r'}", shell=True, check=True)
subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {1}         {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} {'i'} {'i'}", shell=True, check=True)


print("Create Soft-Margin SVM Dataset (small)")
np.random.seed(0)
x0 = np.random.randn(50, 2)
x1 = np.random.randn(50, 2) + np.array([2.5,3])
y = np.array([1]*50 + [0]*50)

x = np.r_[x0,x1]
d_sep = "train"
n_sam = 100
n_columns = 2
file_name_x_out = "../sample_data/svm_soft_x_100x2.csv"
file_name_y_out = "../sample_data/svm_soft_y_100x1.csv"
pd.DataFrame(x).to_csv(file_name_x_out, index=False)
pd.DataFrame(y).to_csv(file_name_y_out, index=False)

subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {n_columns} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {'r'} {'r'}", shell=True, check=True)
subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {1}         {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} {'i'} {'i'}", shell=True, check=True)


