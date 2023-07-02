import os
import pandas as pd
import numpy as np
from sklearn.datasets import make_regression, make_classification, make_blobs, fetch_openml
from sklearn.datasets import load_diabetes, load_linnerud
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import DBSCAN
from sklearn.neighbors import KDTree
from sklearn.model_selection import StratifiedKFold
import subprocess
import glob
import mat73
import scipy.io

import sys
sys.path.append("./sample_data_preprocessing")
from kdd99 import *

subprocess.run("make main_csv2bin", shell=True, check=True)

# datasets = [load_diabetes(), load_linnerud()]
# for data in datasets:
#     try:
#         X, y = data["data"], data["target"][:,0]
#     except:
#         X, y = data["data"], data["target"]
#     n_rows, n_cols = X.shape
#     print(n_rows, n_cols)

#     file_name_x_out = f"../sample_data/{os.path.basename(data.data_filename)}".replace(".gz", "").replace(".csv.gz", "_x.csv")
#     file_name_y_out = f"../sample_data/{os.path.basename(data.data_filename)}".replace(".gz", "").replace(".csv.gz", "_y.csv")
#     pd.DataFrame(X).to_csv(file_name_x_out, index=False)
#     pd.DataFrame(y).to_csv(file_name_y_out, index=False)

#     # print(pd.DataFrame(X.astype(int)))

#     subprocess.run(f"./main_csv2bin.out train {n_rows} {n_cols} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)
#     subprocess.run(f"./main_csv2bin.out train {n_rows} {1}      {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} r r", shell=True, check=True)

# sys.exit()

# fn_kdd_10_percent = download_kdd99_10_percent("../sample_data/")
# fn_kdd_full = download_kdd99("../sample_data/")

# fn_kdd_10_percent_label_enc, fn_kdd_10_percent_one_hot_enc = preprocessing_kdd99(fn_kdd_10_percent)
# fn_kdd_full_label_enc, fn_kdd_full_one_hot_enc = preprocessing_kdd99(fn_kdd_full)

# split_train_valid_test_kdd99()

# # sys.exit()

# mat_fns = glob.glob("../sample_data/*.mat") + ["../sample_data/"]
# # mat_fns = ["../sample_data/creditcard.csv"]
# for fn in mat_fns:
#     FILE_NAME = fn
#     if (FILE_NAME.endswith(".mat")):
#         try:
#             data_dict = mat73.loadmat(FILE_NAME)
#         except:
#             data_dict = scipy.io.loadmat(FILE_NAME)
#         x = pd.DataFrame(data_dict["X"])
#         y = pd.DataFrame(data_dict["y"])
#         ext = ".mat"
#     else:
#         xy = pd.read_csv(FILE_NAME)
#         x = xy.iloc[:,:-1]
#         y = xy.iloc[:,-1:].astype(int)
#         ext = ".csv"

#     sum_flags = [0,0,0]

#     while (0 in sum_flags):
#         random_idx = np.random.permutation(list(range(len(x))))
#         x = x.iloc[random_idx,:]
#         y = y.iloc[random_idx,:]
        
#         skf = StratifiedKFold(n_splits=5)
#         all_indices = None
#         for train_index, test_index in skf.split(x, y):
#             all_indices = list(train_index) + list(test_index)
#             n_test = len(list(test_index))
#             break
#         test_idxs = all_indices[-n_test:]
#         valid_idxs = all_indices[-2*n_test:-n_test]
#         train_idxs = all_indices[:-2*n_test]
#         sum_flags = []
#         for idxs in [train_idxs, valid_idxs, test_idxs]:
#             sum_flags.append(y.iloc[idxs].sum()[0])
#     x_train = x.iloc[train_idxs,:]    
#     x_valid = x.iloc[valid_idxs,:]    
#     x_test  = x.iloc[test_idxs,:]
#     x_train.to_csv(FILE_NAME.replace(ext, "_X_train.csv"), index=False)
#     x_valid.to_csv(FILE_NAME.replace(ext, "_X_valid.csv"), index=False)
#     x_test.to_csv(FILE_NAME.replace(ext, "_X_test.csv"), index=False)

#     N_TRAIN, N_FEATURES = x_train.shape
#     file_name_x_out = FILE_NAME.replace(ext, "_X_train.csv")
#     subprocess.run(f"./main_csv2bin.out train {N_TRAIN} {N_FEATURES} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)

#     N_VALID, N_FEATURES = x_valid.shape
#     file_name_x_out = FILE_NAME.replace(ext, "_X_valid.csv")
#     subprocess.run(f"./main_csv2bin.out valid {N_VALID} {N_FEATURES} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)

#     N_TEST, N_FEATURES = x_test.shape
#     file_name_x_out = FILE_NAME.replace(ext, "_X_test.csv")
#     subprocess.run(f"./main_csv2bin.out test  {N_TEST}  {N_FEATURES} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)

#     y_train = y.iloc[train_idxs,:].astype(int)
#     y_valid = y.iloc[valid_idxs,:].astype(int)    
#     y_test  = y.iloc[test_idxs,:].astype(int)
#     y_train.to_csv(FILE_NAME.replace(ext, "_y_train.csv"), index=False)
#     y_valid.to_csv(FILE_NAME.replace(ext, "_y_valid.csv"), index=False)
#     y_test.to_csv(FILE_NAME.replace(ext, "_y_test.csv"), index=False)

#     N_TRAIN, N_FEATURES = y_train.shape
#     file_name_y_out = FILE_NAME.replace(ext, "_y_train.csv")
#     subprocess.run(f"./main_csv2bin.out train {N_TRAIN} {N_FEATURES} {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} i i", shell=True, check=True)

#     N_VALID, N_FEATURES = y_valid.shape
#     file_name_y_out = FILE_NAME.replace(ext, "_y_valid.csv")
#     subprocess.run(f"./main_csv2bin.out valid {N_VALID} {N_FEATURES} {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} i i", shell=True, check=True)

#     N_TEST, N_FEATURES = y_test.shape
#     file_name_y_out = FILE_NAME.replace(ext, "_y_test.csv")
#     subprocess.run(f"./main_csv2bin.out test  {N_TEST}  {N_FEATURES} {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} i i", shell=True, check=True)


# sys.exit()

# print("Download MNIST dataset")
# mnist_X, mnist_y = fetch_openml('mnist_784', version=1, data_home=".", return_X_y=True)

# print("Save csv")
# N_TRAIN = 50000
# N_VALID = 10000
# N_TEST  = 10000
# N_FEATURES = 784
# X = pd.DataFrame(mnist_X)
# y = pd.DataFrame(mnist_y)

# X.iloc[:N_TRAIN, :].to_csv("../sample_data/mnist_X_train.csv", index=False)
# y.iloc[:N_TRAIN, :].to_csv("../sample_data/mnist_y_train.csv", index=False)
# X.iloc[N_TRAIN:N_TRAIN+N_VALID, :].to_csv("../sample_data/mnist_X_valid.csv", index=False)
# y.iloc[N_TRAIN:N_TRAIN+N_VALID, :].to_csv("../sample_data/mnist_y_valid.csv", index=False)
# X.iloc[N_TRAIN+N_VALID:, :].to_csv("../sample_data/mnist_X_test.csv", index=False)
# y.iloc[N_TRAIN+N_VALID:, :].to_csv("../sample_data/mnist_y_test.csv", index=False)

# file_name_x_out = "../sample_data/mnist_X_train.csv"
# file_name_y_out = "../sample_data/mnist_y_train.csv"
# subprocess.run(f"./main_csv2bin.out train {N_TRAIN} {N_FEATURES} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)
# subprocess.run(f"./main_csv2bin.out train {N_TRAIN} {1}          {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} i i", shell=True, check=True)

# file_name_x_out = "../sample_data/mnist_X_valid.csv"
# file_name_y_out = "../sample_data/mnist_y_valid.csv"
# subprocess.run(f"./main_csv2bin.out train {N_VALID} {N_FEATURES} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)
# subprocess.run(f"./main_csv2bin.out train {N_VALID} {1}          {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} i i", shell=True, check=True)

# file_name_x_out = "../sample_data/mnist_X_test.csv"
# file_name_y_out = "../sample_data/mnist_y_test.csv"
# subprocess.run(f"./main_csv2bin.out train {N_TEST} {N_FEATURES} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)
# subprocess.run(f"./main_csv2bin.out train {N_TEST} {1}          {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} i i", shell=True, check=True)



n_samples_list = [100, 1000, 10000]
n_columns_list = [5, 10, 50, 100, 200, 400, 800]
# n_samples_list = [100000, 1000000]
# n_columns_list = [100]
# n_samples_list = [10000]
# n_columns_list = [100]
ratio_test  = 0.2
ratio_valid = 0.2

# dtype_in = "r"
# dtype_out = "r"
# for n_samples_train in n_samples_list:
#     n_samples_valid = int(n_samples_train*ratio_valid)
#     n_samples_test  = int(n_samples_train*ratio_test)
#     for n_columns in n_columns_list:
#         print("Creating Regression Datasets ***************************************************************************************")
#         print("     Tarin: #Samples={}, #Columns={}".format(n_samples_train, n_columns))
#         print("     Valid: #Samples={}, #Columns={}".format(n_samples_valid, n_columns))
#         print("     Test:  #Samples={}, #Columns={}".format(n_samples_test, n_columns))
#         X, y = make_regression(n_samples=n_samples_train+n_samples_valid+n_samples_test, n_features=n_columns)

#         list_data_seps = ["train", "valid", "test"]
#         list_n_samples = [n_samples_train, n_samples_valid, n_samples_test]
#         n_s = 0
#         n_e = 0
#         for d_sep, n_sam in zip(list_data_seps, list_n_samples):
#             n_e = n_s + n_sam

#             file_name_x_out = "../sample_data/make_regression_X_{0}_{1:0=10}x{2:0=5}.csv".format(d_sep, n_samples_train, n_columns)
#             file_name_y_out = "../sample_data/make_regression_y_{0}_{1:0=10}x{2:0=5}.csv".format(d_sep, n_samples_train, n_columns)
#             pd.DataFrame(X[n_s:n_e,:]).to_csv(file_name_x_out, index=False)
#             pd.DataFrame(y[n_s:n_e]).to_csv(file_name_y_out, index=False)

#             subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {n_columns} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {dtype_in} {dtype_out}", shell=True, check=True)
#             subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {1}         {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} {dtype_in} {dtype_out}", shell=True, check=True)
#             n_s = n_e
# sys.exit()


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


# n_classes = 3
# dtype_in = "r"
# dtype_out = "r"
# for n_samples_train in n_samples_list:
#     n_samples_valid = int(n_samples_train*ratio_valid)
#     n_samples_test  = int(n_samples_train*ratio_test)
#     for n_columns in n_columns_list:
#         print("Creating Classification Datasets ***************************************************************************************")
#         print("     Tarin: #Samples={}, #Columns={}".format(n_samples_train, n_columns))
#         print("     Valid: #Samples={}, #Columns={}".format(n_samples_valid, n_columns))
#         print("     Test:  #Samples={}, #Columns={}".format(n_samples_test, n_columns))
#         X, y = make_classification(n_samples=n_samples_train+n_samples_valid+n_samples_test, n_features=n_columns, n_classes=n_classes)

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


print("Create Clustering Datasets")
centers = [[1, 1], [-1, -1], [1, -1]]
X, labels_true = make_blobs(
        n_samples=750, centers=centers, cluster_std=0.4, random_state=0
    )
X = StandardScaler().fit_transform(X)

dtype_in, dtype_out = "r", "r"
file_name_x_out = "../sample_data/make_brobs_X_750x2.csv"
pd.DataFrame(X).to_csv(file_name_x_out, index=False)
subprocess.run(f"./main_csv2bin.out {'None'} {750} {2} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {dtype_in} {dtype_out}", shell=True, check=True)

db = DBSCAN(eps=0.25, min_samples=8).fit(X)
dtype_in, dtype_out = "i", "i"
file_name_x_out = "../sample_data/make_brobs_DBSCAN_750x2.csv"
pd.DataFrame([x+1 for x in db.labels_]).to_csv(file_name_x_out, index=False)
subprocess.run(f"./main_csv2bin.out {'None'} {750} {1} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {dtype_in} {dtype_out}", shell=True, check=True)


print("Create Nearest Neighbor Search Datasets")
n_samples_list = [100000 + 100] # Sample + Query
n_columns_list = [100]
for n_samples in n_samples_list:
    for n_columns in n_columns_list:
        print("     #Samples={}, #Columns={}".format(n_samples, n_columns))
        X, _ = make_regression(n_samples=n_samples, n_features=n_columns)
        
        file_name_x_out = "../sample_data/nnsearch_X_{0:0=10}x{1:0=5}.csv".format(len(X[:-100,:]), n_columns)
        pd.DataFrame(X[:-100,:]).to_csv(file_name_x_out, index=False)
        subprocess.run(f"./main_csv2bin.out {'None'} {100000} {100} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)

        file_name_x_out = "../sample_data/nnsearch_Q_{0:0=10}x{1:0=5}.csv".format(len(X[-100:,:]), n_columns)
        pd.DataFrame(X[-100:,:]).to_csv(file_name_x_out, index=False)
        subprocess.run(f"./main_csv2bin.out {'None'} {100} {100} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)

        tree = KDTree(X[:-100,:], leaf_size=32)
        dst, ind = tree.query(X[-100:,:], k=100000)

        file_name_x_out = "../sample_data/nnsearch_D_{0:0=10}x{1:0=5}.csv".format(100, 100000)
        pd.DataFrame(dst).to_csv(file_name_x_out, index=False)
        subprocess.run(f"./main_csv2bin.out {'None'} {100} {100000} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} r r", shell=True, check=True)

        ind = ind + 1
        file_name_x_out = "../sample_data/nnsearch_I_{0:0=10}x{1:0=5}.csv".format(100, 100000)
        pd.DataFrame(ind).to_csv(file_name_x_out, index=False)
        subprocess.run(f"./main_csv2bin.out {'None'} {100} {100000} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} i i", shell=True, check=True)
    



# print("Create Hard-Margin SVM Dataset (small)")
# np.random.seed(0)
# x0 = np.random.randn(50, 2)
# x1 = np.random.randn(50, 2) + np.array([5,5])
# y = np.array([1]*50 + [0]*50)

# x = np.r_[x0,x1]
# d_sep = "train"
# n_sam = 100
# n_columns = 2
# file_name_x_out = "../sample_data/svm_hard_x_100x2.csv"
# file_name_y_out = "../sample_data/svm_hard_y_100x1.csv"
# pd.DataFrame(x).to_csv(file_name_x_out, index=False)
# pd.DataFrame(y).to_csv(file_name_y_out, index=False)

# subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {n_columns} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {'r'} {'r'}", shell=True, check=True)
# subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {1}         {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} {'i'} {'i'}", shell=True, check=True)


# print("Create Soft-Margin SVM Dataset (small)")
# np.random.seed(0)
# x0 = np.random.randn(50, 2)
# x1 = np.random.randn(50, 2) + np.array([2.5,3])
# y = np.array([1]*50 + [0]*50)

# x = np.r_[x0,x1]
# d_sep = "train"
# n_sam = 100
# n_columns = 2
# file_name_x_out = "../sample_data/svm_soft_x_100x2.csv"
# file_name_y_out = "../sample_data/svm_soft_y_100x1.csv"
# pd.DataFrame(x).to_csv(file_name_x_out, index=False)
# pd.DataFrame(y).to_csv(file_name_y_out, index=False)

# subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {n_columns} {file_name_x_out} {file_name_x_out.replace('.csv', '.bin')} {'r'} {'r'}", shell=True, check=True)
# subprocess.run(f"./main_csv2bin.out {d_sep} {n_sam} {1}         {file_name_y_out} {file_name_y_out.replace('.csv', '.bin')} {'i'} {'i'}", shell=True, check=True)


