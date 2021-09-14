import pandas as pd
from sklearn.datasets import make_regression

n_samples_list = [100000]
n_columns_list = [100]
# n_samples_list = [100, 1000, 10000, 100000, 1000000]
# n_columns_list = [10, 50, 100, 200, 400]

print("Creating Datasets...")
for n_samples in n_samples_list:
    for n_columns in n_columns_list:
        print("     #Samples={}, #Columns={}".format(n_samples, n_columns))
        X, y = make_regression(n_samples=n_samples, n_features=n_columns)
        pd.DataFrame(X).to_csv("../sample_data/make_regression_X_{0:0=10}x{1:0=5}.csv".format(n_samples, n_columns), index=False)
        pd.DataFrame(y).to_csv("../sample_data/make_regression_y_{0:0=10}x{1:0=5}.csv".format(n_samples, n_columns), index=False)
