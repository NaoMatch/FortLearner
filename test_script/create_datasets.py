import pandas as pd
from sklearn.datasets import make_regression, make_blobs
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import DBSCAN
from sklearn.neighbors import KDTree

n_samples_list = [1000, 100000]
n_columns_list = [5, 100]
# n_samples_list = [100, 1000, 10000, 100000, 1000000]
# n_columns_list = [10, 50, 100, 200, 400]

print("Creating Regression Datasets...")
for n_samples in n_samples_list:
    for n_columns in n_columns_list:
        print("     #Samples={}, #Columns={}".format(n_samples, n_columns))
        X, y = make_regression(n_samples=n_samples, n_features=n_columns)
        pd.DataFrame(X).to_csv("../sample_data/make_regression_X_{0:0=10}x{1:0=5}.csv".format(n_samples, n_columns), index=False)
        pd.DataFrame(y).to_csv("../sample_data/make_regression_y_{0:0=10}x{1:0=5}.csv".format(n_samples, n_columns), index=False)


print("Create Clustering Datasets")
centers = [[1, 1], [-1, -1], [1, -1]]
X, labels_true = make_blobs(
    n_samples=750, centers=centers, cluster_std=0.4, random_state=0
)
X = StandardScaler().fit_transform(X)
pd.DataFrame(X).to_csv("../sample_data/make_brobs_X_750x2.csv", index=False)
pd.DataFrame(labels_true).to_csv("../sample_data/make_brobs_Y_750x2.csv", index=False)
db = DBSCAN(eps=0.25, min_samples=8).fit(X)
pd.DataFrame([x+1 for x in db.labels_]).to_csv("../sample_data/make_brobs_DBSCAN_750x2.csv", index=False)


print("Create Nearest Neighbor Search Datasets")
n_samples_list = [100000 + 100] # Sample + Query
n_columns_list = [100]
for n_samples in n_samples_list:
    for n_columns in n_columns_list:
        print("     #Samples={}, #Columns={}".format(n_samples, n_columns))
        X, _ = make_regression(n_samples=n_samples, n_features=n_columns)
        
        pd.DataFrame(X[:-100,:]).to_csv("../sample_data/nnsearch_X_{0:0=10}x{1:0=5}.csv".format(len(X[:-100,:]), n_columns), index=False)
        pd.DataFrame(X[-100:,:]).to_csv("../sample_data/nnsearch_Q_{0:0=10}x{1:0=5}.csv".format(len(X[-100:,:]), n_columns), index=False)

        tree = KDTree(X[:-100,:], leaf_size=32)
        dst, ind = tree.query(X[-100:,:], k=100000)

        ind = ind + 1
        pd.DataFrame(dst).to_csv("../sample_data/nnsearch_D_{0:0=10}x{1:0=5}.csv".format(100, 100000), index=False)
        pd.DataFrame(ind).to_csv("../sample_data/nnsearch_I_{0:0=10}x{1:0=5}.csv".format(100, 100000), index=False)
