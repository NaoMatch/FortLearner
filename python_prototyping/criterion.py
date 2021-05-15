import numpy as np
from Constant import Const

def gini(total_counts):
    proba = total_counts/np.sum(total_counts)
    return 1-np.sum(proba**2)

def entropy(total_counts):
    const = Const()
    proba = total_counts/np.sum(total_counts)
    return - np.sum(proba * np.log2(proba+const.epsilon))

def mean_squared_error(mean_left, mean_right, n_samples_total, n_samples_l, n_samples_r):
    return n_samples_l*n_samples_r/n_samples_total * (mean_left-mean_right)**2.0


    