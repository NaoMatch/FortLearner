# %%
import numpy as np
import pandas as pd
from sklearn.datasets import load_iris, load_digits, load_breast_cancer, load_wine
from scipy.optimize import minimize
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from scipy.linalg.blas import sgemv
from sklearn.metrics import accuracy_score
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from Constant import Const
import warnings
warnings.simplefilter('ignore')

def sigmoid(x):
    return 1.0 / (1.0 + np.exp(-x))

def condition(x, thetas, intercept):
    return sgemv(1.0, x, thetas) + intercept

class WODTObjectiveNew:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.n_samples, self.n_columns = self.x.shape
        self.n_classes = len(np.unique(y))
    
    def _prep(self, thetas):
        thetas_ = thetas[:-1]
        intercept_ = thetas[-1]
        this.plane = condition(this.x, thetas_, intercept_)



class WODTObjective:
    def __init__(this, x, y):
        this.x = x
        this.y = y
        this.n_samples, this.n_columns = x.shape
        this.n_classes = len(set(y))

    def prep(this, thetas):
        thetas_ = thetas[:-1]
        intercept_ = thetas[-1]
        this.plane = condition(this.x, thetas_, intercept_)

        this.weights_l = np.zeros(this.n_samples)
        this.weights_r = np.zeros(this.n_samples)
        this.class_l   = this.y
        this.class_r   = this.y

        weights = sigmoid(this.plane)
        this.weights_l = 1-weights
        this.weights_r = weights

        this.weights_per_class_l = np.zeros(this.n_classes)
        this.weights_per_class_r = np.zeros(this.n_classes)
        this.samples_per_class_l = np.zeros(this.n_classes)
        this.samples_per_class_r = np.zeros(this.n_classes)
        
        for n in range(this.n_samples):
            w = this.weights_l[n]
            c = int(this.class_l[n])
            this.weights_per_class_l[c] += w
            this.samples_per_class_l[c] += 1

        for n in range(this.n_samples):
            w = this.weights_r[n]
            c = int(this.class_r[n])
            this.weights_per_class_r[c] += w
            this.samples_per_class_r[c] += 1
            
        this.W_L = this.weights_per_class_l.sum()
        this.W_R = this.weights_per_class_r.sum()
    
    def loss(this, theta):
        eps = Const().epsilon
        this.prep(theta)
        obj = this.W_L * np.log2(this.W_L+eps) + this.W_R * np.log2(this.W_R+eps)
        for c in range(this.n_classes):
            w_l = this.weights_per_class_l[c]
            w_r = this.weights_per_class_r[c]
            obj -= w_l * np.log2(w_l + +eps) + w_r * np.log2(w_r + +eps)
        return obj/this.n_samples
    
    def grad(this, theta):
        this.prep(theta)
        eps = Const().epsilon
        beta_i = np.zeros(this.n_columns+1)
        obj_grad = np.zeros(this.n_columns+1)
        for n in range(this.n_samples):
            p_psq = sigmoid(this.plane[n])
            p_psq = p_psq * (1-p_psq)
            beta_i[:-1] = this.x[n,:] * p_psq
            beta_i[-1]  = p_psq
            
            ratio = beta_i * np.log2(this.weights_l[n]/this.weights_r[n]+eps)
            obj_grad += ratio
        obj_grad *= np.log2(this.W_R/(this.W_L+eps)+eps)
        return obj_grad/this.n_samples

data = load_iris()
data = load_digits()
data = load_breast_cancer()
data = load_wine()
                
x, y = data.data, data.target
# x = pd.read_csv("../../uci_data/98_Higgs/HIGGS_train_x.csv", nrows=1000000, header=None)
# y = pd.read_csv("../../uci_data/98_Higgs/HIGGS_train_y.csv", nrows=1000000, header=None, dtype=int)
# x = np.array(x)
# y = np.array(y[0])
# train_test_split(x,y,random)

ss = MinMaxScaler(feature_range=(-2,2))
# ss = StandardScaler()
# ss = MinMaxScaler()
x = ss.fit_transform(x)

print(x.shape)

n_cols_sqrt = int(np.sqrt(x.shape[1]))
for i in range(1000):
    col_indices = list(range(x.shape[1]))

    dt = DecisionTreeClassifier(max_depth=1)
    dt.fit(x,y)
    pred = dt.predict(x)
    
    col_indices = np.random.permutation(col_indices)
    # x_subset = x[:, col_indices[:n_cols_sqrt]]
    x_subset = x
    th = np.random.uniform(-.010,.010,x_subset.shape[1]+1)
    # th = np.zeros(x.shape[1]+1)
    # th[dt.tree_.feature[0]] = 1
    # th[-1] = -dt.tree_.threshold[0]
    wodt_obj = WODTObjective(x_subset,y)

    result = minimize(
        fun = wodt_obj.loss,
        x0=th,
        jac=wodt_obj.grad, 
        method="L-BFGS-B"
    )

    plane = sgemv(1.0, x_subset, result.x[:-1]) + result.x[-1]
    df = pd.DataFrame({"plane": plane<0, "label": y})
    df["count"] = 1
    df.head()

    xxx = df.groupby(["plane", "label"]).agg({"count":"sum"}).unstack().idxmax(axis=1)
    df.groupby(["plane", "label"]).agg({"count":"sum"}).unstack()


    # 1
    print(xxx)
    label_F = xxx[0][1]
    label_T = xxx[1][1]
    df["Pred"] = [label_T if x else label_F for x in df["plane"]]
    acc_my = accuracy_score(y, df["Pred"])

    if accuracy_score(y, pred) < acc_my:
        asta = "*"
    else:
        asta = " "
    print("SKLEARN vs WODT: {0:.5f} vs {1:.5f} :: {2}".format(accuracy_score(y, pred), acc_my, asta))

# %%
