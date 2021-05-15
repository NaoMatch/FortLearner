# %%
import sys
import numpy as np
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score, roc_auc_score
from sklearn.datasets import load_iris, load_digits
from sklearn.datasets import load_iris
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from scipy.linalg.blas import sgemv
from criterion import gini, entropy
from sklearn.model_selection import train_test_split
from scipy.optimize import minimize
from Constant import Const

def sigmoid(x):
    return 1.0 / (1.0 + np.exp(-x))

def condition(x, thetas, intercept):
    return sgemv(1.0, x, thetas) + intercept

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

class BaseDecisionTree:
    def __init__(self, max_depth=6, criterion="gini"):
        self.max_depth = max_depth
        self.criterion = criterion
        self.root_node = None
        self.selected_node = None

    def _extract_most_left_unsplit_node(self, node):
        if self.selected_node is not None:
            return
        if node.node_l is not None:
            self._extract_most_left_unsplit_node(node.node_l)
            self._extract_most_left_unsplit_node(node.node_r)
        else:
            if (not node.is_terminal) & (not node.is_trained):
                self.selected_node = node

    def _extract_train_results(self, node):
        if self.is_axis_parallel:
            self.feature_ids_.append(node.feature_id)
            self.split_values_.append(node.split_value)
            self.leaf_labels_.append(node.label)
            self.label_probas_.append(node.label_proba)
            if node.node_l is not None:
                self._extract_train_results(node.node_l)
                self._extract_train_results(node.node_r)
        else:
            NotImplementedError("Not yet")
    


class WeightedObliqueDecisionTree(BaseDecisionTree):
    def __init__(self, max_depth=6):
        super().__init__(max_depth=max_depth, criterion=None)
        self.is_axis_parallel = False

    def fit(self, x, y):
        self.n_samples, self.n_columns = x.shape
        indices = np.array(range(self.n_samples))
        self.n_classes = len(np.unique(y))
        root_node = Node_oblique(indices, self.n_samples, self.n_columns, self.n_classes, depth=0, 
            max_depth=self.max_depth, criterion=self.criterion)
        root_node.is_root = True
        root_node.label = None
        root_node.label_proba = None
        
        self.root_node = root_node
        iter0 = 0
        while True:
            print(iter0)
            iter0 += 1
            self.selected_node = None
            self._extract_most_left_unsplit_node(self.root_node)
            if self.selected_node is None:
                break
            print(self.selected_node.n_samples)
            self.selected_node.is_trained = True
            self.selected_node.search_best_split(x,y)
            
        self.feature_ids_ = []
        self.split_values_ = []
        self.leaf_labels_ = []
        self.label_probas_ = []
        self._extract_train_results(self.root_node)

    def predict(self,x):
        return self.predict_proba(x).argmax(axis=1)


    def predict_proba(self, x):
        self.idx = -1
        indices = range(len(x))
        print(indices)
        probas = np.zeros((len(indices), self.n_classes))
        self._rec_predict_proba(x, indices, probas)
        return probas
        
    
    def _rec_predict_proba(self, x, indices, probas):
        self.idx += 1

        if (len(indices)==0) & (self.split_values_[self.idx]==sys.maxsize):
            # Empty Terminal Node
            return
        elif len(indices)==0:
            # Empty Internal Node
            indices_l = []
            indices_r = []
            self._rec_predict_proba(x, indices_l, probas)
            self._rec_predict_proba(x, indices_r, probas)
        else:
            # Not Empty Internal or Terminal Node
            if self.split_values_[self.idx]==sys.maxsize:
                for n in range(len(indices)):
                    idx = indices[n]
                    probas[idx] = self.label_probas_[self.idx]
                return
            
            feature    = x[indices, self.feature_ids_[self.idx]]
            conditions = feature <= self.split_values_[self.idx]
            n_samples_l = sum(conditions)
            n_samples_r = len(indices)-n_samples_l
            indices_l = np.zeros(n_samples_l, dtype=np.int32)
            indices_r = np.zeros(n_samples_r, dtype=np.int32)
            
            idx_l = 0
            idx_r = 0
            for n in range(len(indices)):
                idx = indices[n]
                if conditions[n]:
                    indices_l[idx_l] = idx
                    idx_l += 1
                else:
                    indices_r[idx_r] = idx
                    idx_r += 1
            self._rec_predict_proba(x, indices_l, probas)
            self._rec_predict_proba(x, indices_r, probas)


class MyDecisionTreeClassifier(BaseDecisionTree):
    def __init__(self, max_depth=6, criterion="gini"):
        super().__init__(max_depth=max_depth, criterion="gini")
        self.is_axis_parallel = True
        
                
    def fit(self, x, y):
        self.n_samples, self.n_columns = x.shape
        indices = np.array(range(self.n_samples))
        self.n_classes = len(np.unique(y))
        root_node = Node(indices, self.n_samples, self.n_columns, self.n_classes, depth=0, 
            max_depth=self.max_depth, criterion=self.criterion)
        root_node.is_root = True
        root_node.label = None
        root_node.label_proba = None
        
        self.root_node = root_node
        iter0 = 0
        while True:
            self.selected_node = None
            self._extract_most_left_unsplit_node(self.root_node)
            if self.selected_node is None:
                break
            self.selected_node.is_trained = True
            self.selected_node.search_best_split(x,y)
            
        self.feature_ids_ = []
        self.split_values_ = []
        self.leaf_labels_ = []
        self.label_probas_ = []
        self._extract_train_results(self.root_node)


    def predict(self,x):
        return self.predict_proba(x).argmax(axis=1)


    def predict_proba(self, x):
        self.idx = -1
        indices = range(len(x))
        print(indices)
        probas = np.zeros((len(indices), self.n_classes))
        self._rec_predict_proba(x, indices, probas)
        return probas
        
    
    def _rec_predict_proba(self, x, indices, probas):
        self.idx += 1

        if (len(indices)==0) & (self.split_values_[self.idx]==sys.maxsize):
            # Empty Terminal Node
            return
        elif len(indices)==0:
            # Empty Internal Node
            indices_l = []
            indices_r = []
            self._rec_predict_proba(x, indices_l, probas)
            self._rec_predict_proba(x, indices_r, probas)
        else:
            # Not Empty Internal or Terminal Node
            if self.split_values_[self.idx]==sys.maxsize:
                for n in range(len(indices)):
                    idx = indices[n]
                    probas[idx] = self.label_probas_[self.idx]
                return
            
            feature    = x[indices, self.feature_ids_[self.idx]]
            conditions = feature <= self.split_values_[self.idx]
            n_samples_l = sum(conditions)
            n_samples_r = len(indices)-n_samples_l
            indices_l = np.zeros(n_samples_l, dtype=np.int32)
            indices_r = np.zeros(n_samples_r, dtype=np.int32)
            
            idx_l = 0
            idx_r = 0
            for n in range(len(indices)):
                idx = indices[n]
                if conditions[n]:
                    indices_l[idx_l] = idx
                    idx_l += 1
                else:
                    indices_r[idx_r] = idx
                    idx_r += 1
            self._rec_predict_proba(x, indices_l, probas)
            self._rec_predict_proba(x, indices_r, probas)
        

class Node_oblique:
    def __init__(self, indices, n_samples, n_columns, n_classes, depth, max_depth, criterion):
        self.is_root     = False
        self.side        = None
        self.depth       = depth
        self.max_depth   = max_depth
        self.criterion   = criterion
        self.indices     = indices
        self.n_samples   = n_samples
        self.n_columns   = n_columns
        self.n_classes   = n_classes
        self.is_terminal = False
        self.is_trained  = False
        self.split_value = np.ones(n_columns)*sys.maxsize
        self.intercept   = sys.maxsize
        self.eval_count  = 0
        self.node_l      = None
        self.node_r      = None
        
        if self.depth == self.max_depth:
            self.is_trained = True
            self.is_terminal = True

    def search_best_split(self,x,y):
        self.x = x[self.indices,:]
        self.y = y[self.indices]

        thetas_ = np.random.uniform(-2,2, self.n_columns).astype(dtype=np.float32)
        intercept_ = np.random.uniform(-2,2)

        wodt_obj = WODTObjective(self.x, self.y)
        th = np.array(list(thetas_)+list([intercept_]))
        print(th)
        result = minimize(
            fun = wodt_obj.loss,
            x0=th, 
            jac=wodt_obj.grad, 
            method="BFGS"
        )
        self.split_value = th[:-1]
        self.intercept  = th[-1]
        print(th)
        self.adopt_twins()

    def adopt_twins(self):
        if self.eval_count==0:
            self.is_terminal = True
            return

        subset_x = self.x[self.indices, :]
        conditions = sgemv(1.0, subset_x, self.split_value) + self.intercept <= 0
        n_samples_l = sum(conditions)
        n_samples_r = self.n_samples - sum(conditions)
        idx_l = 0
        idx_r = 0
        indices_l = np.zeros(n_samples_l, dtype=np.int32)
        indices_r = np.zeros(n_samples_r, dtype=np.int32)
        class_sum_l = np.zeros(self.n_classes, dtype=np.float64)
        class_sum_r = np.zeros(self.n_classes, dtype=np.float64)
        print("Oblique: ", n_samples_l, n_samples_r)
        for n in range(self.n_samples):
            label = self.y[n]
            if conditions[n]:
                indices_l[idx_l] = self.indices[n]
                class_sum_l[label] += 1
                idx_l += 1
            else:
                indices_r[idx_r] = self.indices[n]
                class_sum_r[label] += 1
                idx_r += 1        
        self.node_l = Node_oblique(indices_l, n_samples_l, self.n_columns, self.n_classes, depth=self.depth+1, 
            max_depth=self.max_depth, criterion=self.criterion)
        self.node_r = Node_oblique(indices_r, n_samples_r, self.n_columns, self.n_classes, depth=self.depth+1, 
            max_depth=self.max_depth, criterion=self.criterion)
        self.node_l.label = np.argmax(class_sum_l)
        self.node_r.label = np.argmax(class_sum_r)
        self.node_l.label_proba = class_sum_l/np.sum(class_sum_l)
        self.node_r.label_proba = class_sum_r/np.sum(class_sum_r)


class Node:
    def __init__(self, indices, n_samples, n_columns, n_classes, depth, max_depth, criterion):
        self.is_root     = False
        self.side        = None
        self.depth       = depth
        self.max_depth   = max_depth
        self.indices     = indices
        self.n_samples   = n_samples
        self.n_columns   = n_columns
        self.n_classes   = n_classes
        self.is_terminal = False
        self.is_trained  = False
        self.split_value = sys.maxsize
        self.feature_id  = -2
        self.eval_count  = 0
        self.node_l      = None
        self.node_r      = None
        self.criterion   = criterion
        
        if self.depth == self.max_depth:
            self.is_terminal = True
        
    def adopt_twins(self):
        if self.eval_count==0:
            self.is_terminal = True
            return
        
        # Common
        feature = self.x[:, self.feature_id]
        conditions = feature <= self.split_value
        n_samples_l = sum(conditions)
        n_samples_r = self.n_samples - sum(conditions)
        idx_l = 0
        idx_r = 0
        indices_l = np.zeros(n_samples_l, dtype=np.int32)
        indices_r = np.zeros(n_samples_r, dtype=np.int32)
        class_sum_l = np.zeros(self.n_classes, dtype=np.float64)
        class_sum_r = np.zeros(self.n_classes, dtype=np.float64)
        for n in range(self.n_samples):
            label = self.y[n]
            if conditions[n]:
                indices_l[idx_l] = self.indices[n]
                class_sum_l[label] += 1
                idx_l += 1
            else:
                indices_r[idx_r] = self.indices[n]
                class_sum_r[label] += 1
                idx_r += 1        
        self.node_l = Node(indices_l, n_samples_l, self.n_columns, self.n_classes, depth=self.depth+1, 
            max_depth=self.max_depth, criterion=self.criterion)
        self.node_r = Node(indices_r, n_samples_r, self.n_columns, self.n_classes, depth=self.depth+1, 
            max_depth=self.max_depth, criterion=self.criterion)
        self.node_l.label = np.argmax(class_sum_l)
        self.node_r.label = np.argmax(class_sum_r)
        self.node_l.label_proba = class_sum_l/np.sum(class_sum_l)
        self.node_r.label_proba = class_sum_r/np.sum(class_sum_r)
        
    
    def search_best_split(self,x,y):
        self.x = x[self.indices,:]
        self.y = y[self.indices]
        class_sum_p = np.zeros(self.n_classes, dtype=np.float64)
        for n in range(self.n_samples):
            label = self.y[n]
            class_sum_p[label] += 1
            
        gini_p = gini(class_sum_p)
        if gini_p==0:
            self.is_terminal = True
            return 
        impurity_gain_best = -sys.maxsize
            
        for col_idx in range(self.n_columns):
            feature = self.x[:,col_idx]
            sorted_indices = np.argsort(feature)
            feature_sorted = feature[sorted_indices]
            targte_sorted  = self.y[sorted_indices]
            class_sum_l = np.zeros(self.n_classes, dtype=np.float64)
            for n in range(self.n_samples-1):
                label = targte_sorted[n]
                class_sum_l[label] += 1
                if feature_sorted[n] != feature_sorted[n+1]:
                    n_samples_l = np.sum(class_sum_l)
                    n_samples_r = self.n_samples-n_samples_l
                    class_sum_r = class_sum_p - class_sum_l
                    if self.criterion=="gini":
                        gini_l = gini(class_sum_l)
                        gini_r = gini(class_sum_r)
                    elif self.criterion=="entropy":
                        gini_l = entropy(class_sum_l)
                        gini_r = entropy(class_sum_r)
                    
                    impurity_gain = gini_p - gini_l * n_samples_l/self.n_samples - gini_r * n_samples_r/self.n_samples
                    if impurity_gain_best < impurity_gain:
                        self.eval_count += 1
                        impurity_gain_best = impurity_gain
                        self.feature_id = col_idx
                        self.split_value = (feature_sorted[n] + feature_sorted[n+1])*.5
                        self.impurity_gain = impurity_gain_best
        
        self.is_trained = True
        self.adopt_twins()
        del self.x
        del self.y
        del self.indices


data = load_iris()
data = load_digits()
x, y = data.data, data.target

x_train, x_test, y_train, y_test = train_test_split(x, y, train_size=.8, random_state=31)

scaler = StandardScaler()
x_train = scaler.fit_transform(x_train)
x_test = scaler.transform(x_test)

max_depth = 2
criterion = "gini"

print("My Decision Tree Classifier")
dt_clf = MyDecisionTreeClassifier(max_depth=max_depth)
dt_clf.fit(x_train, y_train)
acc_my_train = accuracy_score(y_train, dt_clf.predict(x_train))
acc_my_test  = accuracy_score(y_test, dt_clf.predict(x_test))

print("My Weigthed Oblique Decision Tree")
wodt = WeightedObliqueDecisionTree(max_depth=max_depth)
wodt.fit(x_train, y_train)

print("Sklearn Decision Tree Classifier")
dt = DecisionTreeClassifier(max_depth=max_depth, criterion=criterion, random_state=43)
dt.fit(x_train, y_train) 
acc_sk_train = accuracy_score(y_train, dt.predict(x_train))
acc_sk_test  = accuracy_score(y_test, dt.predict(x_test))

print("Accuracy My,      Train vs Test: {0:.5f} vs {1:.5f}".format(acc_my_train, acc_my_test))
print("Accuracy SKlearn, Train vs Test: {0:.5f} vs {1:.5f}".format(acc_sk_train, acc_sk_test))
