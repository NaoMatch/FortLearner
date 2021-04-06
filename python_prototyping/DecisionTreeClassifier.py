#%%
import sys
import numpy as np
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score, roc_auc_score
from sklearn.datasets import load_iris, load_digits


def gini(total_counts):
    proba = total_counts/np.sum(total_counts)
    return 1-np.sum(proba**2)

class MyDecisionTreeClassifier:
    def __init__(self, max_depth=6):
        self.max_depth = max_depth
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
        self.feature_ids_.append(node.feature_id)
        self.split_values_.append(node.split_value)
        self.leaf_labels_.append(node.label)
        self.label_probas_.append(node.label_proba)
        if node.node_l is not None:
            self._extract_train_results(node.node_l)
            self._extract_train_results(node.node_r)
    
    def fit(self, x, y):
        self.n_samples, self.n_columns = x.shape
        print("self.n_samples, self.n_columns", self.n_samples, self.n_columns)
        indices = np.array(range(self.n_samples))
        self.n_classes = len(np.unique(y))
        root_node = Node(indices, self.n_samples, self.n_columns, self.n_classes, depth=0, max_depth=self.max_depth)
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
        
    def predict_proba(self, x):
        self.idx = -1
        indices = range(len(x))
        probas = np.zeros((len(indices), self.n_classes))
        self._rec_predict_proba(x, indices, probas)
        return probas
        
    
    def _rec_predict_proba(self, x, indices, probas):
        self.idx += 1
        if len(indices)==0:
            indices_l = []
            indices_r = []
            self._rec_predict_proba(x, indices_l, probas)
            self._rec_predict_proba(x, indices_r, probas)            
            return
        
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
            
        


class Node:
    def __init__(self, indices, n_samples, n_columns, n_classes, depth, max_depth):
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
        self.node_l = Node(indices_l, n_samples_l, self.n_columns, self.n_classes, depth=self.depth+1, max_depth=self.max_depth)
        self.node_r = Node(indices_r, n_samples_r, self.n_columns, self.n_classes, depth=self.depth+1, max_depth=self.max_depth)
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
                    gini_l = gini(class_sum_l)
                    gini_r = gini(class_sum_r)
                    
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
                    


#%%
data = load_iris()
data = load_digits()
x, y = data.data, data.target    

print("My Decision Tree Classifier")
max_depth = 5
dt_clf = MyDecisionTreeClassifier(max_depth=max_depth)
dt_clf.fit(x,y)
acc_my = accuracy_score(y, dt_clf.predict_proba(x).argmax(axis=1))

#%%
print("Sklearn Decision Tree Classifier")
dt = DecisionTreeClassifier(max_depth=max_depth, criterion="gini", random_state=43)
dt.fit(x,y) 
acc_sk = accuracy_score(y, dt.predict(x))

# %%
print("Accuracy, My vs SK: {0:.5f} vs {1:.5f}".format(acc_my, acc_sk))
# %%
