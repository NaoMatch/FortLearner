# FortLearner
Machine learning scripts for Fortranner.  

# Update
2021/08/20: linear_regression, ridge_regression, lasso_regression, add model dump & load

## requirement
gfortran >= 7.4.0  
gcc >= 7.4.0  
X86_64

### option
doxygen  
make  
graphviz

# Benchmark
## extra_tree_regressor(second)
max_leaf_nodes=100, others are default.
| Data: shape(#Row, #Col)         | Scikit-learn: ExtraTreeRegressor.fit() | FortLearner: extra_tree_regressor%fit() | FortLearner: extra_tree_regressor%fit_faster() | 
| ------------------------------- | -------------------------------- | --------------------------------- | -------------------------------------------- | 
| YearPredictionMSD: (412206, 90) |3.12                              |2.1                                |0.800                                         | 
| sklearn.datasets.make_regressione: (100, 10)      |0.00179         |0.0036                             |0.00075                                       | 
| sklearn.datasets.make_regressione: (100, 50)      |0.00271         |0.01733                            |0.00094                                       | 
| sklearn.datasets.make_regressione: (100, 100)     |0.00359         |0.03132                            |0.0009                                        | 
| sklearn.datasets.make_regressione: (100, 200)     |0.00545         |0.05712                            |0.00127                                       | 
| sklearn.datasets.make_regressione: (100, 400)     |0.00921         |0.11181                            |0.00206                                       | 
| sklearn.datasets.make_regressione: (1000, 10)     |0.00269         |0.00909                            |0.00189                                       | 
| sklearn.datasets.make_regressione: (1000, 50)     |0.0063          |0.04195                            |0.00208                                       | 
| sklearn.datasets.make_regressione: (1000, 100)    |0.0108          |0.08839                            |0.00283                                       | 
| sklearn.datasets.make_regressione: (1000, 200)    |0.0196          |0.16653                            |0.0042                                        | 
| sklearn.datasets.make_regressione: (1000, 400)    |0.0396          |0.33765                            |0.00667                                       | 
| sklearn.datasets.make_regressione: (10000, 10)    |0.0084          |0.0148                             |0.0043                                        | 
| sklearn.datasets.make_regressione: (10000, 50)    |0.0408          |0.0763                             |0.0078                                        | 
| sklearn.datasets.make_regressione: (10000, 100)   |0.0813          |0.1446                             |0.0142                                        | 
| sklearn.datasets.make_regressione: (10000, 200)   |0.188           |0.3053                             |0.0291                                        | 
| sklearn.datasets.make_regressione: (10000, 400)   |0.403           |0.6191                             |0.0498                                        | 
| sklearn.datasets.make_regressione: (100000, 10)   |0.0942          |0.0784                             |0.0456                                        | 
| sklearn.datasets.make_regressione: (100000, 50)   |0.502           |0.3368                             |0.1312                                        | 
| sklearn.datasets.make_regressione: (100000, 100)  |1.08            |0.7288                             |0.2162                                        | 
| sklearn.datasets.make_regressione: (100000, 200)  |2.05            |1.304                              |0.3646                                        | 
| sklearn.datasets.make_regressione: (100000, 400)  |3.72            |2.6974                             |0.559                                         | 
| sklearn.datasets.make_regressione: (1000000, 10)  |1.36            |0.862                              |0.658                                         | 
| sklearn.datasets.make_regressione: (1000000, 50)  |5.91            |3.1822                             |1.38                                          | 
| sklearn.datasets.make_regressione: (1000000, 100) |11.6            |5.9278                             |2.204                                         | 
| sklearn.datasets.make_regressione: (1000000, 200) |28              |17.0186                            |4.44                                          | 
| sklearn.datasets.make_regressione: (1000000, 400) |55.4            |26.56                              |6.163                                         | 

# Implemented
* Linear Regression:
  * linear_regression
  * lasso_regression
  * ridge_regression
* Logistic Regression:
  * logistic_regression
* Stochastic Gradient Descent:
  * sgd_regressor
* Decision Tree:
  * decision_tree_regressor
  * extra_tree_regressor
  * clouds_regressor
  * lawu_regressor
* Ensemble Trees:
  * random_forest_regressor
  * extra_trees_regressor
  * deep_forest_regressor(?)
* Boosting Trees:
  * gradient_boosting_tree_regressor
  * gradient_boosting_extra_tree_regressor
  * gradient_boosting_clouds_regressor
  * gradient_boosting_lawu_regressor
* Dimensionality Reduction:
  * pca
 
# WIP
  
# ToDo
* Decision Tree
  * Axis-Parallel
    * sliq
    * residual likelihood tree
  * Oblique
    * simulated annealing decision tree
    * rotation tree
    * random rotation tree
    * soft decision tree
    * CART-LC
    * OC1
    * householder cart
    * weighted oblique decision tree
    * residual likelihood forest
    * slow-growing tree
* Ensemble Trees
* Gradient Boosting Decision Tree
  * Axis-Parallel
    * Xgboost
    * LightGBM
    * CatBoost
    * SCORE: Selective Cascade of Residual ExtraTrees
    * NGBoost
* Multi-Layer Perceptron
