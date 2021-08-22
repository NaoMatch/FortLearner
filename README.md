# FortLearner
Machine learning scripts for Fortranner.  

# Update
2021/08/22: set '#include <stdint.h>' to inc_covariance_value_of_vectors.C  
2021/08/22: add benchmark for decision_tree_regressor.  
2021/08/22: add model dump & load to decision_tree_regressor, extra_tree_regressor, clouds_regressor, lawu_regressor  
2021/08/20: add model dump & load to linear_regression, ridge_regression, lasso_regression

# requirement
gfortran >= 7.4.0  
gcc >= 7.4.0  

### option
x86_64  
doxygen  
make  
graphviz

# Benchmark
## XXXX_tree_regressor(second)
max_leaf_nodes=100, others are default.  
SK = scikit-learn  
FL = FortLearner  
ET = extra_tree_regressor  
DT = decision_tree_regressor
| Data: shape(#Row, #Col)         | SK: ET | FL: ET | FL: ET(fit_faster) | SK: DT | FL: DT |
| ------------------------------- | ------------------------------- | -------------------------------- | --------------------------------- | -------------------------------------------- | ------------------------------- |
| YearPredictionMSD: (412206, 90) |3.12                              |2.1     |0.800     | 34.3 |25 |
| sklearn.datasets.make_regression: (100, 10)      |0.00179         |0.0036   |0.00075   |0.00176  | 0.000108 |
| sklearn.datasets.make_regression: (100, 50)      |0.00271         |0.01733  |0.00094   |0.00302  | 0.000489 |
| sklearn.datasets.make_regression: (100, 100)     |0.00359         |0.03132  |0.0009    |0.00493  | 0.001036 |
| sklearn.datasets.make_regression: (100, 200)     |0.00545         |0.05712  |0.00127   |0.00176  | 0.001985 |
| sklearn.datasets.make_regression: (100, 400)     |0.00921         |0.11181  |0.00206   |0.00827  | 0.003749 |
| sklearn.datasets.make_regression: (1000, 10)     |0.00269         |0.00909  |0.00189   |0.0146  | 0.0032 |
| sklearn.datasets.make_regression: (1000, 50)     |0.0063          |0.04195  |0.00208   |0.00644  | 0.015 |
| sklearn.datasets.make_regression: (1000, 100)    |0.0108          |0.08839  |0.00283   |0.0485  | 0.032099999 |
| sklearn.datasets.make_regression: (1000, 200)    |0.0196          |0.16653  |0.0042    |0.0897  | 0.05973 |
| sklearn.datasets.make_regression: (1000, 400)    |0.0396          |0.33765  |0.00667   |0.198  | 0.130570007 |
| sklearn.datasets.make_regression: (10000, 10)    |0.0084          |0.0148   |0.0043    |0.0602  | 0.049599999 |
| sklearn.datasets.make_regression: (10000, 50)    |0.0408          |0.0763   |0.0078    |0.284  | 0.243 |
| sklearn.datasets.make_regression: (10000, 100)   |0.0813          |0.1446   |0.0142    |0.591  | 0.479200012 |
| sklearn.datasets.make_regression: (10000, 200)   |0.188           |0.3053   |0.0291    |1.2  | 0.951900024 |
| sklearn.datasets.make_regression: (10000, 400)   |0.403           |0.6191   |0.0498    |2.68| 1.96719995 |
| sklearn.datasets.make_regression: (100000, 10)   |0.0942          |0.0784   |0.0456    |0.78| 0.588 |
| sklearn.datasets.make_regression: (100000, 50)   |0.502           |0.3368   |0.1312    |4.23| 2.903 |
| sklearn.datasets.make_regression: (100000, 100)  |1.08            |0.7288   |0.2162    |8.52  | 5.504 |
| sklearn.datasets.make_regression: (100000, 200)  |2.05            |1.304    |0.3646    |17.4| 11.157 |
| sklearn.datasets.make_regression: (100000, 400)  |3.72            |2.6974   |0.559     |32.5  | 22.087 |
| sklearn.datasets.make_regression: (1000000, 10)  |1.36            |0.862    |0.658     |11.1  | 6.28 |
| sklearn.datasets.make_regression: (1000000, 50)  |5.91            |3.1822   |1.38      |54.8  | 29.956 |
| sklearn.datasets.make_regression: (1000000, 100) |11.6            |5.9278   |2.204     |115  | 59.561 |
| sklearn.datasets.make_regression: (1000000, 200) |28              |17.0186  |4.44      |  | 120.486 |
| sklearn.datasets.make_regression: (1000000, 400) |55.4            |26.56    |6.163     |  | 236.898 |

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

