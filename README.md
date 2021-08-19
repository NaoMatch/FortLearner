# FortLearner
Machine learning scripts for Fortranner.  

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
| Data: shape(#Row, #Col)         | Scikit-learn: ExtraTreeRegressor.fit() | FortLearner: extra_tree_regressor%fit() | FortLearner: extra_tree_regressor%fit_faster() | 
| ------------------------------- | -------------------------------- | --------------------------------- | -------------------------------------------- | 
| YearPredictionMSD: (412206, 90) | 3.12                             | 2.1                               | 0.800                                        | 


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
