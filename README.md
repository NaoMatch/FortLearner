# FortLearner
Machine learning scripts for Fortranner.  

## requirement
gfortran >= 7.4.0  

### option
doxygen  
make  
graphviz

# Implemented
* Linear Regression: see sample_scripts/main_linear_regression.f90
  * linear_regression
  * lasso_regression
  * ridge_regression
* Stochastic Gradient Descent: see sample_scripts/main_sgd_regressor.f90
  * sgd_regressor
* Decision Tree: see sample_scripts/main_decision_tree.f90
  * decision_tree_regressor
  * extra_tree_regressor
  * clouds_regressor
* Ensemble Trees: see sample_scripts/main_forest.f90
  * random_forest_regressor
  * extra_trees_regressor
  * deep_forest_regressor(?)
* Boosting Trees: see sample_scripts/main_gradient_boosting_tree.f90
  * gradient_boosting_tree_regressor
  * gradient_boosting_extra_tree_regressor
* Dimensionality Reduction: see sample_scripts/main_pca.f90
  * pca
  
  
# ToDo
* Linear Model
  * logistic_regression
* Decision Tree
  * Axis-Parallel
    * sliq
  * Oblique
    * weighted oblique decision tree
    * simulated annealing decision tree
    * rotation tree
    * random rotation tree
    * soft decision tree
    * CART-LC
    * OC1
    * householder cart
* Ensemble Trees
* Gradient Boosting Decision Tree
  * Axis-Parallel
    * Xgboost
    * LightGBM
    * CatBoost
    * SCORE: Selective Cascade of Residual ExtraTrees
* Multi-Layer Perceptron
