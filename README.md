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

# Implemented
* Linear Regression: see sample_scripts/main_linear_regression.f90
  * linear_regression
  * lasso_regression
  * ridge_regression
* Logistic Regression: see sample_scripts/main_logistic_regression.f90
  * logistic_regression
* Stochastic Gradient Descent: see sample_scripts/main_sgd_regressor.f90
  * sgd_regressor
* Decision Tree: see sample_scripts/main_decision_tree.f90
  * decision_tree_regressor
  * extra_tree_regressor
  * clouds_regressor
  * lawu_regressor
* Ensemble Trees: see sample_scripts/main_forest.f90
  * random_forest_regressor
  * extra_trees_regressor
  * deep_forest_regressor(?)
* Boosting Trees: see sample_scripts/main_gradient_boosting_tree.f90
  * gradient_boosting_tree_regressor
  * gradient_boosting_extra_tree_regressor
  * gradient_boosting_clouds_regressor
  * gradient_boosting_lawu_regressor
* Dimensionality Reduction: see sample_scripts/main_pca.f90
  * pca
 
# WIP
* weighted oblique decision tree
* residual likelihood forest
* slow-growing tree
  
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
* Ensemble Trees
* Gradient Boosting Decision Tree
  * Axis-Parallel
    * Xgboost
    * LightGBM
    * CatBoost
    * SCORE: Selective Cascade of Residual ExtraTrees
    * NGBoost
* Multi-Layer Perceptron
