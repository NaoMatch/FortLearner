# FortLearner
Machine learning scripts for Fortranner.  

# WIP:


# Update
2021/09/04: Parallel implementation of 'extra_tree_regressor'.  
2021/08/31: add new 'get_matrix_count_and_sum_up_parallel_r8' for extra_tree_regressor (useless).  
2021/08/29: add new 'get_matrix_minmax_parallel' for extra_tree_regressor.  
2021/08/24: add new 'get_minmax'.  
2021/08/22: add benchmark for lawu_regressor.  
2021/08/22: add benchmark for clouds_regressor.  
2021/08/22: set '#include <stdint.h>' to inc_covariance_value_of_vectors.C  
2021/08/22: add benchmark for decision_tree_regressor.  
2021/08/22: add model dump & load to decision_tree_regressor, extra_tree_regressor, clouds_regressor, lawu_regressor  
2021/08/20: add model dump & load to linear_regression, ridge_regression, lasso_regression  

# requirement
gfortran >= 7.4.0  
gcc >= 7.4.0  
openmp  

### option
x86_64  
doxygen  
make  
graphviz

# Benchmark
## XXXX_tree_regressor(second)
n_tree=1 and max_leaf_nodes=100, others are default.  
SK = scikit-learn  
FL = FortLearner  

ET = extra_tree_regressor(FL), ExtraTreeRegressor(SK)  
DT = decision_tree_regressor(FL), DecisionTreeRegressor(SK)  
HG = clouds_regressor(FL), HistGradientBoostingRegressor(SK)  
LW = lawu_regressor(FL) , There is no counterpart to scikit-learn.

| Data: shape(#Row, #Col)         | SK: ET | FL: ET | FL: ET(fit_faster) | SK: DT | FL: DT |SK: HG|FL: CL w/ bining|FL: CL w/o bining|FL: LW w/o bining|
| ------------------------------- | ------------------------------- | -------------------------------- | --------------------------------- | -------------------------------------------- | ------------------------------- |------------------------------- |------------------------------- |------------------------------- |------------------------------- |
| YearPredictionMSD: (412206, 90) |3.12                              |2.1     |0.800     | 34.3 |25 |
| sklearn.datasets.make_regression: (100, 10)      |0.00179         |0.0036   |0.00075   |0.00176  | 0.000108 |0.00338|0.001392|0.000872|0.000634|
| sklearn.datasets.make_regression: (100, 50)      |0.00271         |0.01733  |0.00094   |0.00302  | 0.000489 |0.00725|0.004112|0.003157|0.002543|
| sklearn.datasets.make_regression: (100, 100)     |0.00359         |0.03132  |0.0009    |0.00493  | 0.001036 |0.0148|0.011865|0.009278| 0.006655|
| sklearn.datasets.make_regression: (100, 200)     |0.00545         |0.05712  |0.00127   |0.00176  | 0.001985 |0.0269|0.027636|0.021656| 0.0171  |
| sklearn.datasets.make_regression: (100, 400)     |0.00921         |0.11181  |0.00206   |0.00827  | 0.003749 |0.0416|0.051128|0.040552| 0.035   |
| sklearn.datasets.make_regression: (1000, 10)     |0.00269         |0.00909  |0.00189   |0.0146   | 0.0032   |0.0229|0.00712 |0.00586 | 0.00341 |
| sklearn.datasets.make_regression: (1000, 50)     |0.0063          |0.04195  |0.00208   |0.00644  | 0.015    |0.0708|0.06052 |0.02927 | 0.041   |
| sklearn.datasets.make_regression: (1000, 100)    |0.0108          |0.08839  |0.00283   |0.0485   | 0.0321   |0.135|0.1435  |0.11259 |  0.1016  |
| sklearn.datasets.make_regression: (1000, 200)    |0.0196          |0.16653  |0.0042    |0.0897   | 0.05973  |0.231|0.30134 |0.25577 |  0.238   |
| sklearn.datasets.make_regression: (1000, 400)    |0.0396          |0.33765  |0.00667   |0.198    | 0.13057  |0.374|0.37187 |0.26937 |  0.18178 |
| sklearn.datasets.make_regression: (10000, 10)    |0.0084          |0.0148   |0.0043    |0.0602   | 0.0496   |0.069|0.0181  |0.0123  |  0.0162  |
| sklearn.datasets.make_regression: (10000, 50)    |0.0408          |0.0763   |0.0078    |0.284    | 0.243    |0.21|0.0805  |0.0582  |   0.06    |
| sklearn.datasets.make_regression: (10000, 100)   |0.0813          |0.1446   |0.0142    |0.591    | 0.4792   |0.37|0.1808  |0.116   |   0.1084  |
| sklearn.datasets.make_regression: (10000, 200)   |0.188           |0.3053   |0.0291    |1.2      | 0.952    |0.786|0.4272  |0.2427  |  0.2259  |
| sklearn.datasets.make_regression: (10000, 400)   |0.403           |0.6191   |0.0498    |2.68     | 1.9672   |1.3|0.8592  |0.56    |    0.5131  |
| sklearn.datasets.make_regression: (100000, 10)   |0.0942          |0.0784   |0.0456    |0.78     | 0.588    |0.299|0.298   |0.095   |  0.18    |
| sklearn.datasets.make_regression: (100000, 50)   |0.502           |0.3368   |0.1312    |4.23     | 2.903    |1.2|1.17    |0.203   |    0.357   |
| sklearn.datasets.make_regression: (100000, 100)  |1.08            |0.7288   |0.2162    |8.52     | 5.504    |2.44|2.189   |0.365   |   0.461   |
| sklearn.datasets.make_regression: (100000, 200)  |2.05            |1.304    |0.3646    |17.4     | 11.157   |5.11|4.572   |0.58    |   0.931   |
| sklearn.datasets.make_regression: (100000, 400)  |3.72            |2.6974   |0.559     |32.5     | 22.087   |11.4|9.312   |1.332   |   1.911   |
| sklearn.datasets.make_regression: (1000000, 10)  |1.36            |0.862    |0.658     |11.1     | 6.28     |1.43|3.929   |0.938   |   1.704   |
| sklearn.datasets.make_regression: (1000000, 50)  |5.91            |3.1822   |1.38      |54.8     | 29.956   |4.96|12.517  |1.772   |   3.148   |
| sklearn.datasets.make_regression: (1000000, 100) |11.6            |5.9278   |2.204     |115      | 59.561   |9.91|24.866  |2.399   |   7.036   |
| sklearn.datasets.make_regression: (1000000, 200) |28              |17.0186  |4.44      |         | 120.486  |26.6|50.76   |5.852   |   8.149   |
| sklearn.datasets.make_regression: (1000000, 400) |55.4            |26.56    |6.163     |         | 236.898  |47.8|139.545 |14.76   |   21.268  |


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

