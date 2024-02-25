# FortLearner
Machine learning scripts for Fortranner.  
https://qiita.com/advent-calendar/2022/fortlearner

## how to run
cd test_script  
make test_all  

# WIP:  
[Forest Packing](https://arxiv.org/abs/1806.07300) like implementation for Python  
~Benchmark and Optimize KDTree, Naive Bayse, Implement Linear/Kernel SVR, One-Class SVM, ica, HDBSCAN(not accelarated one)~


# Update
2024/02/25: replace the weighted sampling method with a method that uses ↓  
2024/02/18: branchless binary search and prefix sum with simd  
2024/01/14: apply mydgemv to kdtree (replace openblas 'dgemv')  
2024/01/09: my dgemv  
2023/06/25: models can accept data_holder not only x  
2023/06/10: fix and release random seed    
2023/06/10: [Forest Packing](https://arxiv.org/abs/1806.07300) like implementation, 2x faster than naive implementation  
2023/06/05: Fast Ingerence Engine for Decision Trees (https://cds.cern.ch/record/2688585/files/AA_main.pdf)
2023/04/08: Local Outlier Factor  
2023/04/02: apply openmp for FOREST%predict(x, parallel=.true.)  
2023/03/29: Thinnnig (isolation_forest)  
2023/02/21: KNN classifier  
2023/02/20: KNN regressor  
2023/02/18: Implement and Optimize Kernel SVC (linear, poly, sigmoid, rbf) and add benchmark    
2023/01/30: optimize 'linear' support vector machine classifier  
2023/01/01: hash_map without delete  
2022/12/25: svm (not so optimized)  
2022/10/07: multi layer perceptron (wengert list, reverse mode)  
2022/07/28: bug fix(kdtree, balltree)  
2022/07/24: product_quantization  
2022/06/11: adaboost  
2022/05/03: nipals  
2022/05/02: dbscan(modify 2022/05/19)  
2022/04/27: oblivious_tree (not so optimized)  
2022/04/22: minibatch_kmeans  
2022/04/19: sliq (base estimator of xgboost, w/o openmp)  
2022/02/15: threshold_tree  
2022/01/22: breathing_kmeans  
2022/01/09: locality_sensitive_hashing(random projection, p-stable random projection), balltree(n_neighbors, radius), exact_duplicate_search, one_at_a_time_hash, hash_table(no collision check)  
2021/12/16: kdtree(n_neighbors, radius)  
2021/11/23: (postponed) re-implement matrix-vector multiplication for accelerating kmeans  
2021/10/12: add 'isolation_forest' benchmark  
2021/10/10: Implemented elkan's method to speed up "kmeans", but it didn't make sense.  
2021/09/28: speed up 'kmeans++'(see benchmark)  
2021/09/24: isolation_tree and isolation_forest  
2021/09/21: kmeans++  
2021/09/20: simulated annealing initial temperature  
2021/09/19: refactering simulated annealing  
2021/09/17: implement sadt_regressor, simulated-annealing decision tree.  
2021/09/14: Add Create Dataset Scripts.  
2021/09/14: 'multi_mat_x_vec' with simd.  
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
Python >= 3.7.3  (to create sample datasets)  
scikit-learn >= 0.23.2  (to create sample datasets)  

### option
x86_64  
doxygen  
make  
graphviz

# Benchmark

|Metric|Accuracy|Accuracy|Accuracy|Accuracy|Time|Time|
|:----|:----|:----|:----|:----|----:|----:|
|Library|FL|SK|FL|SK|FL|SK|
|Dataset|Train|Train|Test|Test|Train|Train|
|sklearn.datasets.make_regression: (100, 5)|0.940|0.950|1.000|1.000|0.00013|0.00073|
|sklearn.datasets.make_regression: (100, 10)|0.970|0.980|0.900|0.750|0.00011|0.00090|
|sklearn.datasets.make_regression: (100, 50)|0.990|1.000|0.750|0.750|0.00024|0.00110|
|sklearn.datasets.make_regression: (100, 100)|1.000|1.000|0.700|0.850|0.00113|0.00123|
|sklearn.datasets.make_regression: (100, 200)|1.000|1.000|0.550|0.650|0.00132|0.00130|
|sklearn.datasets.make_regression: (100, 400)|1.000|1.000|0.750|0.650|0.00197|0.00164|
|sklearn.datasets.make_regression: (1000, 5)|0.872|0.926|0.855|0.900|0.00457|0.01122|
|sklearn.datasets.make_regression: (1000, 10)|0.957|0.962|0.940|0.965|0.00675|0.01199|
|sklearn.datasets.make_regression: (1000, 50)|0.931|0.961|0.810|0.880|0.02272|0.03760|
|sklearn.datasets.make_regression: (1000, 100)|0.878|0.970|0.685|0.745|0.03542|0.04781|
|sklearn.datasets.make_regression: (1000, 200)|0.995|0.997|0.950|0.965|0.04629|0.07057|
|sklearn.datasets.make_regression: (1000, 400)|0.994|0.998|0.810|0.870|0.09561|0.10084|
|sklearn.datasets.make_regression: (10000, 5)|0.894|0.894|0.903|0.901|1.25263|1.57138|
|sklearn.datasets.make_regression: (10000, 10)|0.932|0.935|0.923|0.926|0.94122|1.31254|
|sklearn.datasets.make_regression: (10000, 50)|0.956|0.959|0.918|0.922|1.46329|2.76745|
|sklearn.datasets.make_regression: (10000, 100)|0.960|0.974|0.905|0.925|1.69233|3.57061|
|sklearn.datasets.make_regression: (10000, 200)|0.959|0.979|0.878|0.913|3.35433|8.65822|
|sklearn.datasets.make_regression: (10000, 400)|0.969|0.968|0.869|0.867|8.48600|19.53811|
|sklearn.datasets.make_regression: (100000, 5)|0.932|0.944|0.933|0.946|91.43300|94.09670|
|sklearn.datasets.make_regression: (100000, 10)|0.886|0.914|0.882|0.910|268.21300|157.08460|
|sklearn.datasets.make_regression: (100000, 50)|0.842|0.892|0.813|0.844|467.38100|864.29674|
|sklearn.datasets.make_regression: (100000, 100)|0.971|0.977|0.952|0.956|188.23800|474.92779|
|sklearn.datasets.make_regression: (100000, 200)|0.928|0.937|0.888|0.893|451.82700|2611.47660|
|sklearn.datasets.make_regression: (100000, 400)|0.989|0.985|0.959|0.963|277.82000|2312.59334|



## XXXX_tree_regressor(second)
n_tree=1 and max_leaf_nodes=100, others are default.  
SK = scikit-learn  
FL = FortLearner  

ET = extra_tree_regressor(FL), ExtraTreeRegressor(SK)  
DT = decision_tree_regressor(FL), DecisionTreeRegressor(SK)  
HG = clouds_regressor(FL), HistGradientBoostingRegressor(SK)  
LW = lawu_regressor(FL) , There is no counterpart to scikit-learn.

|Data: shape(#Row, #Col)|SK: ET|FL: ET|FL: ET(fit_faster)|SK: DT|FL: DT|SK: HG|FL: CL w/ bining|FL: CL w/o bining|FL: LW w/o bining|
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|
|YearPredictionMSD: (412206, 90)|3.12|2.1|0.8|34.3|25|-|-|-|-|
|sklearn.datasets.make_regression: (100, 10)|0.00179|0.0036|0.00075|0.00176|0.000108|0.00338|0.001392|0.000872|0.000634|
|sklearn.datasets.make_regression: (100, 50)|0.00271|0.01733|0.00094|0.00302|0.000489|0.00725|0.004112|0.003157|0.002543|
|sklearn.datasets.make_regression: (100, 100)|0.00359|0.03132|0.0009|0.00493|0.001036|0.0148|0.011865|0.009278|0.006655|
|sklearn.datasets.make_regression: (100, 200)|0.00545|0.05712|0.00127|0.00176|0.001985|0.0269|0.027636|0.021656|0.0171|
|sklearn.datasets.make_regression: (100, 400)|0.00921|0.11181|0.00206|0.00827|0.003749|0.0416|0.051128|0.040552|0.035|
|sklearn.datasets.make_regression: (1000, 10)|0.00269|0.00909|0.00189|0.0146|0.0032|0.0229|0.00712|0.00586|0.00341|
|sklearn.datasets.make_regression: (1000, 50)|0.0063|0.04195|0.00208|0.00644|0.015|0.0708|0.06052|0.02927|0.041|
|sklearn.datasets.make_regression: (1000, 100)|0.0108|0.08839|0.00283|0.0485|0.0321|0.135|0.1435|0.11259|0.1016|
|sklearn.datasets.make_regression: (1000, 200)|0.0196|0.16653|0.0042|0.0897|0.05973|0.231|0.30134|0.25577|0.238|
|sklearn.datasets.make_regression: (1000, 400)|0.0396|0.33765|0.00667|0.198|0.13057|0.374|0.37187|0.26937|0.18178|
|sklearn.datasets.make_regression: (10000, 10)|0.0084|0.0148|0.0043|0.0602|0.0496|0.069|0.0181|0.0123|0.0162|
|sklearn.datasets.make_regression: (10000, 50)|0.0408|0.0763|0.0078|0.284|0.243|0.21|0.0805|0.0582|0.06|
|sklearn.datasets.make_regression: (10000, 100)|0.0813|0.1446|0.0142|0.591|0.4792|0.37|0.1808|0.116|0.1084|
|sklearn.datasets.make_regression: (10000, 200)|0.188|0.3053|0.0291|1.2|0.952|0.786|0.4272|0.2427|0.2259|
|sklearn.datasets.make_regression: (10000, 400)|0.403|0.6191|0.0498|2.68|1.9672|1.3|0.8592|0.56|0.5131|
|sklearn.datasets.make_regression: (100000, 10)|0.0942|0.0784|0.0456|0.78|0.588|0.299|0.298|0.095|0.18|
|sklearn.datasets.make_regression: (100000, 50)|0.502|0.3368|0.1312|4.23|2.903|1.2|1.17|0.203|0.357|
|sklearn.datasets.make_regression: (100000, 100)|1.08|0.7288|0.2162|8.52|5.504|2.44|2.189|0.365|0.461|
|sklearn.datasets.make_regression: (100000, 200)|2.05|1.304|0.3646|17.4|11.157|5.11|4.572|0.58|0.931|
|sklearn.datasets.make_regression: (100000, 400)|3.72|2.6974|0.559|32.5|22.087|11.4|9.312|1.332|1.911|
|sklearn.datasets.make_regression: (1000000, 10)|1.36|0.862|0.658|11.1|6.28|1.43|3.929|0.938|1.704|
|sklearn.datasets.make_regression: (1000000, 50)|5.91|3.1822|1.38|54.8|29.956|4.96|12.517|1.772|3.148|
|sklearn.datasets.make_regression: (1000000, 100)|11.6|5.9278|2.204|115|59.561|9.91|24.866|2.399|7.036|
|sklearn.datasets.make_regression: (1000000, 200)|28|17.0186|4.44|-|120.486|26.6|50.76|5.852|8.149|
|sklearn.datasets.make_regression: (1000000, 400)|55.4|26.56|6.163|-|236.898|47.8|139.545|14.76|21.268|


## kmeans(second&plusmn;standard deviation)
KM = Kmeans()  
KM_naive: Naive Implementation in Fortlearner  
KM_fast: Fast  Implementation in Fortlearner(Simplified Calculation of Euclid Distance + Fast Matrix-Vector Multiplication + Naive Centroid Update Skip)  
FL: mean±std of 40 runs  
SK: mean±std of 4 runs  
| Data: shape(#Row, #Col)         | #Cluster | SK: KM | FL: KM_naive | FL: KM_fast |
| ------------------------------- | -------- | ------ | ------------ | ----------- |
| YearPredictionMSD: (412206, 90) | 2        |.757(&plusmn;0.0128)        |1.202(&plusmn;0.247)         |0.628(&plusmn;0.105)              |
| YearPredictionMSD: (412206, 90) | 3        |1.47(&plusmn;0.0180)        |3.260(&plusmn;1.865)         |1.880(&plusmn;0.61)              |
| YearPredictionMSD: (412206, 90) | 4        |2.56(&plusmn;1.28)        |2.867(&plusmn;0.599) |1.725(&plusmn;0.649) |
| YearPredictionMSD: (412206, 90) | 5        |3.54(&plusmn;0.335)        |6.573(&plusmn;3.428) |3.08(&plusmn;1.523) |
| YearPredictionMSD: (412206, 90) | 10       |7.4(&plusmn;0.106)        |20.986(&plusmn;7.226) |11.457(&plusmn;3.693) |
| YearPredictionMSD: (412206, 90) | 15       |13.3(&plusmn;12.9)        |38.954(&plusmn;10.945) |21.025(&plusmn;7.5) |
| YearPredictionMSD: (412206, 90) | 20       |22.5(&plusmn;19.1)        |55.788(&plusmn;19.302) |34.358(&plusmn;12.222) |
| YearPredictionMSD: (412206, 90) | 25       |24.1(&plusmn;31)        |98.84(&plusmn;34.977) |55.001(&plusmn;23.099) |
| YearPredictionMSD: (412206, 90) | 30       |29.1(&plusmn;14.2)        |157.163(&plusmn;52.773) |82.906(&plusmn;25.715) |
| YearPredictionMSD: (412206, 90) | 35       |34.0(&plusmn;6.46)        |184.827(&plusmn;58.211) |99.948(&plusmn;31.88) |

## isolation_forest
IF = IsolationForest(n_estimators=100, max_samples=256, n_jobs=8)  
Train:Validation:Test = 3:1:1  
Datasets http://odds.cs.stonybrook.edu/  
Thinnig = Remove trees with low average height.  
Thinnig Detail https://www.scutum.jp/information/waf_tech_blog/2021/06/waf-blog-079.html .  
100 iteration, auc mean±std

| | | |100 trees w/o thinning|1000 trees w/o thinning|100 trees w/ thinning (create 1000 trees, remove 900 trees)|100 trees w/o thinning|1000 trees w/o thinning|100 trees w/ thinning (create 1000 trees, remove 900 trees)|thinning improvement with same #trees|thinning improvement with same #trees|sklearn 100 trees|sklearn 100 trees|thinning improvement (Fortlearner - Sklearn)|thinning improvement (Fortlearner - Sklearn)|Train[Time]| |Sklearn[msec]|
|:----|----:|----:|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|
|data|#Rows|#Cols|Train AUC|Train AUC|Train AUC|Test AUC|Test AUC|Test AUC|Train AUC|Test AUC|Train AUC|Test AUC| | |w/o|w/| |
|http|340,498|3|0.9954±0.0008|0.9956±0.0003|0.9981±0.0013|0.9957±0.0007|0.9959±0.0003|0.9980±0.0012|0.0027|0.0023| 0.9996±0.0005| 0.9996±0.0005|-0.0015|-0.0016|3.44|8.89|155.2|
|cover|171,628|10|0.8842±0.0258|0.8926±0.0075|0.9294±0.0163|0.8870±0.0252|0.8952±0.0073|0.9307±0.0160|0.0452|0.0437| 0.8806±0.0258| 0.8839±0.0251|0.0488|0.0468|4.77|28.79|143.84|
|smtp|57,092|3|0.9055±0.0085|0.9052±0.0025|0.9177±0.0076|0.9219±0.0058|0.9215±0.0021|0.9284±0.0059|0.0122|0.0065| 0.9033±0.0057| 0.9174±0.0059|0.0144|0.011|2.68|16.11|143.4|
|shuttle|29,457|9|0.9968±0.0007|0.9975±0.0002|0.9979±0.0004|0.9958±0.0009|0.9966±0.0002|0.9973±0.0005|0.0011|0.0015| 0.9971±0.0007| 0.9960±0.0010|0.0008|0.0013|2.7|19.47|147.55|
|mammography|6,709|6|0.8722±0.0061|0.8754±0.0024|0.8831±0.0049|0.8074±0.0098|0.8109±0.0029|0.8240±0.0083|0.0109|0.0166| 0.8689±0.0072| 0.8015±0.0106|0.0142|0.0225|2.19|15.31|432.14|
|mnist|4,561|100|0.7940±0.0362|0.8189±0.0145|0.8093±0.0272|0.7981±0.0339|0.8209±0.0134|0.8176±0.0254|0.0153|0.0195| 0.8023±0.0151| 0.7919±0.0146|0.007|0.0257|1.13|5.08|189.09|
|annthyroid|4,320|6|0.8154±0.0136|0.8192±0.0047|0.8186±0.0192|0.8121±0.0135|0.8154±0.0047|0.8149±0.0197|0.0032|0.0028| 0.8167±0.0160| 0.8158±0.0164|0.0019|-0.0009|7.03|23.06|570.2|
|pendigits|4,122|16|0.9435±0.0103|0.9475±0.0035|0.9517±0.0079|0.9450±0.0119|0.9499±0.0036|0.9540±0.0082|0.0082|0.0090| 0.9431±0.0111| 0.9419±0.0136|0.0086|0.0121|4.06|32.13|240.05|
|satellite|3,861|36|0.7072±0.0160|0.7053±0.0061|0.6567±0.0124|0.7233±0.0158|0.7213±0.0064|0.6675±0.0126|-0.0505|-0.0558| 0.7097±0.0176| 0.7202±0.0162|-0.053|-0.0527|3.62|26.63|260.84|
|satimage-2|3,481|36|0.9983±0.0005|0.9987±0.0002|0.9976±0.0006|0.9919±0.0022|0.9927±0.0007|0.9904±0.0025|-0.0007|-0.0015| 0.9982±0.0006| 0.9920±0.0023|-0.0006|-0.0016|3.52|25.23|219.47|
|optdigits|3,128|64|0.6640±0.0622|0.6786±0.0204|0.5960±0.0537|0.6646±0.0657|0.6797±0.0223|0.5789±0.0567|-0.0680|-0.0857| 0.7005±0.0414| 0.6818±0.0476|-0.1045|-0.1029|2.1|12.49|275.28|
|thyroid|2,262|6|0.9790±0.0030|0.9799±0.0008|0.9779±0.0026|0.9629±0.0055|0.9646±0.0017|0.9614±0.0045|-0.0011|-0.0015| 0.9790±0.0030| 0.9567±0.0054|-0.0011|0.0047|2.78|18.85|277.12|
|speech|2,210|400|0.4119±0.0208|0.4058±0.0077|0.4149±0.0193|0.4803±0.0375|0.4729±0.0127|0.4698±0.0319|0.0030|-0.0105| 0.4142±0.0205| 0.4705±0.0336|0.0007|-0.0007|17.45|210.68|261.18|
|musk|1,836|166|0.9996±0.0006|1.0000±0.0000|0.9999±0.0002|0.9989±0.0017|1.0000±0.0001|0.9998±0.0005|0.0003|0.0009| 0.9997±0.0008| 0.9994±0.0013|0.0002|0.0004|4.42|35.86|266.56|
|cardio|1,097|21|0.9238±0.0119|0.9276±0.0037|0.9345±0.0083|0.9480±0.0123|0.9522±0.0036|0.9580±0.0076|0.0107|0.0100| 0.9216±0.0112| 0.9427±0.0135|0.0129|0.0153|12.3|20.93|196.93|
|letter|960|32|0.6440±0.0184|0.6481±0.0062|0.6245±0.0180|0.6415±0.0305|0.6486±0.0086|0.5991±0.0234|-0.0195|-0.0424| 0.6374±0.0202| 0.6293±0.0274|-0.0129|-0.0302|3.25|23.58|143.48|
|vowels|872|12|0.8058±0.0231|0.8162±0.0079|0.7405±0.0254|0.7002±0.0295|0.7065±0.0093|0.6527±0.0256|-0.0653|-0.0475| 0.7966±0.0268| 0.7179±0.0337|-0.0561|-0.0652|3.36|25.49|156.13|
|pima|460|8|0.6634±0.0112|0.6683±0.0035|0.6578±0.0124|0.6981±0.0145|0.7037±0.0045|0.6826±0.0155|-0.0056|-0.0155| 0.6621±0.0104| 0.6988±0.0144|-0.0043|-0.0162|4.05|21.54|155.48|
|breastw|409|9|0.9848±0.0017|0.9858±0.0005|0.9893±0.0011|0.9873±0.0031|0.9880±0.0010|0.9896±0.0028|0.0045|0.0023| 0.9832±0.0017| 0.9860±0.0030|0.0061|0.0036|11.34|25.87|221.07|
|arrhythmia|270|274|0.8095±0.0189|0.8228±0.0063|0.8117±0.0135|0.7164±0.0237|0.7163±0.0078|0.7067±0.0187|0.0022|-0.0097| 0.8155±0.0124| 0.7230±0.0157|-0.0038|-0.0163|12.49|12.97|236.56|
|wbc|226|30|0.9040±0.0101|0.9042±0.0036|0.9031±0.0081|0.9962±0.0027|0.9967±0.0011|0.9979±0.0023|-0.0009|0.0017| 0.9010±0.0105| 0.9985±0.0018|0.0021|-0.0006|2.93|19.7|153.43|
|ionosphere|209|33|0.8372±0.0067|0.8389±0.0022|0.8463±0.0049|0.9065±0.0055|0.9105±0.0027|0.9069±0.0059|0.0091|0.0004| 0.8445±0.0064| 0.9134±0.0057|0.0018|-0.0065|2.73|21.64|150.99|
|vertebral|144|6|0.3295±0.0210|0.3314±0.0063|0.3079±0.0178|0.4286±0.0542|0.4224±0.0226|0.3563±0.0323|-0.0216|-0.0723| 0.3317±0.0190| 0.3504±0.0473|-0.0238|0.0059|2.98|18.24|141.82|
|glass|128|9|0.6519±0.0174|0.6566±0.0053|0.6456±0.0191|0.8970±0.0134|0.8907±0.0024|0.9083±0.0146|-0.0063|0.0113| 0.6543±0.0178| 0.9254±0.0294|-0.0087|-0.0171|1.29|9.72|147.59|
|lympho|88|18|0.9878±0.0064|0.9932±0.0021|0.9899±0.0044|1.0000±0.0000|1.0000±0.0000|1.0000±0.0000|0.0021|0.0000| 0.9971±0.0027| 1.0000±0.0000|-0.0072|0|1.06|6.24|140.96|
|wine|77|13|0.7598±0.0357|0.7639±0.0132|0.7808±0.0302|0.9727±0.0199|0.9798±0.0055|0.9771±0.0146|0.0210|0.0044| 0.7474±0.0410| 0.9681±0.0269|0.0334|0.009|1.58|10.56|140.16|







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
  * sliq_regressor
  * oblivious_tree_regressor  
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
  * nipals
* Clustering
  * kmeans
  * minibatch_kmeans
  * breathing_kmeans
  * threshold_tree
  * dbscan
* Anomaly Detection
  * isolation_tree
  * isolation_forest
* Nearest Neighbours Search
  * kdtree
  * balltree
  * bruteforce
  * lsh
  * product_quantization
* Neural Network
  * simple mlp
* Support Vector Machine
  * support vector classifier
 
# ToDo
* Matrix Factorization
  * Matrix Factorization
  * Non-Negative Matrix Factorization
* Decision Tree
  * Axis-Parallel
    * residual likelihood tree
    * oblivious_tree
  * Oblique
    * simulated annealing decision tree
    * rotation tree
    * soft decision tree
    * CART-LC
    * OC1
    * householder cart
    * weighted oblique decision tree
    * residual likelihood forest
    * slow-growing tree
* Ensemble Trees
    * Extended Isolation Forest(anomaly detection)
    * PIDForest(anomaly detection)
    * Cross-Cluster Weighted Forests
    * WildWood
* Gradient Boosting Decision Tree
  * Axis-Parallel
    * Xgboost
    * LightGBM
    * CatBoost
    * SCORE: Selective Cascade of Residual ExtraTrees
    * NGBoost
* Multi-Layer Perceptron
* Neural Network
  * convolution
* Decomposition
  * Independent Component Analysis
 
