# ICS_feature_selection
To address the challenge of optimally selecting informative, representative, and non-redundant features from the huge volume of data, we propose an integrative clustering and supervised (ICS) feature selection approach. In our framework, the unsupervised clutering contributes to reduce feature redundancy by exploring the correlation among features, while supervised learning selects informative and representative features by examining relavency between features and target outputs. The algorithm was implemented in R and validated on two datasets including CT image dataset and clinical factor dataset. 
![Flowchart](https://user-images.githubusercontent.com/10879680/110190303-baca6880-7e76-11eb-8a92-fccc5e5cf2d0.jpg)


# Citation for ICS feature selection
Xin, Bowen, et al. "Integrative Clustering and Supervised Feature Selection for Clinical Applications." 2018 15th International Conference on Control, Automation, Robotics and Vision (ICARCV). IEEE, 2018.

# Prerequisites
- R environment
- packages: dplyr, survival, readxl, xlsx

# Functions
- Get stable features over repeated meaturements: 
  `getStableCor_func(df1, df2, threshold=0.7, method="pearson")`
- Get prognostic features by rank (Cox):
  `getBestCox(dataset, nFeature=5, rankby="p.value", p.threshold = 0.05)`
- Get prognostic features by rank (RSF):
  `getBestRSF(dataset, nFeature, seed = 1000, ntree = 2000)`
- Get ICS features (Cox):
  `getBestCluster(dataset, nFeature = 5, maxK = 15, optK = NULL,rankby = "p.value")`
- Get ICS features (RSF):
  `getBestCluster2(dataset, nFeature = 5, maxK = 15, optK = NULL,rankby = "p.value")`
- Fit multivaraite model (Cox):
  `multi_cox(dataset, features)`
- Fit multivariate model (RSF):
  `multi_rsf(dataset, features)`
