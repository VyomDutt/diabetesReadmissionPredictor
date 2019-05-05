# diabetesReadmissionPredictor #

## A R-based implementation of various machine-learning models to predict readmission in hospitals due to Diabetes. ##

The dataset used is provided by UCI and can be found [here](https://archive.ics.uci.edu/ml/datasets/diabetes+130-us+hospitals+for+years+1999-2008).

* Data has been cleaned, missing values removed and mappings changed according to the mapping file provided.

* Beautiful plots to analyse how the dataset is distributed on the basis of race, age, gender etc.

* Correlation Matrix computed to check how the variables are related with each other. Obvious inferences like relation of number of medications
  and number of time in hospital prove the correctness of the data.

* Principal Component Analysis (PCA) performed to filter out variables of importance, to improve the speed and accuracy of models implemented.

* Models applied are - 
  * Logistic Regression
  * Decision Tree
  * Random Forest
  * Nueral Network
  * Naive Bayes
  * SVM (doesn't work yet, needs correction in code)
  
* The models are compared by computing confusion matrices.

