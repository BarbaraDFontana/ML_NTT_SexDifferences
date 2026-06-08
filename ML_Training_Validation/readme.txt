This folder contains subfolders with the original data used for training, the consensus label (label in which two experienced researchers agreed on behavior label), the scripts used for training and finally in here you can find the

**Final Model Selection**

The machine learning workflow evaluated four classification algorithms:

* Random Forest
* XGBoost
* k-Nearest Neighbors (k-NN)
* Multi-Layer Perceptron (MLP)

Model performance was assessed using the following metrics:

* Sensitivity
* Specificity
* F1-score
* Matthews Correlation Coefficient (MCC)
* Area Under the ROC Curve (AUC)

Performance comparison plots and metric summaries are provided in this repository.

Based on the validation results, **XGBoost** was selected as the final model due to its consistently high performance across all evaluation metrics and classes, achieving near-perfect discrimination (AUC ≈ 1.0), high sensitivity and specificity, and excellent MCC values.

The final XGBoost model was therefore used for subsequent predictions and analyses.

**Important:** The dataset used to train and validate the machine learning models was not used for the analysis of sex differences in zebrafish. An independent dataset was employed for biological analyses to avoid data leakage and minimize potential bias.
