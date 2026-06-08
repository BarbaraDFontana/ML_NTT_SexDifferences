# Machine Learning Workflow

This folder contains subfolders with:

* The original data used for machine learning model training.
* The consensus labels, generated when two experienced researchers independently agreed on the behavioral classification.
* The scripts used for data processing, model training, validation, and evaluation.
* The final trained model and performance metrics.

## Final Model Selection

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
* Area Under the Receiver Operating Characteristic Curve (AUC)

Performance summaries, confusion matrices, feature importance analyses, and model comparison plots are provided in this repository.

Based on the validation results, **XGBoost** was selected as the final model due to its consistently high performance across all evaluation metrics and behavioral classes, achieving near-perfect discrimination (AUC ≈ 1.0), high sensitivity and specificity, and excellent MCC values.

The final XGBoost model was therefore used for subsequent behavioral predictions and analyses.

## Important Note

The dataset used to train and validate the machine learning models was not used for the analysis of sex differences in zebrafish. An independent dataset was employed for the biological analyses to prevent data leakage and minimize potential bias, ensuring that the machine learning workflow and downstream biological interpretations remained fully independent.
