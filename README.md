# Predictive Analysis of Mental Health Treatment-Seeking Behavior in the Tech Industry

## Project Overview
This project investigates the key factors influencing mental health treatment-seeking behavior among employees in the technology sector. Using statistical modeling and machine learning algorithms, the study predicts the likelihood of an employee seeking professional treatment based on demographic, workplace, and mental health-related predictors.

## Tech Stack & Tools
* **Language:** R
* **Libraries:** `tidyverse`, `caret`, `glmnet` (Elastic Net/Lasso/Ridge), `randomForest`, `rpart`, `pROC`
* **Methodologies:** Exploratory Data Analysis (EDA), Cochran-Mantel-Haenszel (CMH) Test, Stepwise Logistic Regression, Regularization, Cost-Complexity Pruning

## Dataset
* **Observations:** 1,250 technology sector employees
* **Target Variable:** `treatment` (Binary: "Yes" or "No" - indicates whether the individual has sought professional treatment)
* **Key Features:** Age, Gender, family_history, work_interfere, benefits, care_options, anonymity

## Key Findings & Statistical Insights
* **Strongest Predictors:** A family history of mental illness (`family_history`), the degree to which mental health interferes with work (`work_interfere`), and employer-provided mental health benefits (`benefits`) emerged as the strongest predictors of treatment-seeking behavior.
* **Controlling for Confounders:** The Cochran-Mantel-Haenszel (CMH) test confirmed that the association between family history and seeking treatment persists independently across gender strata. 

## Machine Learning Models & Performance
Several algorithms were evaluated on a completely unseen 30% hold-out test set. The evaluation metrics included Accuracy, Sensitivity, Specificity, F1-Score, and ROC-AUC

| Model | ROC-AUC | Sensitivity |
| :--- | :---: | :---: |
| **Elastic Net (Best Model)** | **0.8067** | **0.9189** |
| Logistic Regression (Stepwise) | 0.8059 | 0.8919 |
| Ridge / Lasso | ~0.805 | ~0.913 |
| Random Forest | 0.8043 | 0.8757 |

**Model Conclusion:** The **Elastic Net** model provided the most robust and stable performance by effectively balancing model complexity and handling multicollinearity issues. 

## Repository Contents
* `mental_health_analysis.R`: Full data pipeline including cleaning, EDA, hypothesis testing, and machine learning model training.
* `mental_health_report.pdf`: Detailed academic report interpreting the statistical findings and model outputs.
