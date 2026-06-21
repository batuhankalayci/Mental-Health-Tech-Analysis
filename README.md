# Predictive Analysis of Mental Health Treatment-Seeking Behavior in the Tech Industry

## 📌 Project Overview
This project investigates the key factors influencing mental health treatment-seeking behavior among employees in the technology sector[cite: 9]. Using statistical modeling and machine learning algorithms, the study predicts the likelihood of an employee seeking professional treatment based on demographic, workplace, and mental health-related predictors[cite: 9].

## 🛠️ Tech Stack & Tools
* **Language:** R[cite: 8]
* **Libraries:** `tidyverse`, `caret`, `glmnet` (Elastic Net/Lasso/Ridge), `randomForest`, `rpart`, `pROC`[cite: 8]
* **Methodologies:** Exploratory Data Analysis (EDA), Cochran-Mantel-Haenszel (CMH) Test, Stepwise Logistic Regression, Regularization, Cost-Complexity Pruning[cite: 8, 9]

## 📊 Dataset
* **Observations:** 1,250 technology sector employees[cite: 9]
* **Target Variable:** `treatment` (Binary: "Yes" or "No" - indicates whether the individual has sought professional treatment)[cite: 9]
* **Key Features:** Age, Gender, family_history, work_interfere, benefits, care_options, anonymity[cite: 9]

## 🚀 Key Findings & Statistical Insights
* **Strongest Predictors:** A family history of mental illness (`family_history`), the degree to which mental health interferes with work (`work_interfere`), and employer-provided mental health benefits (`benefits`) emerged as the strongest predictors of treatment-seeking behavior[cite: 9].
* **Controlling for Confounders:** The Cochran-Mantel-Haenszel (CMH) test confirmed that the association between family history and seeking treatment persists independently across gender strata[cite: 8, 9]. 

## 🤖 Machine Learning Models & Performance
Several algorithms were evaluated on a completely unseen 30% hold-out test set[cite: 9]. The evaluation metrics included Accuracy, Sensitivity, Specificity, F1-Score, and ROC-AUC[cite: 9].

| Model | ROC-AUC | Sensitivity |
| :--- | :---: | :---: |
| **Elastic Net (Best Model)** | **0.8067** | **0.9189** |
| Logistic Regression (Stepwise) | 0.8059 | 0.8919 |
| Ridge / Lasso | ~0.805 | ~0.913 |
| Random Forest | 0.8043 | 0.8757 |

**Model Conclusion:** The **Elastic Net** model provided the most robust and stable performance by effectively balancing model complexity and handling multicollinearity issues[cite: 9]. 

## 📂 Repository Contents
* `mental_health_analysis.R`: Full data pipeline including cleaning, EDA, hypothesis testing, and machine learning model training[cite: 8].
* `mental_health_report.pdf`: Detailed academic report interpreting the statistical findings and model outputs[cite: 9].
