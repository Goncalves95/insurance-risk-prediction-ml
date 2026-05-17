# Insurance Risk Prediction with Machine Learning (R)

A supervised machine learning project designed to evaluate and predict automotive insurance underwriting risk (`AltoRisco` vs. `BaixoRisco`). This project implements, evaluates, and critically analyzes three distinct algorithmic paradigms—Decision Trees, k-Nearest Neighbors, and Artificial Neural Networks—using a highly controlled, balanced demographic dataset.

This repository contains the complete academic technical report, dataset, and clean production-ready R code developed as part of the **Machine Learning** curriculum.

---

## 📊 Project Architecture & Key Results

The performance of each algorithm was rigorously measured on an isolated testing partition (20% holdout split) with a fixed randomization seed (`2304336`) to guarantee mathematical reproducibility.

### Performance Metrics Comparison

| Evaluation Metric | Decision Tree (`rpart`) | k-NN ($k=3$) | Neural Network (`neuralnet`) |
| :--- | :---: | :---: | :---: |
| **Accuracy** | **78.57%** | **64.29%** | **50.00%** |
| **Sensitivity (Recall)** | **71.43%** | **71.43%** | **0.00%** |
| **Specificity** | **85.71%** | **57.14%** | **100.00%** |
| **Cohen's Kappa** | **0.5714** | **0.2857** | **0.0000** |

### Core Findings & Engineering Insights
* **The Dominance of Logic Rules:** The **Decision Tree** emerged as the superior model (**78.57% Accuracy**). This highlights that insurance risk thresholds in this specific demographic are highly constrained by discrete, orthogonal boundaries (e.g., historical accident thresholds) rather than geometric distances or spatial proximity.
* **The Complexity Paradox:** The **Artificial Neural Network (ANN)** suffered from severe underfitting (**50.00% Accuracy**), acting as a blind majority class classifier. This provides an empirical validation of *Occam's Razor* in Data Science: high-variance architectures fail to converge on small sample sizes ($n=75$) due to local minima traps during backpropagation.
* **Business & Compliance Value:** Beyond pure statistical metrics, the Decision Tree offers absolute business interpretability. In highly regulated environments (such as GDPR compliance), it guarantees the "right to explanation" for actuarial pricing, whereas k-NN and ANNs act as un-auditable "black boxes".

---

## 🛠️ Repository Structure

```text
├── 2304336_E_Folio_B_RRC.pdf     # Full 13-page academic technical report (Portuguese)
├── dataset_Seguros_2304336.csv   # Structured and balanced insurance customer dataset
├── RRC_2304336_Analise.R         # Clean, automated, and isolated R execution script
└── README.md                     # English portfolio presentation (This file)