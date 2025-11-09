# 🎓 Data-Driven UKT Clustering using Fuzzy Statistical Modeling

This project was developed as part of the **Workshop on Applied Data Analytics** course to design a fair and data-driven classification system for student tuition (UKT) based on socioeconomic indicators.

## 📊 Project Overview
The goal of this project is to cluster students into equitable tuition groups using fuzzy and density-based clustering algorithms. The analysis integrates multiple socioeconomic attributes to ensure that students are grouped according to financial capacity while maintaining revenue balance for the institution.

## 🧮 Data & Preprocessing
- **Dataset size:** 340 student records  
- **Main variables:**  
  - Income  
  - SPI (Initial Development Fee)  
  - Number of dependents  
  - Property ownership  
  - Electricity usage  
  - Housing type  

Data preprocessing steps include:
- Cleaning and handling missing or zero-income data  
- Imputation using regional minimum wage (UMR) mapping and logical rules  
- Normalization and encoding of categorical and ordinal variables  
- Outlier detection using Mahalanobis Distance  

## 🧠 Clustering Methods
Five algorithms were compared:
- **DBSCAN** (density-based)  
- **FCM** – Fuzzy C-Means  
- **PCM** – Possibilistic C-Means  
- **FPCM** – Fuzzy Possibilistic C-Means  
- **MFPCM** – Modified Fuzzy Possibilistic C-Means  

### 📈 Evaluation Metrics
- Silhouette Coefficient  
- BSS/TSS Ratio  
- Fuzzy Silhouette Index (FSI)

**Best model:** FCM  
- **BSS/TSS:** 0.75  
- **FSI:** 0.57  
- **Formed 8 socioeconomic clusters (Rp 500K–Rp 8M)**  

## 💡 Weighted Scoring System
Each student’s cluster score was calculated using weighted factors:

| Factor | Weight |
|--------|--------|
| Income | 40% |
| Dependents | 15% |
| SPI | 15% |
| Property & Electricity | 25% |
| Others | 5% |

## 📊 Dashboard
An analytical dashboard was developed to visualize:
- Cluster distribution by socioeconomic level  
- Tuition fairness analysis  
- Estimated total revenue ≈ **Rp 1.29 billion**

## 🛠️ Tools & Libraries
- **Language:** R  
- **Libraries:** tidyverse, dbscan, fclust, ppclust, ggplot2, cluster


## Author
**Biyan Daniswara**  
Undergraduate Student – Applied Data Science  
Politeknik Elektronika Negeri Surabaya  
