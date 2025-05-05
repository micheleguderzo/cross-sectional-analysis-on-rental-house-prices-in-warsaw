# Cross-sectional Analysis on Rental House Prices in Warsaw – Econometrics Project

![Language](https://img.shields.io/badge/code-R-blue?logo=r&logoColor=white)
![License](https://img.shields.io/badge/license-MIT-green?logo=open-source-initiative)
![Status](https://img.shields.io/badge/status-finished-success?style=flat&logo=github)

This repository contains the final project for the course **Econometrics**, carried out during the academic year 2021/2022 at the Faculty of Economic Sciences, University of Warsaw.

## 📌 Project Overview

The goal of this project is to investigate the **determinants of rental house prices in Warsaw** using a cross-sectional dataset. The analysis aims to understand how various structural and locational factors (such as district, area, number of rooms, etc.) influence rent prices.

The project was a collaborative effort, developed in partnership with a teammate as part of an academic course.


## 🧠 Key Topics

- Linear regression and model evaluation  
- Dummy variables and categorical predictors  
- Model specification and selection  
- Multicollinearity and model diagnostics  
- Log-linear transformation and interpretation of coefficients  

## 🗂️ Repository Structure

├── README.md      # This file  
├── LICENSE        # Project license (MIT)  
├── .gitignore     # Git ignore file for excluded files/folders  
├── code/          # R script for data analysis and regression modeling  
├── data/          # Dataset used (rental housing listings in Warsaw)  
├── output/        # Tables and visualizations from the analysis  
└── report/        # Final report (PDF written in LaTeX)  

## 📊 Dataset

The dataset was collected from [Kaggle.com](https://www.kaggle.com/), consisting of **housing rental listings** in Warsaw in 2021. The dataset was cleaned and prepared before the analysis. Variables with missing or inconsistent values were handled, and categorical variables (such as districts) were encoded as dummies. The cleaned dataset includes:

- 3347 observations  
- 23 variables:  
  - **1 dependent variable**: `gross_price`  
  - **22 independent variables**: `area`, `room_num`, `floor`, `year_built`, and 18 dummy variables representing districts  

## 🛠 Tools & Libraries

- R  
- RStudio  
- LaTeX  
- R packages used:
  - `moments`  
  - `dplyr`  
  - `tseries`  
  - `epiDisplay`  
  - `tidyverse`  
  - `car`  
  - `lmtest`  
  - `MASS`  
  - `olsrr`  
  - `haven`  
  - `ggplot2`  
  - `ggpubr`  

## 📄 Final Report

You can read the full report with methodology, regression models, and conclusions in [`report/final_report.pdf`](report/final_report.pdf).

## 👤 Author

**Michele Guderzo**  
(in collaboration with another student)

## 📝 License

This project is licensed under the MIT License – see the [LICENSE](LICENSE) file for details.

---

*This project was developed for educational purposes only.*
