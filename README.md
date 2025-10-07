# WLS-OLS Time Series Forecasting

**Exploring global (OLS) and local (WLS) linear models for short- and medium-term forecasts of Denmark’s motor vehicle time series.**

This repository implements time series forecasting using **Ordinary Least Squares (OLS)** and **Weighted Least Squares (WLS)** models. The project demonstrates data preprocessing, model fitting, forecast evaluation, and visualization. Implementations are provided in **R** and **Python**.

**Date:** February-March 2024

---

## ⚙️ Problem

- Forecast Denmark’s monthly motor vehicle counts using historical data.  
- Compare **global linear trends** (OLS) vs. **local weighted trends** (WLS) for short- and medium-term predictions.  
- Evaluate model performance and visualize forecasts, prediction intervals, and residuals.

---

## 💡 Methodology

- **OLS (Global Linear Model):** Fits a linear trend to the full dataset and forecasts future values with confidence intervals.  

- **WLS (Local Linear Model):** Uses exponential decay weights to emphasize recent data. Implements recursive updates for efficient parameter estimation and multi-horizon forecasting.  

- **Evaluation & Visualization:**  
  - Residual diagnostics and autocorrelation analysis  
  - RMSE for short- and medium-term forecasts  
  - Comparison of OLS, WLS, and naive persistence models  
  - Forecast and prediction interval visualization  

---

## ⚡ Key Skills Demonstrated

- Time series modeling (OLS, WLS)  
- Forecasting and prediction interval calculation  
- Recursive parameter updates for dynamic models  
- Residual analysis and model evaluation (RMSE, diagnostics)  
- Visualization in R (`ggplot2`) and Python (`matplotlib`)  
- Data handling with Excel and scripting for reproducible workflows  

---

## 🔧 Optional Improvements

- Explore **multi-variable time series** or additional features for forecasting.  
- Incorporate **automatic lambda selection** for WLS using cross-validation.  
- Extend to **continuous or hybrid models** combining OLS/WLS with advanced machine learning.

---

## 🗂 Repository Structure

- **Assignment1.Rproj** – RStudio project for organizing scripts and environment.  

- **DST_BIL54_train.xlsx / DST_BIL54_test.xlsx** – Training (59 months) and test (12 months) time series data.  

- **Assignment1_script.R / Assignment1_script.py** – Main scripts:  
  - Load and preprocess data  
  - Fit OLS model, compute forecasts with confidence intervals  
  - Fit WLS model with exponential decay weighting  
  - Compare models using RMSE  
  - Visualize in-sample fit, out-of-sample forecasts, residuals  

- **exercise4_script.R** – Recursive WLS updates:  
  - 1-, 6-, 12-month ahead forecasts  
  - RMSE analysis for multiple lambda (decay) values  
  - Visualization of RMSE vs. lambda and optimal lambda selection  

- **part1-2.R / part3.py / part4.R** – Supporting scripts for stepwise implementation of OLS/WLS modeling and forecast visualization.

---

## 👥 Contributors

- [kongehund](https://github.com/kongehund)  
- [Fenfen (fenfen22)](https://github.com/fenfen22)  

---

## 📝 References

- Time Series Analysis course material.
- Data is from [Statistics Denmark](www.statistikbanken.dk), describing the number of motor driven vehicles in Denmark.

---


## 🚀 Usage

1. Install dependencies:  
   - **R:** `readxl`, `ggplot2`, `xtable`  
   - **Python:** `pandas`, `numpy`, `matplotlib`, `statsmodels`  
2. Place the training and test Excel files in the repository folder.  
3. Run the scripts in RStudio or Python environment.  
4. Review plots, tables, and RMSE metrics to assess model performance.


