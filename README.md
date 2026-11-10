
# IBEX 35 Quantitative Factor Analysis

**Machine Learning in Finance – University of Deusto**

## About the Project

I developed this project for my Machine Learning in Finance course at the University of Deusto.

The aim was to analyse stock returns from selected companies in the Spanish IBEX 35 index and identify the main factors explaining their movements. I also wanted to explore whether these factors could help explain the behaviour of the overall index and how they related to macroeconomic variables.

Rather than looking at each company individually, I wanted to understand which firms tended to move together, whether these relationships reflected common industry characteristics and how dimensionality reduction could help simplify the analysis of financial markets.

## Methodology

I carried out the analysis in R, mainly using Principal Component Analysis (PCA), Varimax rotation and regression models.

I started by calculating daily logarithmic returns for 15 IBEX 35 companies and examining the correlations between them. After the initial analysis, I excluded ROVI and Grifols because they showed relatively weak correlations with the remaining companies.

I then applied PCA to identify the main sources of variation in stock returns and selected three components. I used Varimax rotation to make the components easier to interpret and examined their loadings to identify groups of companies with similar behaviour.

To understand how these components related to the broader market, I performed a linear regression using IBEX 35 daily returns and a logistic regression to explore market movement classification.

Finally, I examined the relationships between the rotated components and several macroeconomic indicators, including interest rates, quarterly GDP growth, natural gas prices and metal prices.

## Results

The three selected components explained approximately 68% of the variance in the analysed stock returns. After Varimax rotation, the components revealed patterns associated with three main sectors: banking, energy and industrial companies.

The linear regression achieved an R² of approximately 0.86, indicating that the rotated components explained a substantial proportion of the variation in contemporaneous IBEX 35 daily returns.

The logistic regression produced a classification accuracy of approximately 64% in the original analysis. This result reflects the model's performance on the analysed data rather than independently validated forecasting accuracy.

The macroeconomic analysis also revealed relationships between the extracted components and broader economic indicators. In particular, quarterly GDP growth showed a correlation with the first rotated component, while metal price variations were associated with the third component.

Overall, the project helped me understand how PCA and factor rotation can be used to identify common patterns in financial markets, and how statistical results can be connected to economic interpretations.

## Files

- **`Assignment 2 Script.R`** — Complete R script, including data preparation, correlation analysis, PCA, Varimax rotation, regression models and macroeconomic analysis.
- **`ibex_adj_price.rds`** — Dataset containing historical adjusted stock prices of IBEX 35 companies.
- **`be1901.xls`** — Interest rate data used in the macroeconomic analysis.
- **`67824.xlsx`** — Quarterly GDP data.
- **`metal_prices.xlsx`** — Historical metal price data.
- **`Dutch TTF Natural Gas Futures Historical Data.xlsx`** — Natural gas price data.
- **`IBEX35_Factor_Analysis_Report.pdf`** — Full academic report with the methodology, statistical results, graphs and conclusions.

## Tools and Libraries

The analysis was developed in **R using Posit Cloud**.

Main libraries used:

- `psych`, `GPArotation`, `FactoMineR` — Principal Component Analysis and factor rotation.
- `ggplot2`, `factoextra`, `ggrepel`, `corrplot` — Data visualisation and PCA plots.
- `dplyr`, `tidyr`, `stringr`, `lubridate` — Data manipulation and preparation.
- `tidyquant`, `readxl` — Financial data retrieval and importing macroeconomic datasets.
- `caret`, `pROC` — Regression classification evaluation and ROC analysis.

To explore the project, open `Assignment 2 Script.R` in RStudio or Posit Cloud, install the required packages and keep the data files in the same working directory. An internet connection is required to retrieve historical IBEX 35 index data.
