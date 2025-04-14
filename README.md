# Shiny Statistical Analysis Application

## Overview
This Shiny-based application enables users to conduct automatic statistical analyses on their uploaded datasets. It simplifies complex statistical processes, making it accessible for users without advanced knowledge of R. The application supports two main statistical tests: t-test and ANOVA, along with interactive data visualizations and exploratory tools.

---

## Features
- **Data Upload:** Supports CSV and Excel file uploads with automatic detection of headers and appropriate column formatting (numerical or categorical).
- **Variable Selection:** Users can choose dependent and independent variables dynamically based on uploaded data types.
- **Statistical Analysis:**
  - **t-test:** Compare means of two groups with p-value, critical t-value, and null hypothesis evaluation.
  - **ANOVA:** Evaluate differences among more than two groups, including post-hoc analysis (TukeyHSD).
- **Assumption Tests:**
  - Normality test (Shapiro-Wilk) for normal distribution verification.
  - Homogeneity of variances test (Levene) for equal variance checking.
- **Data Visualizations:** Includes boxplots, density plots, Gaussian curves, interaction plots, violin plots, bar charts, scatter plots, and more.
- **Summary Tables:** Provides descriptive statistics and group-level summaries.

---

## Installation
1. Clone this repository:
   ```bash
   git clone https://github.com/<your-username>/<repository-name>.git
