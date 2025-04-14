# 📊 Shiny App: Automatic Statistical Analysis with T-Test and ANOVA

## 🚀 Overview

This Shiny application is designed to simplify statistical analysis by enabling users to compare data groups without needing to write R code. It supports **t-tests** (for two groups) and **ANOVA** (for three or more groups), offering a user-friendly interface along with powerful statistical and visualization tools.

---

## 🎯 Application Objective

The primary goal of this app is to make hypothesis testing accessible to users who may not have programming experience. By uploading a dataset, users can easily perform statistical tests and visualize the results, enabling data-driven decisions in various domains.

---

## 🧩 Main Features

### 📁 Data Upload
- Accepts `.CSV` and `.XLSX` files.
- Automatically detects headers.
- Converts columns to appropriate data types (numerical/categorical).

### 📌 Variable Selection
- Select a **dependent variable** (numerical).
- Select one or more **independent variables** (categorical).
- Dynamic UI adapts to the uploaded data structure.

### 📊 Statistical Tests
- **T-Test**: Compares means between two groups. Provides:
  - p-value
  - critical t-value
  - hypothesis decision based on significance level (α)
- **ANOVA**: For more than two groups. Outputs:
  - F-statistic
  - p-value
  - Post-hoc analysis (TukeyHSD) for pairwise group comparison

### 📈 Assumption Testing
- **Shapiro-Wilk Test**: Checks normality for each group.
- **Levene’s Test**: Verifies equality of variances across groups.

### 📉 Graphical Visualization
Interactive plots include:
- Boxplots
- Density plots
- Gaussian bell curves (for t-tests)
- Interaction plots (for multi-factor ANOVA)
- Violin plots
- Bar charts (with means and standard errors)
- Scatter plots

### 📋 Summary Tables
- Descriptive statistics (mean, median, SD, etc.)
- Grouped means for the dependent variable

---

## 🧪 Case Studies & Applicable Uses

### 🔬 Scientific Research
_Example_: Compare the effectiveness of two medications using a t-test.

### 🎓 Education
_Example_: Compare student performance across two teaching methods using a t-test.

### 🛍️ Market Research
_Example_: Use ANOVA to analyze consumer satisfaction across multiple products.

### 🏥 Public Health
_Example_: Analyze average blood pressure by age group using ANOVA.

### 🧠 Social and Behavioral Studies
_Example_: Evaluate behavior differences by demographic groups.

---

## 🧵 Study Example

### 📚 2024 New Students - National University of the Altiplano (UNA PUNO)

This study uses real data from the 2024 admissions list at UNA Puno. It explores variables such as:

- Age
- Gender
- Academic interest
- Geographical origin

The insights aim to support planning, student integration, and academic decision-making. Real data can have a transformative impact on educational management.

---

## 📦 Technologies Used

- **R**
- **Shiny**
- **ggplot2**, **dplyr**, **car**, **stats**, **readxl**, **shinyWidgets**

---

## 📁 Repository Structure

