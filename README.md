# STAT 5243 - Applied Data Science 
# Project 2 - Interactive Data Analysis Shiny Application

This repository contains an R Shiny web application for interactive data analysis, cleaning, preprocessing, feature engineering, and visualization, developed by Team 11 (Shayan Chowdhury, Ran Yen, Zijun Fu, and Tiantian Li) for Prof. Alex Pijyan's STAT5243 Applied Data Science course in Spring 2025.

## Project Overview

This Shiny application provides a versatile web-based tool for data scientists and analysts. The application allows users to:

1. Load datasets (CSV, Excel, JSON, RDS)
2. Clean and preprocess data
3. Create new features through feature engineering
4. Perform exploratory data analysis with interactive visualizations

## Features

### Data Loading
- Upload files in various formats (CSV, Excel, JSON, RDS)
- Use built-in example datasets (Iris, US Economic Time Series, US Crime Statistics)
- Preview data before analysis

### Data Cleaning and Preprocessing
- Handle missing values through various methods:
  - Remove rows with missing values
  - Fill with constant value (or mean/median/mode)
  - Linear/spline/nearest neighbor interpolation
- Remove duplicate entries (with options to keep first/last occurrence)
- Apply transformations:
  - Standardization (z-score)
  - Normalization (0-1)
  - Log transformation
  - One-hot encoding for categorical variables
- Handle outliers based on z-score threshold
- Text preprocessing options:
  - Tokenization
  - Remove stopwords (e.g. "the", "and", "is")
  - Convert to lowercase
  - Remove punctuation (e.g. ".", ",", "!", "?")
  - Spell correction (using the `hunspell` package, may take a long time)

### Feature Engineering
- Mathematical operations between columns
- Binning (equal width or equal frequency)
- Date component extraction
- Aggregation features with various methods (mean, median, mode, sum, stdev)
- Visualize feature impact through various plots

### Exploratory Data Analysis (EDA)
- Univariate Analysis:
  - Histograms
  - Box plots
  - Density plots
  - Bar charts
- Bivariate Analysis:
  - Scatter plots
  - Line charts
  - Box plots
  - Bar charts
  - Heatmaps
- Correlation Analysis (using the `corrplot` package):
  - Pearson
  - Spearman
  - Kendall
- Interactive data filtering

## Installation and Setup

### Required R Packages
```r
install.packages(c(
  "shiny", "DT", "plotly", "ggplot2", "dplyr", "tidyr", "stringr",
  "readr", "readxl", "jsonlite", "shinydashboard", "corrplot",
  "tm", "hunspell", "zoo", "viridis"
))
```

### Running the Application
1. Clone this repository
2. Open the `app.R` file in RStudio
3. Click "Run App" or run `shiny::runApp()`

## Project Structure

```
├── app.R             # Main application file with UI and server logic
├── README.md         # This documentation file
└── Project 2 Report.pdf     # Report for Project 2 by our team
```

## Usage Guide

1. **Start by loading data**: Choose from built-in datasets (Iris, US Economic Time Series, US Crime Statistics) or upload your own file
2. **Clean your data**: Handle missing values, duplicates, outliers, and apply transformations
3. **Engineer new features**: Create calculated fields, bins, or extract date components
4. **Explore your data**: Generate visualizations and statistical summaries
5. **Filter and analyze**: Use the interactive controls to drill down into your data