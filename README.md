# Bikeshare and Weather Analysis
# Alexa Rossello

## Overview
This project analyzes bikeshare and weather data to assess how different weather factors (temperature and rain) impact daily rides for different rider types (casual vs. member). The analysis involves data preprocessing, exploratory data analysis (EDA), statistical modeling, and machine learning techniques such as random forest and negative binomial regression. The final report presents findings and insights to inform bikeshare usage patterns.


## Prerequisites
- **R packages** (install using the following command in R):
  ```r
  install.packages(c('tidyverse', 'dplyr', 'ggplot2', 'randomForest', 'e1071', 'MASS', 'car', 'FSA'))
  
- **LaTeX compiler**:
  MiKTeX or TeX Live


## Repository Structure

     project/
     ├── README.md
     ├── bikeshare-analysis.Rproj
     ├── .Rhistory
     ├── data/
     │   ├── 202302-captialbikeshare-tripdata.csv
     │   ├── 202303-captialbikeshare-tripdata.csv
     │   ├── 202304-captialbikeshare-tripdata.csv
     │   ├── 202305-captialbikeshare-tripdata.csv
     │   ├── 202306-captialbikeshare-tripdata.csv
     │   ├── washington dc 2023-01-01 to 2023-12-31.csv
     ├── script/
     ├── ├── Bikeshare Project.R
     ├── report/
     │   ├── Bikeshare Analysis-Final Report.pdf
     │   ├── Bikeshare Analysis-Final Report.tex
     │   ├── references.bibtex 
     │   ├── PNG files for LaTeX
     

## Dataset Information
- The dataset used for this project was sourced from Kaggle.
- License information for the dataset is unknown and therefore is not included in this repository.
- For more information, visit the Kaggle dataset page: [Dataset Link](https://www.kaggle.com/datasets/patrickzel/capital-bikeshare-trip-data-february-june-2023)


## Report
The final report is available as a PDF and can be found here:

[Download the final report](report/Bikeshare Analysis-Final Report.pdf)

If you wish to modify the report, the LaTeX source files (`.tex` and `.bib`) are available in the `report/` folder.
