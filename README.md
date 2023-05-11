# expanse_monthly

Version 0.1.0

A short description of your project


## Project organization

```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```


## Source code
- **01_model_5fold_monthly_sep.R** trains and evaluates LUR models for every month using 5-fold cross-validation. It uses two algorithms to train the LUR models: supervised linear regression (SLR) and random forests (RF). 
- **01_model_5fold_monthly_sep_gwr.R** trains LUR models for every month using 5-fold cross-validation. It uses geographically weighted regression (GWR) to train the LUR models. 
- **01_output_prediction_test_cv.R** combines all hold-out validation data from the 5-fold CV.

- **02_model_all_monthly_sep.R** trains LUR models for every month using all available observations. It uses two algorithms to train the LUR models: supervised linear regression (SLR) and random forests (RF). 
- **02_model_all_monthly_sep_gwr.R** trains LUR models for every month using all available observations. It uses geographically weighted regression (GWR) to train the LUR models. 
-- **02_vis_randomPoints_IDW.R**: processes and combines csv files containing estimates from several models at random points into one rds file (from predictionsAll_monthlyRandom_idwAndannualComparison_POLL_blockXXXXX.csv to randomPointsAll_cleaned.rds). It also creates heatmap plot of indicating the correlation values between the model estimates at these random points. 


### Function code
- **00_fun_reaed_monthly_data_gee.R** contains the 'read_data' function that reads monthly predictors for each pollutant and year(s).
- **fun_output_rf.R** outputs RF predictions and variable importance as csv files.
- **fun_rf_rf.R** trains RF models.



## Data
- Files **/data/raw/gee/predictionsAll_monthlyRandom_idwAndannualComparison_POLL_blockXXXXX.csv** contain the estimates from different models (annual GTWR, monthly GWR, monthly adjusted GTWR using two inverse distance weighting methods) at random points.
- The processed and combined file from the abovementioned files is **data/processed/randomPointsAll_cleaned.rds**

## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
