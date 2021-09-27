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

01_model_5fold_monthly.R runs each model using all monthly data from a year.
01_model_5fold_monthly_sep.R runs each model separately using data from every month.

--- the following neeeds to be rerun because i reran the first two scripts for SLR!
01_output_prediction_test_cv.R combines the validation dataset into one csv file.
02_vis_5fold.R visualizes the 5-fold CV results (for both models for each month and models for all 12 months).
02_vis_5fold.Rmd visualizes the 5-fold CV results (for both models for each month and models for all 12 months).
### Function code
00_fun_reaed_monthly_data_gee.R contains the 'read_data' function that reads monthly predictors for each pollutant and year(s).
fun_output_rf.R outputs RF predictions and variable importance as csv files.
fun_rf_rf.R trains RF models.

## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
