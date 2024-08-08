# Format Ranger Random Forest Models for Google Earth Engine

## Project summary

Google Earth Engine (GEE) is a cloud-based platform that allows researchers access to an unprescedented archive of remotely sensed data, as well as the computing power and infrastructure to perform large scale geospatial analysis. As such, GEE has become indispensible for ecological research. However, GEE is not optmized to work with vector data and has limited (albeit growing) options for training and validating machine learning models.

In contrast, R has been used by ecological researchers for decades and has a robust ecosystem of packages and resources for training, tuning and applying machine learning models, including the 'tidymodels' framework.

Thus, researchers often find themselves needing to move between R and GEE over the course of modeling workflow. For example, a researcher might capitalize on GEE's raster processing power to extract predictor variables across a set of training data, then bring the extracted data into R to train and validate a series of models. Ideally, these models could then be applied in GEE across the full raster domain. However, a common sticking point is formatting R-based models in a way that is readable by GEE's javascript based interface. 

This repository includes scripts and sample data to take random forest models fitted via the ranger package in R, and convert them into the string format required for import into GEE. The code presented here is modified from the 'reprtree' package available here: https://github.com/araastat/reprtree/tree/master

## How to use

1. Gather files

The script '0_format_ranger_for_gee.R' performs the random forest conversion. To run this script, users will need two objects:
- A random forest model fit using the 'ranger' package in R, saved as an .rds file. For models fitted using tidymodels, extract the engine specific model specification before saving:
```
parsnip_mod = final_mod %>% extract_fit_parsnip()
engine_mod = parsnip_mod$fit
saveRDS(engine_mod, 'mod.rds')
```
- The training data used to fit the model, saved as a .csv file

2. Set parameters

In the script, set parameters to your specification:
- response_type: Choose 'classification' or 'regression'
- response_var: Specify the name of the response variable
- shortened: Specify whether to use shortened tree format. Forests imported into GEE are subject to a character limit. Using shortened format can prevent errors arising from exceeding this character limit.

3. Run script 

The output will be a .txt file with the random forest model formatted for input into GEE.

4. Import into Google Earth Engine

Script '1_apply_model_in_gee.R' contains javascript pseduo-code for importing the model into GEE and applying it across a stack of predictor variables.
