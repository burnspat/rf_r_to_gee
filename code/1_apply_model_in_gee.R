# SCRIPT METADATA ==========================================================================================
# Description: Pseudo-code for reading in and applying model in Google Earth Engine
# Script Author: Kathleen Orndahl
# Script Date: 2023-05-03

# CLASSIFICATION ==========================================================================================

var response_type = 'classification';

# Get model string from Google Storage
var mod_str = ee.List([ee.Blob('gs://models/mod_' + response_type + '.txt').string()]);

# Convert to GEE classifier
var mod = ee.Classifier.decisionTreeEnsemble(mod_str).setOutputMode('RAW');

# Apply model to predictors
# The results here are an array image where each pixel is a 1-D array of size 1 x ntree
# Each value in the array represents a probability estimate from a tree in the forest
var results = predictors.classify(mod, 'raw_probability');

# Get probability of presence
# Because we are using a probability forest rather than a classification forest, we average the probabilities across the trees to get a final probability estimate
var prob_presence = results.arrayReduce(ee.Reducer.mean(), ee.List([0])).arrayGet(0).rename('prob_presence');

# Assign presence/absence
# Select and apply a user determined threshold to produce presence/absence or class data
var presence_absence = prob_presence.gte(threshold).rename('presence').byte();

# REGRESSION ==========================================================================================

var response_type = 'regression';

# Get model string from Google Storage
var mod_str = ee.List([ee.Blob('gs://models/mod_' + response_type + '.txt').string()]);

# Convert to GEE classifier
var mod = ee.Classifier.decisionTreeEnsemble(mod_str).setOutputMode('REGRESSION');

# Apply model to predictors
var results = predictors.classify(mod, 'predicted');
