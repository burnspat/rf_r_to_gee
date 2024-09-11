// SCRIPT METADATA ==========================================================================================
// Description: Pseudo-code for reading in and applying model in Google Earth Engine
// Script Author: Kathleen Orndahl
// Script Date: 2024-09-10

// PROBABILITY ==========================================================================================

var response_type = 'probability';

// Get model string from Google Storage
var mod_str = ee.List([ee.Blob('gs://models/mod_' + response_type + '.txt').string()]);

// Convert to GEE classifier
var mod = ee.Classifier.decisionTreeEnsemble(mod_str).setOutputMode('REGRESSION');

// Apply model to predictors
var results = predictors.classify(mod, 'predicted');

// CLASSIFICATION ==========================================================================================

var response_type = 'classification';

// Get model string from Google Storage
var mod_str = ee.List([ee.Blob('gs://models/mod_' + response_type + '.txt').string()]);

// Convert to GEE classifier
var mod = ee.Classifier.decisionTreeEnsemble(mod_str).setOutputMode('CLASSIFICATION');

// Apply model to predictors
var results = predictors.classify(mod, 'predicted');

// REGRESSION ==========================================================================================

var response_type = 'regression';

// Get model string from Google Storage
var mod_str = ee.List([ee.Blob('gs://models/mod_' + response_type + '.txt').string()]);

// Convert to GEE classifier
var mod = ee.Classifier.decisionTreeEnsemble(mod_str).setOutputMode('REGRESSION');

// Apply model to predictors
var results = predictors.classify(mod, 'predicted');
