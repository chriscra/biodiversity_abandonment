#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 15 10:53:14 2021

@author: christophercrawford


/* Script to visualize and export data from Crouzeilles et al. 2019
https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/1365-2664.13501

Variation in Forest Restoration Success
(a measure of how likely it is for a pixel to regenerate successfully)
*/ 
"""

import ee
ee.Initialize()


var_forest_restoration_success = ee.Image("users/HotSpotRestoration/Landscape_variation_FLR_succes")

# // Site locations for exporting data

belarus = ee.Image('users/chriscra/abandonment/input_rasters/belarus')
bosnia_herzegovina = ee.Image('users/chriscra/abandonment/input_rasters/bosnia_herzegovina')
chongqing = ee.Image('users/chriscra/abandonment/input_rasters/chongqing')
goias = ee.Image('users/chriscra/abandonment/input_rasters/goias')
iraq = ee.Image('users/chriscra/abandonment/input_rasters/iraq')
mato_grosso = ee.Image('users/chriscra/abandonment/input_rasters/mato_grosso')
nebraska = ee.Image('users/chriscra/abandonment/input_rasters/nebraska')
orenburg = ee.Image('users/chriscra/abandonment/input_rasters/orenburg')
shaanxi = ee.Image('users/chriscra/abandonment/input_rasters/shaanxi')
volgograd = ee.Image('users/chriscra/abandonment/input_rasters/volgograd')
wisconsin = ee.Image('users/chriscra/abandonment/input_rasters/wisconsin')


# // Make a dictionary of sites Features.
sites = {
    'images': [belarus, bosnia_herzegovina, chongqing, goias,
               iraq, mato_grosso, nebraska, orenburg,
               shaanxi, volgograd, wisconsin],
    'labels': ["_b", "_bh", "_c", "_g", "_i", "_mg", 
               "_n", "_o", "_s", "_v", "_w"]
}


# // make variables dictionary

predictors = {
  'images': [
      var_forest_restoration_success
      ],
    
  'names': [
      'var_forest_restoration_success'
      ]
  }

# // return the native resolution
var_scale = var_forest_restoration_success.projection().nominalScale()
print('My scale in meters', var_scale)

# just for exporting gpw, after subsetting:
for site_index in range(0,11): 
    predictor_index = 0
    
    print('exporting:', predictors['names'][predictor_index] + sites['labels'][site_index])
    print('the native scale is: ', predictors['images'][predictor_index].projection().nominalScale(), ' meters')
        
    task_config = {
        'scale': predictors['images'][predictor_index].projection().nominalScale(), # use the native resolution for each layer  
        'region': sites['images'][site_index].geometry(), # small # for testing
        'folder': 'var_for_success',
        'crs': 'EPSG:4326',
        'maxPixels': 1000000000000
        }
    
    task = ee.batch.Export.image.toDrive(
        image = predictors['images'][predictor_index],
        description = predictors['names'][predictor_index] + '_native' + sites['labels'][site_index],
        **task_config)
    
    task.start()
    # print(task.status())
    
    