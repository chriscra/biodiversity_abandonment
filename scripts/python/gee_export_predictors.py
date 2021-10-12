#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 24 23:24:27 2021

@author: christophercrawford

Export multiple datasets for multiple sites, in batch
"""

import ee
ee.Initialize()


# define my small region for testing:
small = ee.Geometry.Rectangle(
  109.17987976074215, 35.87848663551732, 
  109.23275146484372, 35.92131496907426
  );


# # load sites, to iterate across
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


# # // Make a disctionary of sites Features.
sites = {
    'images': [belarus, bosnia_herzegovina, chongqing, goias,
               iraq, mato_grosso, nebraska, orenburg,
               shaanxi, volgograd, wisconsin],
    'labels': ["_b", "_bh", "_c", "_g", "_i", "_mg", 
               "_n", "_o", "_s", "_v", "_w"]
}


# # // Create a FeatureCollection from the list and add it to the map.
# site_locations = ee.FeatureCollection(sites.images)

# // ---------------------------------------
# // load predictor datasets
# // ---------------------------------------

# // 1. Elevation and terrain data
srtm = ee.Image("USGS/SRTMGL1_003")

# // 2. Soils
# // organic carbon stock, topsoil (0-30 cm)
soil_ocs_mean = ee.Image("projects/soilgrids-isric/ocs_mean")

soil_cec_mean = ee.Image("projects/soilgrids-isric/cec_mean")
soil_ph_mean = ee.Image("projects/soilgrids-isric/phh2o_mean")
# // sum or mean across topsoil, defined as 0-30 cm (bands 1, 2, and 3)

# // ----------------
# // 3. Population data
pop_density_col = ee.ImageCollection("CIESIN/GPWv411/GPW_Population_Density")
pop_density = pop_density_col.toBands()

pop_count_col = ee.ImageCollection("CIESIN/GPWv411/GPW_Population_Count")
pop_count = pop_count_col.toBands()


# // note that these basic demographic characteristics are from the 2010 census.
gpw_basic_dem_col = ee.ImageCollection("CIESIN/GPWv411/GPW_Basic_Demographic_Characteristics")

gpw_basic_dem_all = gpw_basic_dem_col.toBands()


gpw_bands_to_select = [0, 6, 9, 12, 21, 24, 27, 30, 33, 
    36, 39, 42, 45, 48, 54, 60, 66, 72,
    18, 74]

gpw_basic_dem = gpw_basic_dem_all.select(gpw_bands_to_select)


# // 4. travel times:
travel_time_2015 = ee.Image("Oxford/MAP/accessibility_to_cities_2015_v1_0")


# // make variables dictionary

predictors = {
  'images': [
      srtm, 
      soil_ocs_mean, soil_cec_mean, soil_ph_mean, 
      pop_density, pop_count, #gpw_basic_dem,
      travel_time_2015
      ],
    
  'names': [
      'srtm', 
      'soil_ocs_mean', 'soil_cec_mean', 'soil_ph_mean',
      'pop_density', 'pop_count', #'gpw_basic_dem',
      'travel_time_2015'
      ],
    
  'resolutions': [ # in meters, for export
                  30, # // srtm,
                  250, #// soil_ocs_mean,
                  250, #// soil_cec_mean
                  250, #// soil_ph_mean,
                  1000, #// pop_density,
                  1000, #// pop_count,
                  1000, #// gpw_basic_dem,
                  1000 #// travel_time_2015 // 30 arc seconds ~ 1000 m
                  ]
  }

## only needed for the geetools method saving a ImageCollection
# variables_collection = ee.ImageCollection(predictors['images'])
# print("variables_collection", variables_collection)

# // ----------------
# // export predictor datasets
# // ----------------

for site_index in range(0,11):
    # site_index = 7
    for predictor_index in range(0,8):
        
        print('exporting:', predictors['names'][predictor_index] + sites['labels'][site_index])
        
        task_config = {
            'scale': 30,       
            'region': sites['images'][site_index].geometry(), # small # for testing
            'folder': 'predictors',
            'crs': 'EPSG:4326', 
            'maxPixels': 120000000
            }
        
        task = ee.batch.Export.image.toDrive(
            image = predictors['images'][predictor_index],
            description = predictors['names'][predictor_index] + sites['labels'][site_index],
            **task_config)
        
        task.start()
        # print(task.status())



# just for exporting gpw, after subsetting:
for site_index in range(0,11): 
    predictor_index = 6
    site_index = 7
    
    print('exporting:', predictors['names'][predictor_index] + sites['labels'][site_index])
        
    task_config = {
        'scale': 30,       
        'region': sites['images'][site_index].geometry(), # small # for testing
        'folder': 'predictors',
        'crs': 'EPSG:4326',
        'maxPixels': 120000000
        }
    
    task = ee.batch.Export.image.toDrive(
        image = predictors['images'][predictor_index],
        description = predictors['names'][predictor_index] + '_subset' + sites['labels'][site_index],
        **task_config)
    
    task.start()
    # print(task.status())
    
    
# for testing

# task_config = {
#             'scale': 30,       
#             'region': small, # sites['images'][site_index]
#             'folder': 'predictors',
#             'crs': 'EPSG:4326'
#             }


# task = ee.batch.Export.image.toDrive(
#     image = srtm, 
#     description = 'TEST_todrive3', 
#     **task_config)
# task.start()    

# this is with the geetools batch, which I actually don't think is that great
# # Set parameters
# bands = ['B2', 'B3', 'B4']
# scale = 30
# name_pattern = '{sat}_{system_date}_{WRS_PATH:%d}-{WRS_ROW:%d}'
# ## the keywords between curly brackets can be {system_date} for the date of the
# ## image (formatted using `date_pattern` arg), {id} for the id of the image
# ## and/or any image property. You can also pass extra keywords using the `extra`
# ## argument. Also, numeric values can be formatted using a format string (as
# ## shown in {WRS_PATH:%d} (%d means it will be converted to integer)
# date_pattern = 'ddMMMy' # dd: day, MMM: month (JAN), y: year
# folder = 'MYFOLDER'
# data_type = 'uint32'
# extra = dict(sat='L8SR')
# region = site

# # ## Export
# tasks = geetools.batch.Export.imagecollection.toDrive(
#             collection = variables_collection,
#             folder = folder,
#             region = site,
#             namePattern = name_pattern,
#             scale = scale,
#             dataType = data_type,
#             datePattern = date_pattern,
#             extra = extra,
#             verbose = True,
#             maxPixels = int(1e13)
#         )





