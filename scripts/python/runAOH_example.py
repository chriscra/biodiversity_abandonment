# -*- coding: utf-8 -*-
""" Example of how to calculate range stats, including AOH

Created on Thu Sep 26 13:36:16 2019
@author: rsenior
"""
import ee
import ee.mapclient
ee.Initialize()
import rangeStats as rs

# Habitat pref data
habitatPrefs = ee.FeatureCollection('users/rasenior/IUCN-data/habitat_prefs')

# Species ranges
sppRanges = ee.FeatureCollection('users/rasenior/IUCN-range/bird_valid1')

# Land cover data (for 2015)
lc = ee.Image('users/rasenior/LandCover/cci_landcover').select(23)

# Elevation data
elevPath = 'users/rasenior/LandCover/EarthEnv-DEM90_wgs84_1km'

# Run for echo parakeet
echoParakeet = rs.rangeStats(
  'Psittacula eques',
  sppRanges,
  lc,
  'cci_code',
  habitatPrefs,
  elevPath,
  None,None,None)

# Add to the map
ee.mapclient.centerMap(57.5864, -20.32, 10)
ee.mapclient.addToMap(echoParakeet, {'color': 'turquoise'})

# Export
task = ee.batch.Export.table.toDrive(**{
        'collection': ee.FeatureCollection([echoParakeet]),
        'description': 'echoParakeet',
        'fileFormat': 'SHP'})
task.start()