#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 26 18:33:44 2021

@author: christophercrawford
"""

import ee
ee.Initialize()

# define image
srtm = ee.Image("USGS/SRTMGL1_003")

# define my small region for testing:
small = ee.Geometry.Rectangle(
  109.17987976074215, 35.87848663551732, 
  109.23275146484372, 35.92131496907426
  );


task_config = {
    'scale': 30,  
    'region': small,
    'folder': 'predictors'
}

task = ee.batch.Export.image.toDrive(srtm, 'TEST_todrive', **task_config)
task.start()