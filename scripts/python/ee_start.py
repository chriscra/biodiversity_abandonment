#!/usr/bin/env python3
# -*- coding: utf-8 -*-
""" Linking ee and python
Created on Thu Jul  9 16:17:45 2020

@author: christophercrawford
"""
import ee

ee.Authenticate()

ee.Initialize()


test = 'does it work?'
print(test)


# define my small region for testing:
shaanxi = ee.Image('users/chriscra/abandonment/input_rasters/shaanxi')



print('shaanxi')
