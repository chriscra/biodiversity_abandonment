# -*- coding: utf-8 -*-
""" Auxiliary functions needed to calculate AOH

Created on Thu Jun  4 09:52:03 2020
@author: rsenior
"""
import ee
ee.Initialize()

def simpFt(ft, maxError):
    """Simplify features"""
    # Get properties
    binom = ft.get('binom')
    pres = ft.get('pres')
    origin = ft.get('origin')
    season = ft.get('season')
    # Simplify
    simple = ee.Feature(ft.geometry().simplify(maxError))\
        .set('binom', binom)\
        .set('pres', pres)\
        .set('origin', origin)\
        .set('season', season)
    # Return
    return(simple)

def geoType(ft):
    """Assign geometry type"""
    geotype = ee.Geometry(ft.geometry()).type()
    return(ft.set('geotype', geotype))

def badGeom(geom):
    """Deal with bad geometries"""
    # Define this geometry type
    geotype = ee.List([ee.Geometry(geom).type()])
    # Define bad geometry types
    bad = ee.List(['Point', 'MultiPoint', 'LineString', 
                   'MultiLineString', 'LinearRing'])
    
    # Replace with 0 if geometry is bad
    geotypeFilter = geotype.filter(ee.Filter.inList('item', bad))
    result = ee.Algorithms.If(geotypeFilter.size().gt(0), 0, geom)
    return(result)

def maskLC(img, lcCodes):
    """Mask land cover map to suitable lc categories"""
    masked = img.eq(ee.Image.constant(lcCodes))\
        .reduce(ee.Reducer.anyNonZero())\
        .copyProperties(img)
    return(masked)