# -*- coding: utf-8 -*-
""" Calculate species Area of Habitat

Created on Thu Jun  4 09:58:36 2020
@author: rsenior

Keyword arguments:
    spRange -- the species range (ee.Feature)
    lc -- land cover data (ee.Image)
    elevpath -- path to the elevation data (ee.String)
    lcCodes -- suitable land cover codes (ee.List)
    elevLo -- elevation lower threshold (ee.Number)
    elevHi -- elevation upper threshold (ee.Number)
"""
import ee
ee.Initialize()
import AOHaux as aux

def getAOH(spRange, lc, elevpath, lcCodes, elevLo, elevHi):
    """Calculate AOH from habitat & elevation prefs"""    
    # Clip land cover to species range
    #lcClip = lc.map(function(img){return(img.clip(spRangeUnion.geometry()))})
    lcClip = lc.clipToBoundsAndScale(spRange.geometry())
    
    # Mask land cover & clip to species range
    lcMask = ee.Image(aux.maskLC(lcClip, lcCodes))\
        .clip(spRange.geometry())
        
    # Mask unsuitable areas (with value of 0)
    lcMaskBi = lcMask.mask(lcMask.eq(1))
    
    # Read in elevation
    elev = ee.Image(elevpath)
    # Clip elevation to species range
    elevClip = elev.clip(spRange.geometry())
    
    # Mask elevation map to suitable elevation range
    elevLoHiMask = elevClip.mask(elevClip.gte(elevLo))\
        .updateMask(elevClip.mask(elevClip.lte(elevHi)))
    # Make binary
    elevMask = elevLoHiMask.gt(0)
    
    # Mask cells in range with suitable lc to to also have suitable elevation
    aohMask = lcMaskBi.updateMask(elevMask)
    
    # Replace with zero image if AOH is empty
    aoh = ee.Image(ee.Algorithms.If(aohMask,aohMask,ee.Image(0)))
    # Return
    return(aoh)

def toCollection(img,band):
    """Convert image to image colelction with one image per band"""
    # Select one band at a time, add name as property & rename
    newImg = img.select(ee.String(band)).set('yearID', band).rename('lc')
    return(newImg)

def getSpRange(species, allRange, simplify):
    """Get range features for focal species
    
    (1) Load all ranges 
    (2) Filter to focal species
    (3) Filter to presence: Extant, Probably Extant, Possibly Extinct, Extinct and Presence Uncertain
    (4) Filter to origin: Native, Reintroduced
    (5) Simplify 
    (5) Add geometry type
    """
    spRange = ee.FeatureCollection(allRange)\
        .filter(ee.Filter.eq('binom', species))\
        .filter(ee.Filter.inList('pres', ee.List([1, 2, 4, 5, 6])))\
        .filter(ee.Filter.inList('origin', ee.List([1,2])))\
        .map(lambda ft: aux.simpFt(ft, simplify))\
        .map(lambda ft: aux.geoType(ft))
    return(spRange)

def fixGeom(ftColl):
    """Fix geometry of a feature collection"""
    geotypes = ee.List(ftColl.aggregate_array('geotype'))
    multitypes = geotypes.filter(
            ee.Filter.inList('item', ee.List(['GeometryCollection',
                                              'MultiPolygon'])))
    
    # Break each feature into constituent geometries
    polyFt = ee.List(
            ee.Algorithms.If(geotypes.contains('Polygon'),
                             [ftColl.filter(ee.Filter.eq('geotype', 'Polygon'))\
                                  .geometry()],
                             # Empty list if there are no Polygons
                             []))
    
    polyFtFinal = ee.List(
            # If there are polygons
            ee.Algorithms.If(polyFt.length().gt(0),
                             ee.Algorithms.If(
                                     # If there are multiple polygon features, 
                                     # creates a multipolygon that needs to be 
                                     # broken apart
                                     ee.List([ee.Geometry(polyFt.get(0)).type()])\
                                         .contains('MultiPolygon'),
                                     ee.Geometry(polyFt.get(0)).geometries(),
                                    # Otherwise, return input
                                     [ee.Geometry(polyFt.get(0))]),
            # Empty list if there are no Polygons
            []))
    multiFt = ee.List(
            ee.Algorithms.If(multitypes.size().gt(0),
                             ftColl\
                             .filter(ee.Filter.neq('geotype', 'Polygon'))\
                             .geometry()\
                             .geometries()\
                             .map(lambda geom: aux.badGeom(geom))\
                             .filter(ee.Filter.neq('item', 0)),
                             # Empty list if there are no GeomColls/MultiPolygons
                             []))       
    # Combine
    ftCollFix = ee.FeatureCollection(
            ee.Feature(ee.Geometry.MultiPolygon(polyFtFinal.cat(multiFt))))    
    # Return
    return(ftCollFix)

def matchPrefs(sp, habitatPrefs, codeField, season, suitability, major):
    """Retrieve species habitat prefs"""
    # Filter to focal species
    spHabitatPrefs1 = habitatPrefs.filter(ee.Filter.eq('scientificName',sp))
    # Filter by seasonality, suitability and major importance
    spHabitatPrefs2 = ee.FeatureCollection(
            ee.Algorithms.If(season,
                             spHabitatPrefs1.filter(ee.Filter.eq('habitat_season', season)),
                             spHabitatPrefs1))
    spHabitatPrefs3 = ee.FeatureCollection(
            ee.Algorithms.If(season,
                             spHabitatPrefs2.filter(ee.Filter.eq('habitat_suitability', suitability)),
                             spHabitatPrefs2))
    spHabitatPrefs4 = ee.FeatureCollection(
            ee.Algorithms.If(season,
                             spHabitatPrefs3.filter(ee.Filter.eq('habitat_suitability', major)),
                             spHabitatPrefs3))
    
    # Get land cover codes
    lcCodes = spHabitatPrefs4.aggregate_array(codeField)\
        .filter(ee.Filter.neq('item','NA'))\
        .join(';')\
        .split(';')\
        .distinct()
        
    # Get upper and lower elevation limits
    elevLoOrig = spHabitatPrefs4.aggregate_array('elev_low')
    elevHiOrig = spHabitatPrefs4.aggregate_array('elev_hi')
    
    # If lower elevation limit is NA, change to 0
    elevLo = ee.Number(ee.Algorithms.If(elevLoOrig.contains('NA'),
                                        0, ee.Number.parse(elevLoOrig.get(0))))
    # If upper elevation limit is null, change to 10000
    elevHi = ee.Number(ee.Algorithms.If(elevHiOrig.contains('NA'),
                                        10000, ee.Number.parse(elevHiOrig.get(0))))
    
    spHabitatPrefs = ee.Dictionary(
            {'lcCodes': lcCodes,
             'elevLo': elevLo,
             'elevHi': elevHi})
    return(spHabitatPrefs)

def areaOverlap(img, geom, scale):
    """Calculate overlap between image and polygon"""
    # Sum the values of suitable habitat area
    pxArea = ee.Number(img\
                       .multiply(ee.Image.pixelArea())\
                       .reduceRegion(**{'reducer': ee.Reducer.sum(),
                                      'geometry': geom,
                                      'scale': scale,
                                      'maxPixels': 1e13,
                                      'tileScale': 8})\
                        .values()
                        .get(0))
    return(ee.Feature(None, {'pxArea':pxArea}))

def countIsl(intersection, isles):
    """Count islands"""
    saveAllJoin = ee.Join.saveAll(**{
      'matchesKey': 'scenes',
    })
    intersectFilter = ee.Filter.intersects(**{
      'leftField': '.geo',
      'rightField': '.geo',
      'maxError': 100
    })
    # Join intersection to islands 
    join = saveAllJoin.apply(intersection, isles, intersectFilter)
    # Count features
    noIsles = ee.FeatureCollection(ee.List(join.first().get('scenes'))).size()
    return(noIsles)

def toFt(img, geom, scale):
    """Convert image to polygons"""
    ft = ee.Feature(img.reduceToVectors(
            **{'geometry': geom.geometry(),
               'scale': scale,
               'maxPixels': 1e13})\
        .union(ee.ErrorMargin(1))\
        .first()\
        .copyProperties(img))
    return(ft)