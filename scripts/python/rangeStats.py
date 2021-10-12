# -*- coding: utf-8 -*-
""" Calculate species range statistics

Created on Thu Jun  4 10:25:31 2020
@author: rsenior

Keyword arguments:
    species -- the focal species (ee.String)
    sppRange -- all species ranges (ee.FeatureCollection)
    lc -- land cover data (ee.Image)
    lcField -- the name of the field denoting suitable land cover codes (ee.String)
    habitatPrefs -- the habitat preference data (ee.Feature)
    elevpath -- path to the elevation data (ee.String)
    season -- the 'season' of range data to use; use 'None' to include all (ee.String)
    suitability -- the 'suitability' of habitat to include; use 'None' to include all (ee.String)
    major -- the 'importance' of habitat to include; use 'None' to include all (ee.String)
"""
import ee
ee.Initialize()
import AOH
import AOHaux as aux

def rangeStats(species,sppRanges,lc,lcField,habitatPrefs,elevpath,season,suitability,major):
    # Species range -----------------------------------------------------------
    # Get focal species range
    spRangeAll = AOH.getSpRange(species, sppRanges, 1e2)
    
    # Union of full range (extant and extinct, for AOH)
    spRangeUnion = ee.Feature(AOH.fixGeom(spRangeAll).union().first())
    bbox = spRangeUnion.geometry().bounds()
    bboxStr = bbox.coordinates().join(";")
    
    # Extant ranges (presence = extant or probably extant)
    spRangeExtant = spRangeAll.filter(ee.Filter.inList('pres', ee.List([1, 2])))
    spRangeExtantUnion = ee.Feature(AOH.fixGeom(spRangeExtant).union().first())
    # Extant range area
    rangeArea = ee.Number(ee.Algorithms.If(
            spRangeExtant.size().eq(0),0, spRangeExtantUnion.area()))
    
    # Set scale to 5 km
    spScale = 5e3
    # Set scale depending on range size - either 1, 5 or 10 km
    #spScale = ee.Number(
    #ee.Algorithms.If(rangeArea.lte(10e9), 1e3,
      #ee.Algorithms.If(rangeArea.gt(10e9) && rangeArea.lte(10e11), 
        #5e3, 10e3)))
  
    # Reintroduced range (origin = reintroduced)
    spRangeReintro = ee.Feature(spRangeAll.filter(ee.Filter.eq('origin', 2))\
                                .union()\
                                .first()\
                                .copyProperties(spRangeAll))
    spReintroduced = ee.Algorithms.If(spRangeReintro.area().neq(0), True, False)
    
    # Get habitat prefs -------------------------------------------------------
    
    # Use species name to get list of suitable land cover codes
    spHabitatPrefs = AOH.matchPrefs(
            species,habitatPrefs,lcField,season,suitability,major)
    lcCodesFull = ee.List(
            spHabitatPrefs\
                .get('lcCodes'))\
                .map(lambda i: ee.Algorithms.If(i,ee.Number.parse(i)))
    # Remove null values
    lcCodes = lcCodesFull.filter(ee.Filter.neq('item', None))
    # Flatten to return as string
    lcCodesStr = lcCodes.flatten().join(";")
    
    # Get elevation
    elevLo = ee.Number(spHabitatPrefs.get('elevLo'))
    elevHi = ee.Number(spHabitatPrefs.get('elevHi'))
    
    # Protected Areas ---------------------------------------------------------    
    # Load & clip cleaned WDPA
    wdpa = ee.Image('users/rasenior/wdpa')\
        .clipToBoundsAndScale(bbox)\
        .gt(0)
        
    # Identify different seasonal range types
    rangeTypes = ee.List(spRangeAll.aggregate_array('season')).distinct()
    
    # Summarise range area inside PA for each range type
    areaRangeinPA = rangeTypes\
        .map(lambda typ: ee.Number(
                AOH.areaOverlap(wdpa,
                                spRangeAll.filter(ee.Filter.eq('season', typ)), 
                                spScale)\
                                .get('pxArea')))
                
    # Collapse into string
    rangeTypesStr = rangeTypes.join(";")
    areaRangeinPAStr = areaRangeinPA.join(";")
    
    # Identify island species -------------------------------------------------
    
    # Clip Global Island Database
    gid = ee.FeatureCollection('users/rasenior/GID')\
        .filterBounds(bbox)\
        .map(lambda ft: aux.simpFt(ft, 1e2))
        
    # Intersect species range with island polygons
    islandIntersect = spRangeUnion\
        .geometry()\
        .intersection(gid, ee.ErrorMargin(1))
    # Calculate area of range on islands
    islandArea = islandIntersect.area()
    
    # Count number of islands
    #noIsle = ee.Algorithms.If(
    #islandArea.gt(0), 
    #AOH.countIsl(islandIntersect, gid), 0)
  
    # AOH ---------------------------------------------------------------------
    
    # Get AOH for focal species
    aoh = AOH.getAOH(spRangeUnion, lc, elevpath, lcCodes, elevLo, elevHi)
    
    # Add wdpa and AOH & make binary (value of 2 -> 1, all else -> 0)
    aohWDPA = ee.Image(aoh.add(wdpa).gt(0))
    
    # Calculate area of overlap
    areaAOHinPA = AOH.areaOverlap(aohWDPA, bbox, spScale).get('pxArea')
    
    # Convert to polygons for export
    aohPolys = ee.Feature(
            AOH.fixGeom(ee.FeatureCollection(
                    aux.geoType(ee.Feature(AOH.toFt(aoh, spRangeUnion, spScale)))))\
                    .first())
            
    # AOH area
    aohArea = ee.Number(aohPolys.area())
    
    # Gather results
    final = aohPolys\
        .set('species', species)\
        .set('spScale', spScale)\
        .set('bboxStr', bboxStr)\
        .set('lcCodes', lcCodesStr)\
        .set('elevLo', elevLo)\
        .set('elevHi', elevHi)\
        .set("rangeArea", rangeArea)\
        .set("PAtype",rangeTypesStr)\
        .set("rangePA",areaRangeinPAStr)\
        .set("spReintro", spReintroduced)\
        .set("aohArea", aohArea)\
        .set("aohPA", areaAOHinPA)\
        .set("islArea", islandArea)
    return(final)