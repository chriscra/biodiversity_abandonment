# --------------------------------------------------------------- #
#
# Spatial functions
# 
# --------------------------------------------------------------- #

# AOH ----

# ------------------------------------------------------------------------- #
### AOH data.table function starts here ###
# ------------------------------------------------------------------------- #
cc_AOH_data.table <- 
  function(index,
           site_index,
           year_index, # the real year(s)
           calc_lc = TRUE, # switch between area in "lc" (Yin et al.) vs. "map_code" (IUCN)
           include_time = FALSE, 
           major_importance_only = FALSE, # calc. area of only habitats coded as "majorImportance"
           hab_dt = hab_dt,
           sp_ranges = species_ranges, 
           sp_list = species_list
  ) {
    # ------------------- details ------------------- #
    # This function calculates the Area of Habitat for one species (index)
    # at one site (site_index), and in one or more years (year_index).
    # It requires the following elements to be pre-loaded
    # a. "habitat_prefs"
    # b. "jung_hab_type_area_df"
    # c. "iucn_crosswalk"
    # d. "elevation_prefs"
    # e. "site_df"
    
    # It takes as parameters:
    # i. "hab_dt", the habitat map  (e.g., Yin et al. 2020, lc maps or otherwise), 
    #     loaded as a data.table. This must include a column with elevation 
    #     (e.g., elevation_map, derived from  "p_derived/elevation/shaanxi_srtm_crop.tif"), and a column
    #     of area (derived from site_area_ha, e.g., "p_derived/site_area_ha/shaanxi_area_ha.tif"),
    #     which are selected by site_index, then converted to data.table and added to hab_dt.
    # ii. "sp_ranges," a sf file of species range maps, 
    #     which is "species_ranges" by default, filtered to just 
    #     mammals, birds, and amphibians, following standard presence/origin filtering,
    #     and cropped to my 11 sites.
    # iii."sp_list," a simple data.frame containing all unique runs to be calculated
    #     during the function call. The index is used to isolate a specific species/site
    #     combination for AOH to be calculated.
    
    # for testing
    # index <- 3
    # index <- grep("Alauda arvensis", sp_list$binomial)
    # site_index <- 3
    # core <- 5
    # year_index <- 2011:2017
    # year_index <- 2015
    # calc_lc <- TRUE
    # include_time <- TRUE
    # sp_ranges <- species_ranges
    # sp_list <- species_list %>% filter(core_index == core)
    # ----------------------------------------------- #
      
    # --------- #
    tic.clearlog()
    tic(
      paste0("data.table AOH, site ", site_df$site[site_index],
             ", run index ", index,
             ", for years ", min(year_index), "-", max(year_index))
    )

    print(sp_list[index, ])
    sp_name <- sp_list$binomial[index]
    
    cat("Species name:", sp_name, fill = TRUE)
    
    # ---- extract species range polygons at the site ---- #
    # select a subset of species range polygons based on binomial, and 
    # update all features to multipolygon, for fasterize()
    range_sf <- st_cast(sp_ranges[sp_ranges$binomial == sp_name, ]) %>%
      filter(site == site_df$site[site_index])
    
    # ---- turn the species range polygons (sf) into a raster ---- #
    range_t <- 
      fasterize(range_sf,
                raster::raster(resolution = terra::res(elevation_map[[site_index]]),
                               ext = raster::extent(terra::ext(elevation_map[[site_index]])[1:4])),
                field = "seasonal" # so that cell values correspond to seasonality code
      ) %>% 
      rast() #%>% # convert to SpatRaster 
    # subst(1, 0,
    #       filename = paste0(tmp_location, "range_t_tmp.tif"),
    #       overwrite = T) # update cell values from 1 to 0.
    
    # plot(range_t)
    
    range_dt <- spatraster_to_dt(spt = range_t)
    
    # a quick test to make sure the x and y columns match, to circumvent the need
    # to round x and y to get the data.table::merge() to work correctly.
    stopifnot(
      all.equal(hab_dt$x, range_dt$x),
      all.equal(hab_dt$y, range_dt$y)
    )
    
    # add range to the data.table as a column
    hab_dt[, range := range_dt$layer]
    
    # ------------------------------------------------------------------------- #
    ### Habitat Filter ###
    # ------------------------------------------------------------------------- #
    z1 <- habitat_prefs %>% # extract the habitat classifications for the species in question
      filter(binomial == sp_name,
             suitability == "Suitable",
             !is.na(map_code)) %>% 
      { if (major_importance_only) filter(., majorImportance == "Yes") else .}
    

    # extract the lc (or map_code) codes from habitat_prefs based on 
    # iucn_crosswalk, which correspond to the IUCN habitat codes
    habitat_prefs_rcl <- z1 %>%
      select(codes = if(calc_lc) "lc" else "map_code") %>% # select lc class codes, or IUCN habitat map codes, depending on the "calc_lc" switch
      arrange(codes) %>% unique() %>% .$codes
    
    # data.frame to use to adjust area estimates based on the proportion of each
    # land cover type that is made up by suitable habitat types
    adj_df <- jung_hab_type_area_df %>%
      filter(site == site_df$site[site_index],
             habitat_type %in% unique(z1$map_code)
             ) %>%
      group_by(lc) %>%
      summarise(adjustment = sum(prop_lc))
    
    # ------------------------------------------------------------------------- #
    ### Elevation Filter ###
    # ------------------------------------------------------------------------- #
    elevation_prefs_rcl <- elevation_prefs %>% filter(binomial == sp_name)
    
    # ------------------------------------------------------------------------- #
    ### Calculate AOH, broken down by habitat type ###
    # ------------------------------------------------------------------------- #
    
    # subset data.table to only pixels within both species range and elevation range, first:
    hab_filtered_range_el <- hab_dt[!is.na(range) &
                                      elevation <= elevation_prefs_rcl$elevation_upper &
                                      elevation >= elevation_prefs_rcl$elevation_lower]
    
    # calculate AOH in each year
    # i think this is going to list the area in each map_code / lc, which would allow for 
    # filtering based on major importance at a later point.
    df_tmp <- lapply(year_index, function(i){
      tmp <-
        hab_filtered_range_el[get(paste0("y", i)) %in% habitat_prefs_rcl, 
                              sum(area_ha), 
                              by = c(paste0("y", i), "range")
        ][,"year" := i]
      names(tmp) <- c(if(calc_lc) "lc" else "map_code", 
                      "seasonality", # added the area in each seasonality type for each lc/map_code
                      "area_ha", "year")
      tmp
    }) %>% bind_rows()
    
    # # all data.table
    # df_tmp <- df_tmp[, ':='(site = site_df$site[site_index],
    #               binomial = sp_name)]
    # df_tmp <- df_tmp[, .(site, binomial, lc, year, area_ha)]
    # df_tmp <- df_tmp[adj_df, on = "lc"]
    # df_tmp[, adj_area_ha := area_ha * adjustment]
    # df_tmp[, ':='(IUCN_aoh_ha = sum(area_ha),
    #               adj_IUCN_aoh_ha = sum(adj_area_ha)), 
    #        by = c("site", "binomial", "year")]
    
    # add in site, species name
    df_tmp <- df_tmp %>% 
      mutate(site = site_df$site[site_index],
             binomial = sp_name) %>%
      select(site, binomial, year, everything())
    
    # adjust area by the proportion of land cover that is suitable
    if (calc_lc) {
      df_tmp <- df_tmp %>%
        left_join(adj_df, by = "lc") %>% 
        mutate(adj_area_ha = area_ha * adjustment)
    }
    
    # calculate the AOH summed across habitat types, join to original df
    dt_tmp <- df_tmp %>%
      left_join(df_tmp %>% 
                  group_by(site, binomial, year) %>% 
                  summarise(IUCN_aoh_ha = sum(area_ha),
                            adj_IUCN_aoh_ha = sum(adj_area_ha, na.rm = TRUE))
      )
    
    toc(log = T)
    
    if (include_time) {
      dt_tmp <- dt_tmp %>%
        mutate(time = 
                 tic.log(format = F) %>% bind_rows() %>%
                 mutate(time = toc - tic) %>% .$time)
    }
    
    cat("calculated AOH for", sp_name, fill = TRUE)
    cat("Adjusted AOH in", year_index[length(year_index)],
        "=", dt_tmp$adj_IUCN_aoh_ha[nrow(dt_tmp)], "ha", fill = TRUE)
    
    # return summary table
    dt_tmp
  }


cc_AOH_dt_max_age <- 
  function(index,
           site_index,
           year_index, # the real year(s)
           calc_lc = TRUE,
           include_time = FALSE,
           hab_dt = hab_dt,
           sp_ranges = species_ranges, 
           sp_list = species_list
  ) {
    # ------------------- details ------------------- #
    # This function calculates the Area of Habitat for one species (index)
    # at one site (site_index), and in one or more years (year_index).
    # It requires the following elements to be pre-loaded
    # a. "habitat_prefs"
    # b. "jung_hab_type_area_df"
    # c. "iucn_crosswalk"
    # d. "elevation_prefs"
    # e. "site_df"
    
    # It takes as parameters:
    # i. "hab_dt", the habitat map  (e.g., Yin et al. 2020, lc maps or otherwise), 
    #     loaded as a data.table. This must include a list of elevation maps 
    #     (e.g., p_derived/elevation/shaanxi_srtm_crop.tif"), and a list of 
    #     area maps (site_area_ha, e.g., p_derived/site_area_ha/shaanxi_area_ha.tif),
    #     which are selected by site_index, then converted to data.table and added to hab_dt.
    # ii. "sp_ranges," a sf file of species range maps, 
    #     which is "species_ranges" by default, filtered to just 
    #     mammals, birds, and amphibians, following standard presence/origin filtering,
    #     and cropped to my 11 sites.
    # iii."sp_list," a simple data.frame containing all unique runs to be calculated
    #     during the function call. The index is used to isolate a specific species/site
    #     combination for AOH to be calculated.
    
    # for testing
    # index <- 3
    # site_index <- 3
    # year_index <- 2011:2017
    # calc_lc <- TRUE
    # include_time <- TRUE
    # range_maps <- vert_sites
    # ----------------------------------------------- #
    
    # --------- #
    tic.clearlog()
    tic(
      paste0("data.table AOH, site ", site_df$site[site_index],
             ", run index ", index,
             ", for years ", min(year_index), "-", max(year_index))
    )
    
    print(sp_list[index, ])
    sp_name <- sp_list$binomial[index]
    
    cat("Species name:", sp_name, fill = TRUE)
    
    # ---- extract species range polygons at the site ---- #
    # select a subset of species range polygons based on binomial, and 
    # update all features to multipolygon, for fasterize()
    range_sf <- st_cast(sp_ranges[sp_ranges$binomial == sp_name, ]) %>%
      filter(site == site_df$site[site_index])
    
    # ---- turn the species range polygons (sf) into a raster ---- #
    range_t <- 
      fasterize(range_sf,
                raster::raster(resolution = terra::res(elevation_map[[site_index]]),
                               ext = raster::extent(terra::ext(elevation_map[[site_index]])[1:4]))
      ) %>% 
      rast() #%>% # convert to SpatRaster 
    # subst(1, 0,
    #       filename = paste0(tmp_location, "range_t_tmp.tif"),
    #       overwrite = T) # update cell values from 1 to 0.
    
    # plot(range_t)
    
    range_dt <- spatraster_to_dt(spt = range_t)
    
    # a quick test to make sure the x and y columns match, to circumvent the need
    # to round x and y to get the data.table::merge() to work correctly.
    stopifnot(
      all.equal(hab_dt$x, range_dt$x),
      all.equal(hab_dt$y, range_dt$y)
    )
    
    # add range to the data.table as a column
    hab_dt[, range := range_dt$layer]
    
    # ------------------------------------------------------------------------- #
    ### Habitat Filter ###
    # ------------------------------------------------------------------------- #
    z1 <- habitat_prefs %>% 
      filter(binomial == sp_name,
             suitability == "Suitable") # extract the habitat classifications for the species in question
    
    # extract the lc codes from my crosswalk that correspond to the IUCN habitat codes
    habitat_prefs_rcl <- 
      iucn_crosswalk %>% 
      filter(code %in% unique(z1$code)) %>%
      select(codes = ifelse(calc_lc, "lc", "map_code")) %>% # select lc class codes, or IUCN habitat map codes, depending on the "calc_lc" switch
      unique() %>% .$codes
    
    # data.frame to use to adjust area estimates based on the proportion of each
    # land cover type that is made up by suitable habitat types
    adj_df <- jung_hab_type_area_df %>%
      filter(site == site_df$site[site_index],
             # habitat_type %in% unique(z1$map_code)[!is.na(unique(z1$map_code))]
             code %in% unique(z1$code)
      ) %>%
      group_by(lc) %>%
      summarise(adjustment = sum(prop_lc))
    
    # ------------------------------------------------------------------------- #
    ### Elevation Filter ###
    # ------------------------------------------------------------------------- #
    elevation_prefs_rcl <- elevation_prefs %>% filter(binomial == sp_name)
    
    # ------------------------------------------------------------------------- #
    ### Calculate AOH, broken down by habitat type ###
    # ------------------------------------------------------------------------- #
    
    # subset data.table to only pixels within both species range and elevation range, first:
    hab_filtered_range_el <- hab_dt[max_age >= 5 &
                                      !is.na(range) &
                                      elevation <= elevation_prefs_rcl$elevation_upper &
                                      elevation >= elevation_prefs_rcl$elevation_lower]
    
    # calculate AOH in each year
    df_tmp <- lapply(year_index, function(i){
      tmp <-
        hab_filtered_range_el[get(paste0("y", i)) %in% habitat_prefs_rcl, 
                              sum(area_ha), 
                              by = c(paste0("y", i))
        ][,"year" := i]
      names(tmp) <- c("lc", "area_ha", "year")
      tmp
    }) %>% bind_rows()
    
    # # all data.table
    # df_tmp <- df_tmp[, ':='(site = site_df$site[site_index],
    #               binomial = sp_name)]
    # df_tmp <- df_tmp[, .(site, binomial, lc, year, area_ha)]
    # df_tmp <- df_tmp[adj_df, on = "lc"]
    # df_tmp[, adj_area_ha := area_ha * adjustment]
    # df_tmp[, ':='(IUCN_aoh_ha = sum(area_ha),
    #               adj_IUCN_aoh_ha = sum(adj_area_ha)), 
    #        by = c("site", "binomial", "year")]
    
    # add in site, species name
    df_tmp <- df_tmp %>% 
      mutate(site = site_df$site[site_index],
             binomial = sp_name) %>%
      select(site, binomial, year, everything())
    
    # adjust area by the proportion of land cover that is suitable
    df_tmp <- df_tmp %>%
      left_join(adj_df, by = "lc") %>% 
      mutate(adj_area_ha = area_ha * adjustment)
    
    # calculate the AOH summed across habitat types, join to original df
    dt_tmp <- df_tmp %>%
      left_join(df_tmp %>% 
                  group_by(site, binomial, year) %>% 
                  summarise(IUCN_aoh_ha = sum(area_ha),
                            adj_IUCN_aoh_ha = sum(adj_area_ha, na.rm = TRUE))
      )
    
    toc(log = T)
    
    if (include_time) {
      dt_tmp <- dt_tmp %>%
        mutate(time = 
                 tic.log(format = F) %>% bind_rows() %>%
                 mutate(time = toc - tic) %>% .$time)
    }
    
    cat("calculated AOH for", sp_name, fill = TRUE)
    cat("Adjusted AOH in", year_index[length(year_index)],
        "=", dt_tmp$adj_IUCN_aoh_ha[nrow(dt_tmp)], "ha", fill = TRUE)
    
    # return summary table
    dt_tmp
  }





# based on all species ranges, not a subset
cc_AOH_data.table_general <- 
  function(index,
           site_index,
           year_index, # the real year(s)
           calc_lc = TRUE,
           include_time = FALSE,
           hab_dt = hab_dt,
           range_maps = vert_sites#, species_list = species_list
           ) {
    # ------------------- details ------------------- #
    # This function calculates the Area of Habitat for one species (index)
    # at one site (site_index), and in one or more years (year_index).
    # It requires the following elements to be pre-loaded
    # a. "hab_dt", the habitat map  (e.g., Yin et al. 2020, lc maps or otherwise), 
    #     loaded as a data.table. This must include a list of elevation maps 
    #     (e.g., p_derived/elevation/shaanxi_srtm_crop.tif"), and a list of 
    #     area maps (site_area_ha, e.g., p_derived/site_area_ha/shaanxi_area_ha.tif),
    #     which are selected by site_index, then converted to data.table and added to hab_dt.
    # b. "range_maps," specified as a parameter. This is "vert_sites" by default, which is
    #     an sf object of all species range maps, filtered to just 
    #     mammals, birds, and amphibians, following standard presence/origin filtering,
    #     and cropped to my 11 sites.
    # c. "habitat_prefs"
    # d. "jung_hab_type_area_df"
    # d. "iucn_crosswalk"
    # e. "elevation_prefs"
    # f. "site_df"
    
    # for testing
    # index <- 3
    # site_index <- 3
    # year_index <- 2011:2017
    # calc_lc <- TRUE
    # include_time <- TRUE
    # range_maps <- vert_sites
    # ----------------------------------------------- #
    
    
    # --------- #
    tic.clearlog()
    tic(
      paste0("data.table AOH, site ", site_df$site[site_index],
             ", run index ", index,
             ", for years ", min(year_index), "-", max(year_index))
    )
    
    sp_ranges <- range_maps %>%
      filter(vert_class != "gard",
             site == site_df$site[site_index]) # filter to just the site in question
    
    sp_list <- sp_ranges %>% st_drop_geometry() %>%
      select(site, vert_class, binomial) %>% unique() %>%
      arrange(site, vert_class, binomial)
    
    sp_name <- sp_list$binomial[index]

    cat("Species name:", sp_name, fill = TRUE)
    
    # ---- extract species range polygons at the site ---- #
    # select a subset of species range polygons based on binomial, and 
    # update all features to multipolygon, for fasterize()
    range_sf <- st_cast(sp_ranges[sp_ranges$binomial == sp_name, ]) 

    # ---- turn the species range polygons (sf) into a raster ---- #
    range_t <- 
      fasterize(range_sf,
                raster::raster(resolution = terra::res(elevation_map[[site_index]]),
                               ext = raster::extent(terra::ext(elevation_map[[site_index]])[1:4]))
      ) %>% 
      rast() #%>% # convert to SpatRaster 
    # subst(1, 0,
    #       filename = paste0(tmp_location, "range_t_tmp.tif"),
    #       overwrite = T) # update cell values from 1 to 0.
    
    # plot(range_t)
    
    range_dt <- spatraster_to_dt(spt = range_t)
    
    # a quick test to make sure the x and y columns match, to circumvent the need
    # to round x and y to get the data.table::merge() to work correctly.
    stopifnot(
      all.equal(hab_dt$x, range_dt$x),
      all.equal(hab_dt$y, range_dt$y)
    )
    
    # add range to the data.table as a column
    hab_dt[, range := range_dt$layer]
    
    # ------------------------------------------------------------------------- #
    ### Habitat Filter ###
    # ------------------------------------------------------------------------- #
    z1 <- habitat_prefs %>% 
      filter(binomial == sp_name,
             suitability == "Suitable") # extract the habitat classifications for the species in question
    
    # extract the lc codes from my crosswalk that correspond to the IUCN habitat codes
    habitat_prefs_rcl <- 
      iucn_crosswalk %>% 
      filter(code %in% unique(z1$code)) %>%
      select(codes = ifelse(calc_lc, "lc", "map_code")) %>% # select lc class codes, or IUCN habitat map codes, depending on the "calc_lc" switch
      unique() %>% .$codes
    
    # data.frame to use to adjust area estimates based on the proportion of each
    # land cover type that is made up by suitable habitat types
    adj_df <- jung_hab_type_area_df %>%
      filter(site == site_df$site[site_index],
             code %in% unique(z1$code)) %>%
      group_by(lc) %>%
      summarise(adjustment = sum(prop_lc))
    
    # ------------------------------------------------------------------------- #
    ### Elevation Filter ###
    # ------------------------------------------------------------------------- #
    elevation_prefs_rcl <- elevation_prefs %>% filter(binomial == sp_name)
    
    # ------------------------------------------------------------------------- #
    ### Calculate AOH, broken down by habitat type ###
    # ------------------------------------------------------------------------- #
    
    # subset data.table to only pixels within both species range and elevation range, first:
    hab_filtered_range_el <- hab_dt[!is.na(range) &
                                      elevation <= elevation_prefs_rcl$elevation_upper &
                                      elevation >= elevation_prefs_rcl$elevation_lower]
    
    # calculate AOH in each year
    df_tmp <- lapply(year_index, function(i){
      tmp <-
        hab_filtered_range_el[get(paste0("y", i)) %in% habitat_prefs_rcl, 
                              sum(area_ha), 
                              by = c(paste0("y", i))
        ][,"year" := i]
      names(tmp) <- c("lc", "area_ha", "year")
      tmp
    }) %>% bind_rows()
    
    # # all data.table
    # df_tmp <- df_tmp[, ':='(site = site_df$site[site_index],
    #               binomial = sp_name)]
    # df_tmp <- df_tmp[, .(site, binomial, lc, year, area_ha)]
    # df_tmp <- df_tmp[adj_df, on = "lc"]
    # df_tmp[, adj_area_ha := area_ha * adjustment]
    # df_tmp[, ':='(IUCN_aoh_ha = sum(area_ha),
    #               adj_IUCN_aoh_ha = sum(adj_area_ha)), 
    #        by = c("site", "binomial", "year")]

    # add in site, species name
    df_tmp <- df_tmp %>% 
      mutate(site = site_df$site[site_index],
             binomial = sp_name) %>%
      select(site, binomial, year, everything())
    
    # adjust area by the proportion of land cover that is suitable
    df_tmp <- df_tmp %>%
      left_join(adj_df, by = "lc") %>% 
      mutate(adj_area_ha = area_ha * adjustment)
    
    # calculate the AOH summed across habitat types, join to original df
    dt_tmp <- df_tmp %>%
      left_join(df_tmp %>% 
                  group_by(site, binomial, year) %>% 
                  summarise(IUCN_aoh_ha = sum(area_ha),
                            adj_IUCN_aoh_ha = sum(adj_area_ha, na.rm = TRUE))
      )

    toc(log = T)
    
    if (include_time) {
      dt_tmp <- dt_tmp %>%
        mutate(time = 
                 tic.log(format = F) %>% bind_rows() %>%
                 mutate(time = toc - tic) %>% .$time)
    }
    
    cat("calculated AOH for", sp_name, fill = TRUE)
    cat("Adjusted AOH in", year_index[length(year_index)],
        "=", dt_tmp$adj_IUCN_aoh_ha[nrow(dt_tmp)], "ha", fill = TRUE)
    
    # return summary table
    dt_tmp
  }


cc_AOH_terra <- function(index,
                       site_index,
                       year_index, # the real year(s)
                       calc_lc = TRUE,
                       calc_AOO = FALSE,
                       include_time = TRUE,
                       tmp_location = paste0(p_derived, "aoh/tmp/"),
                       sp_ranges = species_ranges, 
                       sp_list = species_list
                       ) {
  # requires lc, elevation_map, site_jung_l2_30, etc. to be loaded and named correctly before running:
  
  # ------------------------------------------------------------------------- #
  ### starts here ###
  # ------------------------------------------------------------------------- #
  tic.clearlog()
  tic(
    paste0("{terra} AOH, site ", site_df$site[site_index],
           ", run index ", index,
           ", for years ", min(year_index), "-", max(year_index))
  )
  
  print(sp_list[index, ])
  
  # # No longer necessary, given that the function has sp_ranges and sp_list as parameters.
  # sp_ranges <- vert_sites %>%
  #   filter(vert_class != "gard",
  #          site == site_df$site[site_index]) # filter to just the site in question
  # 
  # sp_list <- sp_ranges %>% st_drop_geometry() %>%
  #   select(site, vert_class, binomial) %>% unique() %>%
  #   arrange(site, vert_class, binomial)
 
  sp_name <- sp_list$binomial[index]
  
  cat("Species name:", sp_name, fill = TRUE)

  # load in the habitat maps as rasters
  habitat_map <- if (calc_lc) {
    lc[[site_index]][[paste0("y", year_index)]]
    } else {
    site_jung_l2_30[[site_index]]
    }
  
  elevation_map <- elevation_map[[site_index]]
  
  # ---- extract species range polygons at the site ---- #
  # select a subset of species range polygons based on binomial, and 
  # update all features to multipolygon, for fasterize()
  range_sf <- st_cast(sp_ranges[sp_ranges$binomial == sp_name, ]) 
  
  # ---- turn the species range polygons (sf) into a raster ---- #
  range_t <- 
    fasterize(range_sf,
              raster::raster(resolution = terra::res(habitat_map),
                             ext = raster::extent(terra::ext(habitat_map)[1:4]))
    ) %>% 
    rast() %>% # convert to SpatRaster 
    subst(1, 0 #,
          # filename = paste0(tmp_location, "range_t_tmp.tif"),
          # overwrite = TRUE
          ) # update cell values from 1 to 0.
  
  # plot(range_t)
  # ------------------------------------------------------------------------- #
  ### Habitat Filter ###
  # ------------------------------------------------------------------------- #
  z1 <- habitat_prefs %>% 
    filter(binomial == sp_name,
           suitability == "Suitable") # extract the habitat classifications for the species in question
  
  # extract the lc codes from my crosswalk that correspond to the IUCN habitat codes
  suitable_habitat_rcl <- iucn_crosswalk %>% 
    filter(code %in% unique(z1$code))
  
  # reclassify habitat raster to 
  habitat_map_rcl <- 
    classify(
      habitat_map,
      rcl = select(suitable_habitat_rcl, 
                   is = ifelse(calc_lc, "lc", "map_code")
      ) %>% unique() %>% 
        mutate(becomes = 0),
      othersNA = TRUE#,
      # filename = paste0(tmp_location, "habitat_rcl_tmp.tif"),
      # overwrite = TRUE
      )
  
  # ------------------------------------------------------------------------- #
  ### Elevation Filter ###
  # ------------------------------------------------------------------------- #
  elevation_prefs_rcl <- elevation_prefs %>% filter(binomial == sp_name)
  
  elevation_map_rcl <- 
    classify(
      elevation_map,
      rcl = tibble(from = elevation_prefs_rcl$elevation_lower,
                   to = elevation_prefs_rcl$elevation_upper,
                   becomes = 0),
      othersNA = TRUE,
      include.lowest = TRUE, right = TRUE#,
      # filename = paste0(tmp_location, "elevation_rcl_tmp.tif"),
      # overwrite = TRUE
      )
  
  # mask the range polygon by the habitat mask and the elevation mask
  # tic()
  aoh <- range_t + elevation_map_rcl # step one, separated for speed
  aoh <- aoh + habitat_map_rcl
  # toc() # 4.487 for 4 layers, 7 seconds for 31 layers
  # 
  # 
  # tic()
  # aoh1 <- terra::mask(
  #   range_t, elevation_map_rcl,
  #   filename = paste0(tmp_location, "aoh_tmp1.tif"),
  #   overwrite = TRUE
  # )
  # aoh1 <- terra::mask(
  #   aoh, habitat_map_rcl,
  #   filename = paste0(tmp_location, "aoh_tmp1.tif"),
  #   overwrite = TRUE
  # )
  # toc() # 5.81 for 4 layers, 9.693 seconds
  
  # ------------------------------------------------------------------------- #
  ### Calculate stats:
  # ------------------------------------------------------------------------- #
  
  # Calculate the Area Of Occurrence (AOO), before filtering by habitat and elevation
  if (calc_AOO) {
    range_aoo_ha <- 
      terra::cellSize(range_t, unit = "ha", mask = TRUE) %>% 
      terra::global(fun = "sum", na.rm = TRUE) %>% .$sum
  }
  
  # Calculate the Area Of Occurrence (AOO), before filtering by habitat and elevation
  range_aoh_ha <- 
    terra::cellSize(aoh, unit = "ha", mask = TRUE) %>% 
    global(fun = "sum", na.rm = TRUE) %>% .$sum
  
  toc(log = T)
  
  aoh_tmp <- 
    tibble(site = site_df$site[site_index],
           binomial = sp_name,
           year = if(calc_lc) {year_index} else {"jung_2015"},
           IUCN_aoh_ha = range_aoh_ha)
  
  if (calc_AOO) {
    aoh_tmp <- aoh_tmp %>%
      mutate(IUCN_aoo_ha = range_aoo_ha)
  }
  
  if (include_time) {
    aoh_tmp <- aoh_tmp %>%
      mutate(time = 
               tic.log(format = F) %>% bind_rows() %>%
               mutate(time = toc - tic) %>% .$time)
  }
  
  # Note: the slight difference between areas before and after habitat filtering is
  # the result of a small amount of 0s added to the Yin et al. 2020 land cover maps. 
  
  cat("calculated AOH for", sp_name, fill = TRUE)
  cat("AOH in", 
      if(calc_lc) {year_index[length(year_index)]} else {"2015 (Jung AOH)"},
      "=", range_aoh_ha[length(range_aoh_ha)], "ha", fill = TRUE)
  aoh_tmp
}


# data.table ----


# Miscellaneous ----

# ------------------------- #
# Calculate standard error 
# ------------------------- #
se <- function(x) sqrt(var(x, na.rm = TRUE) / length(na.omit(x)))
# also the same as sd(x)/sqrt(length(na.omit(x)))




# ------------------------- #
# convert a data.table to a SpatRaster, via an intermediary data.frame, trimming NA border
# ------------------------- #
# analogous to Lyndon's function dtraster::dt_to_raster()
# note that this process introduces a single cell border on the top and right edge of the SpatRaster filled with NA values. 
# introducing terra::trim() fixes this issue. 
# I have confirmed that the spatraster %>% dt_to_spatraster() %>% spatraster_to_dt() workflow results in an identical spatraster.

dt_to_spatraster <- function(dt){
  spt <- dt %>%
    as.data.frame() %>%
    terra::rast(
      type = "xyz",
      crs = "+proj=longlat +datum=WGS84 +no_defs"
    ) %>% 
    terra::trim()
  
  spt
}


# ------------------------- #
# convert a SpatRaster into a data.table, via an intermediary data.frame
# ------------------------- #
# analogous to Lyndon's function dtraster::as.data.table.raster()

spatraster_to_dt <- function(spt, xy_switch = TRUE) {
  dt <- spt %>%
    terra::as.data.frame(
      na.rm = FALSE, # na.rm=TRUE is default
      xy = xy_switch) %>% 
    as.data.table()
  
  dt
}



#


# --------------------------------------------------------------- #
#
# Miscellaneous functions
# 
# --------------------------------------------------------------- #


# ------------------------- #
# calculate the sizes of items in the environment
# ------------------------- #
env_size <- function(workspace = ls()) {
  size = 0
  for (x in workspace){
    thisSize = obj_size(get(x))
    size = size + thisSize
    message(x, " = ", appendLF = F); print(thisSize, units='auto')
  }
  message("total workspace is ", appendLF = F); print(size, units='auto')
}

# ------------------------- #
# return a tibble of objects and their sizes
# ------------------------- #

get_sizes <- function(stuff) {
  lapply(stuff, function(i) {
    tibble(object = i, size = obj_size(get(i)))
  }) %>% bind_rows() %>% arrange(desc(size)) %>% mutate(mb = size / 1024^2)
}

# 
# object_sizes <- lapply(ls(), function(i) {
#   tibble(object = i, size = obj_size(get(i)))
# }) %>% bind_rows()
# 

# ------------------------- #
# capitalize name
# ------------------------- #
capwords <- function(s, strict = FALSE) {
  cap <- function(s) {
    paste(
      toupper(substring(s, 1, 1)), 
      {s <- substring(s, 2); if(strict) tolower(s) else s},
      sep = "", collapse = " "
    )
  }
  sapply(strsplit(s, split = " "), 
         cap, 
         USE.NAMES = !is.null(names(s))
  )
}
