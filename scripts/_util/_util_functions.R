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
           calc_lc = FALSE, # switch between area in "lc" (Yin et al.) vs. "map_code" (IUCN)
           include_time = TRUE, 
           major_importance_only = FALSE, # calc. area of only habitats coded as "majorImportance"
           habitat_dt = hab_dt,
           sp_ranges = species_ranges, 
           sp_list = species_list
  ) {
    # ------------------- details ------------------- #
    # This function calculates the Area of Habitat for one species ("index")
    # at one site ("site_index"), and in one or more years ("year_index").
    
    # Prerequisites:
    # It requires the following elements to be pre-loaded into the current environment.
    # a. "habitat_prefs"          -- .csv of the IUCN habitat preferences for all species at all sites.
    # b. "jung_hab_type_area_df"  -- .csv containing the area in each IUCN habitat type 
    #                             -- at each site, which is used to determine the proportion
    #                             -- in each lc class, to adjust AOH.
    # c. "iucn_crosswalk"         -- .csv crosswalking "map_code" (IUCN habitat types) with
    #                             -- "lc" (Yin et al. 2020)
    # d. "elevation_prefs"        -- .csv of IUCN elevation prefs for all species at all sites.
    # e. "site_df"                -- a simple .csv with information about eleven sites.
    
    # Parameters:
    # 1. "index"                  -- selects a binomial from "species_list", and subsequently 
    #                             -- a range from "species_ranges."
    # 2. "site_index"             -- the site index denoting the site for which AOH is being run.
    # 3. "year_index"             -- the year(s) to calculate AOH for. For the entire time series,
    #                             -- this begins with 1987 if using the full map (all pixels), or
    #                             -- 1992 if only using abandoned pixels.
    # 4. "calc_lc"                -- a switch telling the function telling the calculate area
    #                             -- for either "lc" codes (Yin et al. 2020) if TRUE, or 
    #                             -- for "map_code" (IUCN habitat codes, from Jung et al. 2021) 
    #                             -- if FALSE (the default)
    # 5. "include_time"           -- a switch that adds the processing time to the resulting 
    #                             -- data.frame if TRUE (the default).
    # 6. "major_importance_only"  -- a switch telling the function to only calculate AOH for 
    #                             -- only habitats coded as "majorImportance" by the IUCN.
    #                             -- FALSE is the default.
    # 7. "habitat_dt"             -- the habitat map (e.g., Yin et al. 2020 lc maps, IUCN habitat
    #                             -- maps [either my interpolated one, or the original from 
    #                             -- Jung et al. 2021], or otherwise), loaded as a data.table. 
    #                             -- This must include a column with elevation (e.g., elevation_map, 
    #                             -- derived from  "p_derived/elevation/shaanxi_srtm_crop.tif"), 
    #                             -- and a column of area (derived from site_area_ha, e.g., 
    #                             -- "p_derived/site_area_ha/shaanxi_area_ha.tif"), which are selected
    #                             -- by site_index, then converted to data.table and added to hab_dt.
    # 8. "sp_ranges"              -- a sf file of species range maps, which is "species_ranges" by
    #                             -- default, filtered to just mammals, birds, and amphibians, 
    #                             -- following standard presence/origin filtering, and cropped 
    #                             -- to my 11 sites.
    # 9. "sp_list"                -- a simple data.frame containing all unique runs to be calculated 
    #                             -- during the function call. The index is used to isolate a specific
    #                             -- species/site combination for AOH to be calculated.
    
    # Parameters for testing:
    # index <- 3
    # index <- grep("Catharus swainsoni", sp_list$binomial)
    # site_index <- 11 
    # species_list %>% filter(site == site_df$site[site_index], str_detect(binomial, "Catharus swainsoni"))
    # core <- 20
    # year_index <- 2011:2017
    # year_index <- 2015
    # calc_lc <- FALSE
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
      all.equal(habitat_dt$x, range_dt$x),
      all.equal(habitat_dt$y, range_dt$y)
    )
    
    # add range to the data.table as a column
    habitat_dt[, range := range_dt$layer]
    
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
      dplyr::select(codes = if(calc_lc) "lc" else "map_code") %>% # select lc class codes, or IUCN habitat map codes, depending on the "calc_lc" switch
      arrange(codes) %>% unique() %>% .$codes
    
    
    if (calc_lc) {
      # data.frame to use to adjust area estimates based on the proportion of each
      # land cover type that is made up by suitable habitat types
      
      adj_df <- jung_hab_type_area_df %>%
        filter(site == site_df$site[site_index],
               habitat_type %in% unique(z1$map_code)
               ) %>%
        group_by(lc) %>%
        summarise(adjustment = sum(prop_lc))
    }
    
    # ------------------------------------------------------------------------- #
    ### Elevation Filter ###
    # ------------------------------------------------------------------------- #
    elevation_prefs_rcl <- elevation_prefs %>% filter(binomial == sp_name)
    
    # ------------------------------------------------------------------------- #
    ### Calculate AOH, broken down by habitat type ###
    # ------------------------------------------------------------------------- #
    
    # subset data.table to only pixels within both species range and elevation range, first:
    habitat_dt <- habitat_dt[!is.na(range) &
                                      elevation <= elevation_prefs_rcl$elevation_upper &
                                      elevation >= elevation_prefs_rcl$elevation_lower]
    
    #testing ground:
    # hab_filtered_range_el[!is.na(y2015), .N, by = "range"] # area in each season type
    # z1
    # z1$map_code %>% sort()
    # hab_filtered_range_el[get(paste0("y", i)) %in% z1$map_code, 
    #                       sum(area_ha), 
    #                       by = c(paste0("y", i), "range")
    #                       ][,"year" := i][]
    # tmp <- hab_filtered_range_el[#range == 2
    #   , 
    #                       .N, by = c("y2015", "range")] %>% as_tibble()
    # tmp %>% filter(y2015 %in% z1$map_code)
    # z1 %>% select(season_code, map_code, IUCNLevel) %>% arrange(season_code, map_code)

    # Calculate AOH in each year.
    # This will list the area in each map_code / lc, which would allow for 
    # filtering based on major importance at a later point.
    df_tmp <- lapply(year_index, function(i){
      tmp <-
        habitat_dt[get(paste0("y", i)) %in% habitat_prefs_rcl, 
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
      dplyr::select(site, binomial, year, everything())
    
    if (calc_lc) {
      # adjust area by the proportion of land cover that is suitable
      df_tmp <- df_tmp %>%
        left_join(adj_df, by = "lc") %>% 
        mutate(adj_area_ha = area_ha * adjustment)
    }
    
    # calculate the AOH summed across habitat types, join to original df
    dt_tmp <- df_tmp %>%
      left_join(
        df_tmp %>% 
          group_by(site, binomial, year) %>% 
          summarise(IUCN_aoh_ha = sum(area_ha),
                    adj_IUCN_aoh_ha = if (calc_lc) sum(adj_area_ha, na.rm = TRUE))
        )
    
    toc(log = T)
    
    if (include_time) {
      dt_tmp <- dt_tmp %>%
        mutate(time = tic.log(format = F) %>% bind_rows() %>%
                 mutate(time = toc - tic) %>% .$time)
      }
    
    cat("calculated AOH for", sp_name, fill = TRUE)
    cat("AOH in", year_index[length(year_index)],
        "=", dt_tmp$IUCN_aoh_ha[nrow(dt_tmp)], "ha", fill = TRUE)
    
    if (calc_lc) {
      cat("Adjusted AOH in", year_index[length(year_index)],
          "=", dt_tmp$adj_IUCN_aoh_ha[nrow(dt_tmp)], "ha", fill = TRUE)
      }
    
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


# save SpatRaster to data.table
cc_save_spatraster_as_dt <- function(input_raster_path,
                                     output_file_ext = ".parquet") {
  
  # raster_path <- paste0(p_derived, "abn_lcc/", 
  #                       site_df$site[site_index], 
  #                       "_abn_lcc", run_label, ".tif")
  
  cat(fill = TRUE, "Converting SpatRaster to data.table...:", input_raster_path)
  
  r_tmp <- rast(input_raster_path) # load SpatRaster
  
  dt <- spatraster_to_dt(r_tmp) # convert SpatRaster to data.table
  
  output_path <- gsub(".tif", output_file_ext, input_raster_path)
  
  if (output_file_ext == ".csv") {
    fwrite(dt, file = output_path) # write to file
  }
  
  if (output_file_ext == ".parquet") {
    write_parquet(x = dt, output_path) # write to file
  }
  
  cat(fill = TRUE, "Done! Wrote data.table to:", output_path)
  
  rm(dt, r_tmp)
}


# data.table helper functions -----

# ------------------------- #
# convert a data.table to a SpatRaster, via an intermediary data.frame, trimming NA border
# ------------------------- #
# analogous to Lyndon's function dtraster::dt_to_raster()
# note that this process introduces a single cell border on the top and right edge of the SpatRaster filled with NA values. 
# introducing terra::trim() fixes this issue. 
# I have confirmed that the spatraster %>% dt_to_spatraster() %>% spatraster_to_dt() workflow results in an identical spatraster.
# However, this would be worth additional checks to confirm that it works.

dt_to_spatraster <- function(dt, trim = TRUE){
  spt <- dt %>%
    as.data.frame() %>%
    terra::rast(
      type = "xyz",
      crs = "+proj=longlat +datum=WGS84 +no_defs"
    ) 
  
  if(trim) {spt <- terra::trim(spt)}
  
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



# ------------------------- #
# calculate a dummy data.table
# ------------------------- #
cc_create_dt <- function(numrow = 15, numcol = 15, seed = 34L) {
  set.seed(seed)
  dt <- matrix(round(runif(numrow*numcol)), nrow = numrow, ncol = numcol)
  dt <- as.data.frame(dt)
  setDT(dt)
}

# ------------------------- #
#
# ------------------------- #
cc_create_bin <- function(numrow = 15, numcol = 15, seed = 34L) {
  dt <- cc_create_dt(numrow = numrow, numcol = numcol, seed = seed)
  dt[3] <- 1
  dt[13] <- 1
  dt[12] <- 0
  dt[4, 1:4] <- 1
  dt[14, 1] <- 1
  dt[1, 9] <- 1
  dt[3, 1:2] <- 0
  dt
}


# --------------------------------------------------------------- #
#
# Plotting ----
# 
# --------------------------------------------------------------- #

cc_plot_aoh_sp_site <- function(binomial, site_index,
                                passage_opt = "exclude_passage") {
  sp_name <- binomial
  lapply(binomial, function(sp_name) {
    gg_aoh_example_tmp <-
      aoh %>%
      filter(site %in% site_df$site[site_index], binomial == sp_name) %>% #print(n = 124)
      pivot_wider(
        id_cols = c("vert_class", "site", "binomial", "common_names",
                    "redlistCategory", "year", "mature_forest_obl", "passage_type"),
        names_from = aoh_type, values_from = aoh, names_pref = "aoh_") %>%
      mutate(diff_obs = aoh_full_iucn - aoh_max_abn_iucn, 
             diff_pot = aoh_full_iucn - aoh_max_potential_abn_iucn) %>%
      pivot_longer(cols = c("aoh_full_iucn", "aoh_max_abn_iucn", 
                            "aoh_max_potential_abn_iucn", 
                            "diff_obs", "diff_pot"),
                   names_to = "type", values_to = "aoh") %>%
      filter(passage_type %in% passage_opt) %>%
      
      ggplot(mapping = aes(x = year, y = aoh, col = type)) + 
      geom_line(size = 1) + 
      labs(x = "Year", y = "AOH (ha)", 
           title = sp_name,
           caption = unique(filter(run_indices, binomial == sp_name)$common_names)
      ) +
      scale_color_manual(
        name = "Type",
        labels = c("aoh_full_iucn" = "Landscape",
                   "aoh_max_abn_iucn" = "Abn, observed", 
                   "aoh_max_potential_abn_iucn" = "Abn, potential", 
                   "diff_obs" = "Diff., observed", 
                   "diff_pot" = "Diff., potential"),
        values = gg_color_hue(5)
      ) +
      facet_wrap(vars(site), labeller = as_labeller(cap_labels),
                 scales = "free")
    
    # save plot
    ggsave(plot = gg_aoh_example_tmp,
           filename = paste0(p_output, "plots/aoh/", "aoh_ex_sp_", 
                             gsub(" ", "_", sp_name),  
                             {if (length(site_df$site[site_index]) == 1) 
                               paste0("_", site_df$site[site_index])},
                             aoh_run_date, ".pdf"),
           width = 8, height = 6, units = "in")
  })
}


cc_plot_aoh_trend_sp_site <- function(binomial, site_index, 
                                      passage_opt = "exclude_passage",
                                      aoh_type = "crop_abn_iucn") {
  sp_name <- binomial
  lapply(binomial, function(sp_name) {
    gg_aoh_example_tmp <-
      aoh %>%
      filter(
        aoh_type %in% aoh_types_sub,
        site %in% site_df$site[site_index], binomial == sp_name) %>% #print(n = 124)
      pivot_wider(
        id_cols = c("vert_class", "site", "binomial", "common_names",
                    "redlistCategory", "year", "mature_forest_obl", "passage_type"),
        names_from = aoh_type, values_from = aoh, names_pref = "aoh_") %>%
      # mutate(diff_obs = aoh_full_iucn - aoh_max_abn_iucn, 
             # diff_pot = aoh_full_iucn - aoh_max_potential_abn_iucn
             # ) %>%
      pivot_longer(cols = c("aoh_full_iucn", "aoh_max_abn_iucn", "aoh_crop_abn_iucn",
                            # "aoh_max_potential_abn_iucn", 
                            # "diff_obs",
                            # "diff_pot"
                            ),
                   names_to = "type", values_to = "aoh") %>% 
      mutate(type = gsub("aoh_", "", type)) %>%
      filter(passage_type %in% passage_opt,
             type == aoh_type) %>%
      
      ggplot(mapping = aes(x = year, y = aoh, 
                           # col = type, group = type
                           )) + 
      geom_point() + 
      geom_smooth(method = "lm", se = 0.95) +
      labs(x = "Year", y = "AOH (ha)", 
           title = sp_name,
           subtitle = aoh_type,
           caption = unique(filter(run_indices, binomial == sp_name)$common_names)
      ) +
      scale_color_manual(
        name = "Type",
        labels = c("full_iucn" = "Landscape",
                   "max_abn_iucn" = "Abn, observed", 
                   "max_potential_abn_iucn" = "Abn, potential", 
                   "diff_obs" = "Diff., observed", 
                   "diff_pot" = "Diff., potential"),
        values = gg_color_hue(5)
      ) +
      facet_wrap(vars(site), labeller = as_labeller(cap_labels),
                 scales = "free")
    
    # save plot
    ggsave(plot = gg_aoh_example_tmp,
           filename = paste0(p_output, "plots/aoh/", "aoh_trend_", 
                             gsub(" ", "_", sp_name),  
                             {if (length(site_df$site[site_index]) == 1) 
                               paste0("_", site_df$site[site_index])},
                             aoh_run_date, ".pdf"),
           width = 8, height = 6, units = "in")
  })
}

# --------------------------------------------------------------- #
#
# Miscellaneous ----
# 
# --------------------------------------------------------------- #

# ------------------------- #
# Calculate standard error 
# ------------------------- #
se <- function(x) {sqrt(var(x, na.rm = TRUE) / length(na.omit(x)))}
# also the same as sd(x)/sqrt(length(na.omit(x)))



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
