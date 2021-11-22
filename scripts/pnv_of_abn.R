# What are the PNV of those pixels that are abandoned at some point during the time series?
# Christopher Crawford, October 21st, 2021
# ----------------------------------------------------------------------- #
source("/Users/christophercrawford/work/projects/biodiversity_abn/scripts/0_start.R")
site_df <- read.csv(file = paste0(p_derived, "site_df.csv"))
run_label <- "_2021_03_13" 

# ----------------------------------------------------------------------------- #
# Part I: What proportion of the land that was abandoned at each site falls into the different Potential Natural Vegetation classes?
# ----------------------------------------------------------------------------- #

# 1. Easiest method is to make a stack out of:
#   a) abandonment age rasters, year 2017, 
#   b) land cover rasters, year 2017, 
#   c) area (ha) raster
# 2. Convert to data.tables
# 3. Calculate summary statistics

# load site input land cover rasters:
# --------------- list of all sites ----------------- #
# prepared input rasters (derived by Chris)
site_input_raster_files <- list.files(paste0(p_dat_derived, "input_rasters"), full.names = TRUE) %>%
  grep(".tif", ., value = TRUE) #%>% grep("age", ., value = TRUE, invert = TRUE)

# as SpatRaster
lc <- lapply(seq_along(site_input_raster_files), function(i) {
  terra::rast(site_input_raster_files[i])
})
names(lc) <- site_df$site

# rename raster layers:
for (i in 1:11) {
  if (names(lc[i]) == "nebraska") {
    names(lc[[i]]) <- paste0("y", 1986:2018)
  } else {
    if (names(lc[i]) == "wisconsin") {
      names(lc[[i]]) <- paste0("y", 1987:2018)
    } else {
      # everything else, just 1987:2017
      names(lc[[i]]) <- paste0("y", 1987:2017)
    }}}

# ----------------- load abandonment age rasters ---------------- #
# abandonment age maps (produced by Chris)
age_files <- list.files(paste0(p_dat_derived, "age_rasters/", run_label), full.names = TRUE) %>%
  grep(".tif", ., value = TRUE) #%>% grep("age", ., value = TRUE, invert = FALSE)

# as SpatRaster (for area calculation)
age_t <- lapply(seq_along(age_files), function(i) {
  terra::rast(age_files[i])
})
names(age_t) <- site_df$site
for (i in seq_along(age_t)) {names(age_t[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017


# load PNV
site_pnv_30 <- lapply(
  list.files(paste0(p_derived, "site_pnv"), full.names = TRUE) %>% grep("_30.tif", ., value = TRUE), 
  function(i) rast(i)
)
names(site_pnv_30) <- site_df$site





# ----------------------------------------- #
# stack and manipulate #
# ----------------------------------------- #

pnv_area_df <- lapply(site_df$site, 
                      function(i) {
  # Select just the lc and abandonment age for 2017, calculate area, and combine into a SpatRaster
  t17 <- terra::rast(
    list(
      lc[[i]]$y2017, # land cover in 2017
      age_t[[i]]$y2017, # abandonment age in 2017,
      site_pnv_30[[i]], # PNV, resampled to 30 m
      terra::cellSize(age_t[[i]]$y2017, unit = "ha", mask = FALSE)
    )
    )
  
  names(t17) <- c("lc_2017", "age_2017", "pnv", "area_ha")
  # plot(t17)
  
  # convert to data.tables
  dt17 <- spatraster_to_dt(t17)
  
  # dt17[, .N, by = lc_2017][order(lc_2017)]
  # dt17[, .N, by = pnv][order(pnv)]
  # plot(t17$lc_2017)
  # plot(t17)
  # 
  # dt17[age_2017 >= 5, 
  #      .(pixel_count = .N, 
  #        sum_area_ha = sum(area_ha)), 
  #      by = .(lc_2017, age_2017, pnv)] 
  # 
  # # how many abandoned pixels have an NA pnv?
  # dt17[#is.na(pnv) & 
  #        age_2017 >= 5, 
  #      .(pixel_count = .N, 
  #        sum_area_ha = sum(area_ha)), 
  #      by = .(lc_2017, age_2017, pnv)][order(lc_2017, age_2017)][, sum(sum_area_ha)]
  # 
  # # the area with NA pnv is 0.04 % of the total area abandoned. 
  
  # where age is greater than or equal to 5, what is the land cover class breakdown?
  # calculate the sum of area, pixels, etc.
  lc_area <- dt17[age_2017 >= 5, 
                  .(pixel_count = .N, 
                    sum_area_ha = sum(area_ha)), 
                  by = .(lc_2017, age_2017, pnv)] %>% 
    as_tibble() %>%
    arrange(lc_2017, age_2017) %>%
    mutate(site = i)
  
  lc_area
  }
) %>% bind_rows()

pnv_area_df %>% arrange(lc_2017, age_2017) %>%
  group_by(pnv) %>%
  summarise(full_sum = sum(sum_area_ha))

# save the pnv_area_df tibble
write_csv(pnv_area_df, file = paste0(p_derived, "/pnv_area_df.csv"))

pnv_area_df <- read_csv(file = paste0(p_derived, "/pnv_area_df.csv"))

pnv_area_df$site %>% unique()

# what proportion of the abandoned land at each site ends up in forest vs. grassland?
pnv_prop_abn_df <- pnv_area_df %>% 
  group_by(site, pnv) %>%
  summarise(area_abn_ha = sum(sum_area_ha)) %>%
  left_join(., 
            pnv_area_df %>% 
              group_by(site) %>%
              summarise(total_area_abn_ha = sum(sum_area_ha))) %>%
  mutate(prop_abn_by_pnv = area_abn_ha/total_area_abn_ha) %>% 
  ungroup() # %>%
  # left_join(., filter(., lc_2017 == 4) %>% arrange(prop_lc_area_ha) %>% mutate(order = 1:n()) %>% select(site, order))

# save:
write_csv(pnv_prop_abn_df, file = paste0(p_derived, "/pnv_prop_abn_df.csv"))
# pnv_prop_abn_df <- read_csv(file = paste0(p_derived, "/pnv_prop_abn_df.csv"))

pnv_prop_abn_df %>% 
  filter(prop_abn_by_pnv > 0.001,
         !is.na(pnv)) # filter to show only the PNV with greater than 0.1% of abandoned area

pnv_table$labels

pnv_prop_abn_df$pnv %>% unique() %>% sort

my_new_colors <- pnv_table_sub$cols
my_new_labels <- pnv_table_sub$labels
names(my_new_labels) <- pnv_table_sub$Number
names(my_new_colors)<- pnv_table_sub$Number

my_new_labels[pnv_prop_abn_df$pnv %>% unique()]

pnv_table_sub <- filter(pnv_table, Number %in% sort(unique(pnv_prop_abn_df$pnv)))



# plot
gg_pnv_prop_abn <-
  ggplot(data = pnv_prop_abn_df %>% 
           filter(prop_abn_by_pnv > 0.001,
                  !is.na(pnv)), 
         mapping = aes(y = site,#fct_reorder(site, order),
                       x = prop_abn_by_pnv, 
                       fill = as_factor(pnv))) +
  geom_col(position = position_dodge(), width = 0.6) + 
  scale_x_continuous(n.breaks = 10) +
  labs(x = "Proportion of abandoned cropland area\nin each PNV class (2017)", 
       y = NULL,
       fill = "PNV class") + 
  scale_fill_manual(
    values = my_new_colors,
    labels =function(i) str_wrap(my_new_labels[i], width = 20) 
    #values = c("#276419", "#7FBC41"), labels = c("Forest", "Grassland")
    ) +
  theme_classic() +
  scale_y_discrete(#expand = c(0.02, 0),
                   labels = c("belarus" = "Vitebsk, Belarus /\nSmolensk, Russia",
                              "bosnia_herzegovina" = "Bosnia &\n Herzegovina",
                              "chongqing" = "Chongqing, China",
                              "goias" = "Goiás, Brazil",
                              "iraq" = "Iraq",
                              "mato_grosso" = "Mato Grosso,\nBrazil",
                              "nebraska" = "Nebraska /\nWyoming, USA",
                              "orenburg" = "Orenburg, Russia /\nUralsk, Kazakhstan",
                              "shaanxi" = "Shaanxi/Shanxi,\nChina",
                              "volgograd" = "Volgograd, Russia",
                              "wisconsin" = "Wisconsin, USA")) + 
    guides(fill = guide_legend(order = 1, ncol = 1, override.aes = list(linetype = 0)))

ggsave(plot = gg_pnv_prop_abn,
       filename = paste0(p_plots, "/gg_pnv_prop_abn.pdf"), 
       width = 7, height = 6, units = "in")


### TBD ###
# ----------------------------------------------------------------------------- #
# Part II: At what age does abandoned land transition from grassland to forest?
# ----------------------------------------------------------------------------- #
# Take the abandonment age dt, and run the filter again to remove all 0s and all 1s, after the moving window filters.
# Take the raw land cover dt, and select the matching x and y, and Select a new  this as 1




# ----------------------------------------------------------------------------- #
# Part III: what land cover class is most likely to be found in newly abandoned land?
# ----------------------------------------------------------------------------- #

# what land cover classes are more likely to get recultivated? What is the LC the year prior to recultivation? 


# Update, 4.12.2021
# Two parts to this idea:
# 0. What is the distribution of abandoned land across land cover types at the end of the time series? 
# In other words, in 2017, what percentage of abandoned land is in grassland, forest, etc. 

# 1. transitions between categorical land cover classes, and 
# 1a. Which pieces of land transition from crop -> grassland -|, crop -> grassland -> forest, crop -> forest?
# 1b. How long does it take for abandoned land to transition from grassland to forest in different sites?

# 2. continuous evolution of some metrics through time post abandonment
# 2. NDVI, biomass accumulation post abandonment. 