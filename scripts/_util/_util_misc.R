# --------------------------------------------------------------- #
#
# Util miscellaneous - primarily for formatting various plots and tables
# 
# --------------------------------------------------------------- #

# ------------------------ #
# Plot labels ----
# ------------------------ #
# update site labels

fancy_labels <- c("belarus" = "Vitebsk, Belarus /\nSmolensk, Russia",
                  "bosnia_herzegovina" = "Bosnia &\nHerzegovina",
                  "chongqing" = "Chongqing, China",
                  "goias" = "Goiás, Brazil",
                  "iraq" = "Iraq",
                  "mato_grosso" = "Mato Grosso,\nBrazil",
                  "nebraska" = "Nebraska /\nWyoming, USA",
                  "orenburg" = "Orenburg, Russia /\nUralsk, Kazakhstan",
                  "shaanxi" = "Shaanxi/Shanxi,\nChina",
                  "volgograd" = "Volgograd, Russia",
                  "wisconsin" = "Wisconsin, USA")

long_labels <- c("belarus" = "Vitebsk, Belarus / Smolensk, Russia",
                 "bosnia_herzegovina" = "Bosnia & Herzegovina",
                 "chongqing" = "Chongqing, China",
                 "goias" = "Goiás, Brazil",
                 "iraq" = "Iraq",
                 "mato_grosso" = "Mato Grosso, Brazil",
                 "nebraska" = "Nebraska/Wyoming, USA",
                 "orenburg" = "Orenburg, Russia / Uralsk, Kazakhstan",
                 "shaanxi" = "Shaanxi/Shanxi, China",
                 "volgograd" = "Volgograd, Russia",
                 "wisconsin" = "Wisconsin, USA")

cap_labels <- c("belarus" = "Belarus",
                "bosnia_herzegovina" = "Bosnia & Herzegovina",
                "chongqing" = "Chongqing",
                "goias" = "Goiás",
                "iraq" = "Iraq",
                "mato_grosso" = "Mato Grosso",
                "nebraska" = "Nebraska/Wyoming",
                "orenburg" = "Orenburg",
                "shaanxi" = "Shaanxi",
                "volgograd" = "Volgograd",
                "wisconsin" = "Wisconsin")

site_labels <- 
  tibble(
    site = names(long_labels),
    site_long = long_labels,
    # fancy = fancy_labels,
  )




# --------------------------------------------------------------- #
# ----------------------- Plot colors ------------------------ 
# --------------------------------------------------------------- #

# from: https://medialab.github.io/iwanthue/
cbf_col <- c("#6972d7", "#9fac3a", "#583586", "#69a150", "#b853a2",
             "#45c097", "#ba4b7d", "#c1893c", "#628ed6", "#b85136",
             "#bf81d7", "#ba4758")

# general ggplot color palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
# show_col(gg_color_hue(3))

# to define a color palette for plotting, first make a character vector with the colors, then assign names to match the factor levels.
# use this as scale_fill_manual(values = your_custom_palette)

# ------ land cover map plots ------- #

# define color palette for plotting:
# 1. Non-veg
# 2. Woody veg
# 3. Crop
# 4. Grassland
lc_plot_cols <- data.frame(
  color = c("gray80", # gray, 1. Non-veg
            terrain.colors(9)[1], # "#00A600" # dark green, 2. Woody veg
            terrain.colors(9)[5], # "#E8C32E" # gold, 3. Crop
            terrain.colors(9)[3]  # "#8BD000" # light green, 4. Grassland
  ),
  name = c("Non-veg.", 
           "Forest", 
           "Cropland", 
           "Grassland"),
  breaks = c(1, 2, 3, 4))


# ------------------------------------------------------ #
## AOH color palette ----
# ------------------------------------------------------ #

# met.brewer(name = "VanGogh2", 4)
# met.brewer(name = colorblind_palettes[24], 4)
# met.brewer(name = colorblind_palettes[6], 4)
# colorblind_palettes
show.col(colorblind_palettes[1])
met.brewer(name = colorblind_palettes[8], 3)

input_palette <- met.brewer(name = "Kandinsky", 4)[c(1, 4, 3, 2)] # use this one, for now
# input_palette <- met.brewer(name = "Egypt", 4)[c(3, 2, 4, 1)]
# input_palette <- met.brewer(name = "Java", 4)[c(4, 1, 3, 2)]
# show.col(input_palette)


# set up color palettes:
palette_du_jour <- c(
  "gain" = input_palette[1],
  "weak gain" = alpha(input_palette[1], 0.5),
  "no trend" = input_palette[2],
  "context dependent" = input_palette[3],
  "weak loss" = alpha(input_palette[4], 0.5),
  "loss" = input_palette[4]
)

palette_du_jour2 <- c(
  "gain" = input_palette[1],
  "weak gain" = alpha(input_palette[1], 0.5),
  "no trend" = "gray60",
  "context dependent" = input_palette[3],
  "weak loss" = alpha(input_palette[4], 0.5),
  "loss" = input_palette[4]
)
# 
# show_col(palette_du_jour)
# show_col(palette_du_jour2)

palette_labels <- c(
  "gain" = "Gain",
  "weak gain" = "Weak Gain",
  "no trend" = "No Trend",
  "context dependent" = "Conflicting",
  # "context dependent" = "Opposites",
  "weak loss" = "Weak Loss", 
  "loss" = "Loss"
)


frag_palette <- c(
  "increase" = input_palette[1],
  "no trend" = input_palette[2],
  "decrease" = input_palette[4]
)
# 
# show_col(frag_palette)
# show_col(palette_du_jour2)

frag_palette_labels <- c(
  "increase" = "Increasing",
  "no trend" = "No Trend",
  "decrease" = "Decreasing"
)




# ------------------------------------------------------ #
# Table formatting ----
# ------------------------------------------------------ #

regression_model_labels0 <- 
  list(
    Trophic_level = "Trophic Level",
    `log(Body_mass_g)` = "ln(Body Mass, g)",
    `log(total_range_area)` = "ln(Global Range Area, km2)",
    forest_occ = "Forest Suitability",
    savanna_occ = "Savanna Suitability",
    shrubland_occ = "Shrubland Suitability",
    grass_occ = "Grassland Suitability",
    wetlands_occ = "Wetlands Suitability",
    artificial_occ = "Artificial Suitability",
    n_hab_occ = "Number of Suitable Habitats",
    threatened = "Threatened (IUCN)",
    `abs(centroid_latitude)` = "Abs. Latitude of Range Centroid",
    max_abn_ext_percent_site = "Percent of Site Abandoned"
    # max_abn_extent_div_site_area = "Proportion of Site Abandoned"
  )

regression_model_labels <- 
  list(
    Trophic_level = "Trophic Level",
    `log10(Body_mass_g)` = "log(Body Mass, g)",
    `log10(total_range_area)` = "log(Global Range Area, km2)",
    forest_occ = "Forest Suitability",
    savanna_occ = "Savanna Suitability",
    shrubland_occ = "Shrubland Suitability",
    grassland_occ = "Grassland Suitability",
    wetlands_occ = "Wetlands Suitability",
    rocky_occ = "Rocky Areas Suitability",
    caves_occ = "Caves Suitability",
    desert_occ = "Desert Suitability",
    artificial_terrestrial_occ = "Artificial Suitability",
    arable_occ = "Arable Land Suitability",
    urban_occ = "Urban Land Suitability",
    # n_suitable_habitats = "Number of Suitable Habitats",
    n_suitable_habitats_lvl2 = "Number of Suitable Habitats (IUCN Level 2)",
    # Habitat_breadth_IUCN = "Habitat Breadth (IUCN Level 2)",
    threatened = "Threatened (IUCN)",
    vert_class = "Vertebrate Class",
    `abs(centroid_latitude)` = "Abs. Latitude of Range Centroid",
    max_abn_ext_percent_site = "Percent of Site Abandoned"
    # max_abn_extent_div_site_area = "Proportion of Site Abandoned"
  )


regression_model_flextable_labels <- 
  c(
    forest_occTRUE = "Forest",
    savanna_occTRUE = "Savanna",
    shrubland_occTRUE = "Shrubland",
    grassland_occTRUE = "Grassland",
    wetlands_occTRUE = "Wetlands",
    rocky_occTRUE = "Rocky Areas",
    caves_occTRUE = "Caves",
    desert_occTRUE = "Desert",
    artificial_terrestrial_occTRUE = "Artificial",
    arable_occTRUE = "Arable Land",
    urban_occTRUE = "Urban Land",
    # n_suitable_habitats = "Number of Suitable Habitats",
    n_suitable_habitats_lvl2 = "Number of Suitable Habitats (IUCN Level 2)",
    # Habitat_breadth_IUCN = "Habitat Breadth (IUCN Level 2)",
    vert_class = "Vertebrate Class",
    vert_classBirds = "Bird",
    vert_classMammals = "Mammal",
    threatenedThreatened = "Threatened (IUCN)",
    Trophic_level = "Trophic Level",
    Trophic_levelCarnivore = "Carnivore",
    Trophic_levelHerbivore = "Herbivore",
    Trophic_levelOmnivore = "Omnivore",
    `log(Body_mass_g)` = "ln(Body Mass, g)",
    `log(total_range_area)` = "ln(Global Range Area, km2)", 
    `log10(Body_mass_g)` = "log(Body Mass, g)",
    `log10(total_range_area)` = "log(Global Range Area, km2)",
    `abs(centroid_latitude)` = "Abs. Latitude of Range Centroid",
    max_abn_ext_percent_site = "Percent of Site Abandoned",
    # max_abn_extent_div_site_area = "Proportion of Site Abandoned"
    "term" = "Term",
    "binary_estimate" = "OR",
    "percent_estimate" = "Beta", 
    "logratio_estimate" = "Beta", 
    "binary_p.value" = "p-value", 
    "percent_p.value" = "p-value", 
    "logratio_p.value" = "p-value",
    "logLik" = "Log-likelihood",
    "deviance" = "Deviance",
    "df.residual" = "Residual df",
    "nobs" = "No. Obs.",
    "null.deviance" = "Null deviance",
    "df.null" = "Null df",
    "percent_deviance_explained" = "Deviance explained (%)",
    "adj.r.squared" = "Adjusted R²",
    "sigma" = "Sigma"
  )
