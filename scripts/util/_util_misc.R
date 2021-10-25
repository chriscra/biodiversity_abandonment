# --------------------------------------------------------------- #
#
# Util miscellaneous
# 
# --------------------------------------------------------------- #

# --------------------------------------------------------------- #
# ----------------------- plotting colors ------------------------ 
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
