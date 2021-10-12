# Abandonment and Biodiversity: exploring the biodiversity implications of cropland abandonment
## The potential effects of cropland abandonment on biodiversity and carbon sequestration

This repository houses analysis scripts and documentation for:

> Crawford C.L.\*, Senior, R.A., Yin, H., Radeloff, V.C., and Wilcove, D.S. Year. Title. *Journal Status.*

\*@chriscra, ccrawford@princeton.edu, Robertson Hall, Princeton University, Princeton, NJ

In this project, we did the following: TBD must update.


The annual land cover maps (1987-2017, 30 meter resolution) that underlie our analysis were developed on Google Earth Engine using publicly available Landsat satellite imagery ([Yin et al. 2020, *Remote Sensing of Environment*](https://doi.org/10.1016/j.rse.2020.111873)).
These data are archived and publicly available at Zenodo (URL TBD), along with other datasets produced during the analysis.
Primary derived data products include maps of abandonment age and datasets describing the area abandoned (in hectares) at each site in a given year, separated by year of initial abandonment ("cohort"). 

This is a living repository. See the public release and Zenodo archive for the code used at time of publication. 

## Components of this repository

This repository has one primary directory housing the scripts used in our analysis.
The "scripts" directory contains a handful of working scripts along with two other directories: "cluster" and "util." 

The working R scripts and Rmarkdown files are numbered based on the general order in which parts of the analysis were conducted. 
Note that the `.Rmd` scripts are intended to be run interactively chunk by chunk (sometimes line by line), not knit together all at once. 
They also include code for data visualization and exploratory data analysis; therefore, not all code is strictly required. 
Some chunks produce and save data files, which other chunks sometimes reload and use for further analysis or visualization.

These files include:

- **0_Start.R** serves as the starting place for all scripts, loading required packages and custom functions.
- **1_summary_stats.Rmd** manipulates and summarizes the data analysis products, producing summary datasets and figures for the manuscript.
- **2_decay_models.Rmd** contains the code for developing linear models of the decay of abandoned cropland, or the process through which abandoned land is recultivated over time following initial abandonment.
- **3_figures.Rmd** contains code for exploratory data analysis and visualization, including the production of more figures for the manuscript and supporting information.
- **Crawford_abandonment_MS+SI.Rmd** contains the text of the main text of the manuscript as well as the supporting information. Data is loaded and manipulated directly and programmatically in this document, and the document requires access to data files and figures as a result to knit correctly.
- **Crawford_abandonment_SI.Rmd** contains only the supporting information text, tables, and figures.
- **preamble.tex** contains common LaTeX code for both the manuscript and the supporting information file.


### cluster scripts

The "cluster" folder contains scripts performing much of the heavy-lifting of the analysis of the land cover time series, which was conducted on Princeton's High Performance Computing cluster system. 
Each R script is accompanied by a "slurm" script, which sets the parameters for each scripts cluster usage through the Slurm scheduling system:

- **0_cluster_packages.R** loads R packages required for cluster analysis.
- **1_prep_r_to_dt.R** merges, recodes, and preps land cover rasters for eleven sites from Yin et al. 2020, saving them as data.tables for further analysis.
- **2_analyze_abn.R** is the most important analysis script for this work, taking the prepped land cover time series from **1_prep_r_to_dt.R** and applying a series of custom functions to it, filtering the time series to only those pixels that were abandoned croplands, calculating their age in each year, calculating the maximum age throughout the time series for each pixel, saving a map of the age of abandoned cropland as both rasters and data.tables, and finally, extracting summary statistics from these abandonment data products.
- **2.1_summarize_abn_dts_only.R** performs *only* the final step of the previous script (**2_analyze_abn.R**), extracting a range of summary statistics for each site based on the derived maps of abandonment.
- **3_distill_lengths.R** takes raw data.tables of the length of each abandonment period (for each pixel) observed during the time series at each site, and distills these tens of millions of pixels down to the frequency of each length at each site. 
- **4_lc_of_abn.R** calculates the proportion of abandoned cropland (as of 2017) that is classified as woody vegetation vs. grassy (herbaceous) vegetation. This results in summary files "abn_lc_count_2017" & "abn_prop_lc_2017," along with a figure "abn_prop_lc_2017."


### util scripts

The "util" folder contains scripts that include custom analysis functions and pathnames for managing the project:

- **_util_functions.R** contains custom functions written by Christopher Crawford, which underlie the bulk of this land cover time series analysis. These functions accomplish a variety of tasks, including: cleaning, filtering, and organizing data; calculating abandonment age; extracting area, persistence, and turnover statistics from abandonment maps; saving plots; and projecting abandonment into the future.
- **_util_misc.R** includes miscellaneous functions for development, analysis, and plotting.
- **_util_pathnames.R** includes user-specific pathnames for managing the project, which are used throughout the rest of the analysis scripts. Note, however, that **cluster** scripts make use of alternative pathnames (specific to the Princeton high-performance computing clusters used for the analysis), as specified in each script.
- **_util_master.R** runs the four "util" scripts described above.
