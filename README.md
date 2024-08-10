# Effects of cropland abandonment on biodiversity
## Cropland abandonment benefits most birds and mammals but rarely compensates for habitat loss

This repository houses analysis scripts and documentation for:

> Crawford CL\*, Wiebe RA, Yin H, Radeloff VC, and Wilcove DS. 2024. Effects of cropland abandonment on biodiversity. *Nature Sustainability*.

\*@chriscra, chris.L.crawford@gmail.com, School of Public & International Affairs, Robertson Hall, Princeton University, Princeton, NJ

This project builds on [Crawford et al. 2022](https://doi.org/10.1126/sciadv.abm8999) (https://github.com/chriscra/abandonment_trajectories) and [Yin et al. 2020](https://doi.org/10.1016/j.rse.2020.111873) to assess the impact of cropland abandonment on the area of habitat for over 1,322 species of birds and mammals at 11 sites across 4 continents.
We combined annual maps of cropland abandonment with [IUCN](https://www.iucnredlist.org/) habitat and elevation preferences and a map of IUCN habitat types [Jung et al. 2020](https://www.nature.com/articles/s41597-020-00599-8) to estimate the area of habitat for each species in each year from 1987-2017.
We then use simple linear models to assess habitat trends for each species, and further investigate how these trends are associated with various species traits.
Our primary analysis isolates the effect of abandonment on habitat by calculating AOH provided by abandoned croplands during cultivation and subsequent abandonment (and recultivation, where applicable). 
In order to place the habitat effects of abandonment into the broader context of land cover changes at each site, we also calculate 1) the net habitat change in croplands that were abandoned at some point from 1987 to 2017 (therefore factoring in habitat that was cleared for croplands prior to abandonment), and 2) the net habitat change across the full extent of each site from 1987-2017 (factoring in habitat loss that occurred elsewhere at the site).
Following [Crawford et al. 2022](https://doi.org/10.1126/sciadv.abm8999), we also calculate the potential AOH under a scenario in which no recultivation took place, in order to quantify the benefit of reducing recultivation rates.

The annual land cover maps (1987-2017, 30-meter resolution) that underlie our analysis were developed on Google Earth Engine using publicly available Landsat satellite imagery ([Yin et al. 2020, *Remote Sensing of Environment*](https://doi.org/10.1016/j.rse.2020.111873)).
These data are archived and publicly available at Zenodo ([DOI: 10.5281/zenodo.5348287](https://doi.org/10.5281/zenodo.5348287)), along with other datasets produced for [Crawford et al. 2022](https://doi.org/10.1126/sciadv.abm8999).

The derived and supporting data for this project are archived on Zenodo at (DOI TBD).

This is a living repository.
See the public release and Zenodo archive for the code used at time of publication. 

## Components of this repository

This repository contains one primary directory housing the scripts used in our analysis.
The "scripts" directory contains the primary working scripts for the project and two additional directories: "cluster" and "util." 

The working R scripts and Rmarkdown files are numbered based on the general order in which parts of the analysis were conducted. 
Note that the `.Rmd` scripts are intended to be run interactively chunk by chunk (sometimes line by line), not knit together all at once. 
They also include code for data visualization and exploratory data analysis; not all code is strictly required. 
Some chunks produce and save data files, which other chunks sometimes reload and use for further analysis or visualization.

These files include:

- **0_start.R** serves as the starting place for all scripts, loading required packages and custom functions.
- **habitats.Rmd**
- **IUCN.Rmd**
- **AOH.Rmd**
- **traits.Rmd**
- **figures.Rmd**
- **biodiv_abn_MS.Rmd** contains the text of the main text of the manuscript as well as the supporting information. Data is loaded and manipulated directly and programmatically in this document, and the document requires access to data files and figures as a result to knit correctly.
- **pnv_of_abn.R**
- **preamble.tex** contains common LaTeX code for both the manuscript and the supporting information file.


### cluster scripts

The "cluster" folder contains scripts performing much of the heavy-lifting of the analysis on Princeton's High Performance Computing cluster system, primarily calculations of Area of Habitat for each species at each site over time.
Most of these R scripts are accompanied by one or more "slurm" scripts (e.g., "calc_range_area.R" accompanied by "calc_range_area.slurm"), which set the parameters for processing each script on PRinceton's HPC clusters through the Slurm scheduling system:

- Area of Habitat (AOH) calculations.  
    - **aoh.R** calculates Area of Habitat for a set of species at a given site for a range of different calculations and scenarios ("aoh1.slurm" through "aoh11.slurm" correspond to 11 different AOH calculations, as described in the script; the analysis in Crawford et al. 2024 focuses on indices 9 [Calc. 1a], 10 [Calc. 1b], 6 [Calc. 2a], 8 [Calc. 2b], 4 [Calc. 3a], and 11 [Calc. 3b]). This relies on maps of land cover (Yin et al. 2020) interpolated to IUCN Level 2 Habitat Types based on the June et al. (2020) map for the year 2015. This script is designed to run in parallel on an array of nodes for high performance.
    - **aoh_terra_jung.R** calculates the Area of Habitat for each species at each site using the IUCN Level 2 habitat map for the year 2015 from Jung et al. (2020).

- Input Data Preparation.  
    - **noncrop_precrop_mask.R** produces the final input maps for Calculations 1a and 1b ("crop_abn_iucn" observed and potential), by masking out noncrop land covers / habitat types that occurred prior to cultivation in pixels that experienced abandonment at some point during the time series.
    - **potential_full_iucn.R** produces rasters with IUCN Habitat types at each site for a scenario in which no recultivation took place (i.e., all abandoned croplands remained abandoned through the end of the time series). These data feed into one of the calculations in **aoh.R**.
    - **resample_iucn.R**

- Validate, crop, and save bird and mammal ranges from BirdLife International and IUCN respectively.  
    - **calc_range_area.R**
    - **crop_mammal_ranges.R**
    - **crop_sp_ranges_array.R**
    - **merge_crop_bird_ranges.R**
    - **save_bird_ranges_as_blocks.R**
    - **validate_bird_ranges_array.R**

- Clean and save data.tables and rasters (SpatRasters).  
    - **remove_0s.R**
    - **save_spatraster_as_dt.R** saves {terra} SpatRasters as data.tables {data.table}.
    - **save_parquet.R** saves data.tables (.csv files on disk, read using {data.table}) as .parquet files (using {arrow}), which take up much less space on disk. 
    - **troubleshooting.R**

- Calculate fragmentation statistics using the {landscapemetrics} package.  
    - **lsm_2024.R**
    - **lsm_two_2024.R**


### util scripts

The "util" folder contains scripts that include custom analysis functions and pathnames for managing the project:

- **_util_files.R** loads all of the relevant input and derive data needed for the manuscript .Rmd file. 
- **_util_functions.R** contains custom functions written by Christopher Crawford, which underlie the bulk of these analyses and are used throughout the scripts above. These functions accomplish a variety of tasks, including: cleaning, filtering, and organizing data; calculating...
- **_util_main.R** includes user-specific switches (e.g., "run_label") and pathnames for managing the project, which are used throughout the rest of the analysis scripts. Note, however, that **cluster** scripts make use of alternative pathnames (specific to the Princeton high-performance computing clusters used for the analysis), as specified in each script.
