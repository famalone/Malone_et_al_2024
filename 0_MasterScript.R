
# Master R Script (Author: Fin Malone; R version 4.4.1)


# Run scripts to produce data transformation, analysis, and figures
work.dir = '/home/fmalone/Documents/Malone_et_al_2024'

source(paste0(work.dir,'/1_Geospatial.R'), echo = TRUE)
rm(list = ls())

work.dir = '/home/fmalone/Documents/Malone_et_al_2024'

source(paste0(work.dir,'/2_Fit_Growth_Curves.R'), echo = TRUE)
rm(list = ls())

work.dir = '/home/fmalone/Documents/Malone_et_al_2024'

source(paste0(work.dir,'/3_Analysis_FINAL.R'), echo = TRUE)
rm(list = ls())

work.dir = '/home/fmalone/Documents/Malone_et_al_2024'

source(paste0(work.dir,'/4_SI.R'), echo = TRUE)
rm(list = ls())
