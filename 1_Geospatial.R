if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
if ("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")}
if ("terra" %in% rownames(installed.packages()) == FALSE) {install.packages("terra")}
if ("ggnewscale" %in% rownames(installed.packages()) == FALSE) {install.packages("ggnewscale")}
if ("spatialEco" %in% rownames(installed.packages()) == FALSE) {install.packages("spatialEco")}
if ("cowplot" %in% rownames(installed.packages()) == FALSE) {install.packages("cowplot")}
if ("viridis" %in% rownames(installed.packages()) == FALSE) {install.packages("viridis")}
if ("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
if ("ggspatial" %in% rownames(installed.packages()) == FALSE) {install.packages("ggspatial")}
if ("openxlsx2" %in% rownames(installed.packages()) == FALSE) {install.packages("openxlsx2")}
if ("RSAGA" %in% rownames(installed.packages()) == FALSE) {install.packages("RSAGA")}
if ("extrafont" %in% rownames(installed.packages()) == FALSE) {install.packages("extrafont")}

###### Geospatial Data (Author: Fin Malone; R version 4.4.1)
library(tidyverse)
library(sf)
library(terra)
library(ggnewscale)
library(spatialEco)
library(cowplot)
library(viridis)
library(patchwork)
library(ggspatial)
library(openxlsx2)
library(RSAGA)
library(extrafont)
# Text for all plots:
font_import()
##########################################
# Folder
work.dir = '/home/fmalone/Documents/Malone_et_al_2024'
# Catchment Outline
NFEC_outline = read_sf(paste0(work.dir,'/Geospatial/NFK_Elk_clip.shp'))
# Transform to polygon
NFEC_poly = st_polygonize(NFEC_outline)
# Crop the DEM raster to catchment area.
NFEC_DEM = crop(rast(paste0(work.dir,'/Geospatial/LEF_10m_DEM.tif')), NFEC_poly) %>% mask(.,NFEC_poly)
# set outline CRS to the same as DEM
st_crs(NFEC_outline) <- st_crs(NFEC_DEM)

# Montana shp
Montana = read_sf(paste0(work.dir, '/Geospatial/State_of_Montana__Boundary.shp'))
Montana = st_transform(Montana,crs = 4269)

# Hillslope Outlines
LESA_line = read_sf(paste0(work.dir,'/Geospatial/LESA_Outline.shp'))
LENA_line = read_sf(paste0(work.dir,'/Geospatial/LENA_Outline.shp'))
MESA_line = read_sf(paste0(work.dir,'/Geospatial/MESA_Outline.shp'))
MENA_line = read_sf(paste0(work.dir,'/Geospatial/MENA_Outline.shp'))
HESA_line = read_sf(paste0(work.dir,'/Geospatial/HESA_Outline.shp'))
HENA_line = read_sf(paste0(work.dir,'/Geospatial/HENA_Outline.shp'))

# Sites
sites = read_sf(paste0(work.dir,'/Geospatial/Site_Locations.shp')) %>%
  mutate(Position = case_when(stringr::str_sub(ID,6,6) %in% c(1,3,5) ~ 'Hollow',
                           stringr::str_sub(ID,6,6) %in% c(2,4,6) ~ 'Sideslope', T ~ 'Ridgeline'))

sites$Position = factor(sites$Position, levels = c('Hollow','Sideslope','Ridgeline')) # reorder for plot legend

LESA_ID = c('LESAt1','LESAt3','LESAt5','LESAt6')
LENA_ID = c('LENAt1','LENAt2','LENAt3','LENAt7')
MESA_ID = c('MESAt1','MESAt2','MESAt4','MESAt8','MESAt9')
MENA_ID = c('MENAt1','MENAt2','MENAt3','MENAt6')
HESA_ID = c('HESAt2','HESAt3','HESAt4','HESAt5','HESAt9')
HENA_ID = c('HENAt1','HENAt3','HENAt5','HENAt8','HENAt9')

allsites = append(LESA_ID, LENA_ID) %>%
  append(.,MESA_ID) %>% append(.,MENA_ID) %>% append(.,HESA_ID) %>% append(.,HENA_ID)

# Filter Sites
usedsites = sites %>% filter(ID %in% allsites)


####### Calculate hillshade and index values ######
NFEC_slope = terrain(NFEC_DEM, 'slope', unit = 'radians')
NFEC_aspect = terrain(NFEC_DEM, 'aspect', unit = 'radians')
NFEC_hill = shade(NFEC_slope, NFEC_aspect, 40, 270)
NFEC_aspect = trasp(NFEC_DEM) # Topographic Solar Radiation Index (TSRI)

# Topographic Position Index for Supplimentary analysis
NFEC_TPI = tpi(NFEC_DEM, win = 'circle', scale = 150)

### TWI via SAGA GIS:
#########################################################################################################
# Requires SAGA GIS and RSAGA
# SAGA GIS version 9.5.1 was used in our analysis

env = rsaga.env() # Establish SAGA path (you may need to define path argument, depending on the location of saga and your OS)

# Convert NFEC DEM to SAGA format
writeRaster(NFEC_DEM, paste0(work.dir,'/Geospatial/NFEC_DEM.sdat'), filetype = "SAGA", overwrite = TRUE)

suppressWarnings({
# 1. Calculate Slope
slope_file <- tempfile(fileext = ".sgrd")
rsaga.geoprocessor(
  lib = "ta_morphometry",
  module = "Slope, Aspect, Curvature",
  param = list(ELEVATION = paste0(work.dir,'/Geospatial/NFEC_DEM.sgrd'), SLOPE = slope_file),
  env = env
)
# 2. Calculate Upslope Flow Accumulation
flow_acc_file <- tempfile(fileext = ".sgrd")
rsaga.geoprocessor(
  lib = "ta_hydrology",
  module = "Flow Accumulation (Top-Down)",
  param = list(ELEVATION = paste0(work.dir,'/Geospatial/NFEC_DEM.sgrd'), FLOW = flow_acc_file, LINEAR_MIN = 0),
  env = env
)

# 3. Calculate TWI
twi_file <- tempfile(fileext = ".sgrd")
rsaga.geoprocessor(
  lib = "ta_hydrology",
  module = "Topographic Wetness Index (TWI)",
  param = list(SLOPE = slope_file, AREA = flow_acc_file, CONV = 1, METHOD = 1, TWI = twi_file),
  env = env
)
# 4. Export TWI to a TIFF
rsaga.geoprocessor(
  lib = "io_gdal",
  module = "Export GeoTIFF",
  param = list(GRIDS = twi_file, FILE = paste0(work.dir,'/Geospatial/NFEC_10m_TWI.tif')),
  env = env
)
})

NFEC_TWI = rast(paste0(work.dir,'/Geospatial/NFEC_10m_TWI.tif')) # Import as a SpatRaster
##########################################################################################################
############################ Export Geospatial Data

pre_site_data = vect(usedsites)
# Extract point values
pre_site_elev = extract(NFEC_DEM, pre_site_data, xy = T)
pre_site_aspect = extract(NFEC_aspect, pre_site_data, xy = T)
pre_site_TWI = extract(NFEC_TWI, pre_site_data, xy = T)

pre_site_data = usedsites %>% dplyr::select(ID) %>%
  mutate(site = gsub('t','',ID),
         Elevation = pre_site_elev$LEF_10m_DEM,
         Aspect = pre_site_aspect$lyr.1,
         TWI = pre_site_TWI$NFEC_10m_TWI,
         x = pre_site_data$POINT_X,
         y = pre_site_data$POINT_Y,
         ID = site) %>%
  as.data.frame(.) %>% 
  dplyr::select(ID, Elevation, Aspect, TWI, x, y) %>% arrange(.,ID)

wb_save(wb = wb_workbook(creator = 'Fin Malone') %>%
          wb_add_worksheet(sheet = "pre_site_data") %>%
          wb_add_data(x = as.data.frame(pre_site_data), col_names = T),
        file = paste0(work.dir,'/_3_geospatial_data_thesis.xlsx'))


################################## Crop TWI to Hillslope and define outlines

# LESA TWI Zoom-in
LESA_raster = crop(NFEC_TWI, sites %>% filter(Hillslope == 'LESA'))
LESA_ext = extend(ext(LESA_raster), c(100,105))
LESA_raster = crop(NFEC_TWI,LESA_ext)

outline_ext = LESA_ext
outline_ext[3] = outline_ext[3] + 5
outline_ext[4] = outline_ext[4] + 5
LESA_outline = as.polygons(outline_ext)
LESA_outline = st_as_sf(LESA_outline) %>% st_cast("LINESTRING")
st_crs(LESA_outline) <- st_crs(LESA_raster)



# LENA TWI Zoom-in
LENA_raster = crop(NFEC_TWI, sites %>% filter(Hillslope == 'LENA'))
LENA_ext = extend(ext(LENA_raster), c(100,80))
LENA_raster = crop(NFEC_TWI,LENA_ext)

outline_ext = LENA_ext
LENA_outline = as.polygons(outline_ext)
LENA_outline = st_as_sf(LENA_outline) %>% st_cast("LINESTRING")
st_crs(LENA_outline) <- st_crs(LESA_raster)


# MESA TWI Zoom-in
MESA_raster = crop(NFEC_TWI, sites %>% filter(Hillslope == 'MESA'))
MESA_ext = extend(ext(MESA_raster), c(165,100))
MESA_raster = crop(NFEC_TWI,MESA_ext)

outline_ext = MESA_ext
outline_ext[1] = outline_ext[1] + 5
outline_ext[2] = outline_ext[2] + 5
MESA_outline = as.polygons(outline_ext)
MESA_outline = st_as_sf(MESA_outline) %>% st_cast("LINESTRING")
st_crs(MESA_outline) <- st_crs(LESA_raster)

# MENA TWI Zoom-in
MENA_raster = crop(NFEC_TWI, sites %>% filter(Hillslope == 'MENA'))
MENA_ext = extend(ext(MENA_raster), c(110,100))
MENA_raster = crop(NFEC_TWI,MENA_ext)

outline_ext = MENA_ext
MENA_outline = as.polygons(outline_ext)
MENA_outline = st_as_sf(MENA_outline) %>% st_cast("LINESTRING")
st_crs(MENA_outline) <- st_crs(LESA_raster)

# HESA TWI Zoom-in
HESA_raster = crop(NFEC_TWI, sites %>% filter(Hillslope == 'HESA'))
HESA_ext = extend(ext(HESA_raster), c(130,100))
HESA_raster = crop(NFEC_TWI,HESA_ext)

outline_ext = HESA_ext
HESA_outline = as.polygons(outline_ext)
HESA_outline = st_as_sf(HESA_outline) %>% st_cast("LINESTRING")
st_crs(HESA_outline) <- st_crs(LESA_raster)

# HENA TWI Zoom-in
HENA_raster = crop(NFEC_TWI, sites %>% filter(Hillslope == 'HENA'))
HENA_ext = extend(ext(HENA_raster), c(100,125))
HENA_raster = crop(NFEC_TWI,HENA_ext)

outline_ext = HENA_ext
outline_ext[3] = outline_ext[3] + 5
outline_ext[4] = outline_ext[4] + 5
HENA_outline = as.polygons(outline_ext)
HENA_outline = st_as_sf(HENA_outline) %>% st_cast("LINESTRING")
st_crs(HENA_outline) <- st_crs(LESA_raster)


# Calculate the minimum and maximum TWI values across all six maps

min_TWI <- min(c(values(LESA_raster),values(LENA_raster),values(MESA_raster),
                 values(MENA_raster),values(HESA_raster),values(HENA_raster)))
max_TWI <- max(c(values(LESA_raster),values(LENA_raster),values(MESA_raster),
                 values(MENA_raster),values(HESA_raster),values(HENA_raster)))

# LESA
p2 = ggplot(data = as.data.frame(LESA_raster, xy = T)) +
  geom_tile(aes(x, y, fill = NFEC_10m_TWI)) +
  scale_fill_viridis(option = 'H', limits = c(min_TWI, 12), direction = -1, oob = scales::squish, breaks = c(2.5,5,7.5,10)) +
  geom_sf(data = sites %>% filter(Hillslope == 'LESA'), fill = 'white', size = 2,
          aes(shape = Position, color = ID %in% allsites)) +
  geom_sf(data = LESA_outline, size = 1.5, color = '#0900ff') +
  scale_shape_manual(values = c('Hollow' = 21, 'Sideslope' = 22, 'Ridgeline' = 24)) +
  scale_color_manual(values = c('black','red')) +
  guides(color = 'none', shape = 'none', fill = guide_colorbar('TWI', frame.colour = 'black', ticks.colour = 'black')) +
  theme_void() +
  theme(text = element_text(family = 'Liberation Sans'),
        legend.key.size = unit(0.15, "in"),
        legend.position = c(3.15, 0.05),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

# LENA
p3 = ggplot(data = as.data.frame(LENA_raster, xy = T)) +
  geom_tile(aes(x, y, fill = NFEC_10m_TWI)) +
  scale_fill_viridis(option = 'H', limits = c(min_TWI, 12), direction = -1, oob = scales::squish) +
  geom_sf(data = sites %>% filter(Hillslope == 'LENA'), fill = 'white', size = 2,
          aes(shape = Position, color = ID %in% allsites)) +
  geom_sf(data = LENA_outline, size = 1.5, color = '#0900ff') +
  scale_shape_manual(values = c('Hollow' = 21, 'Sideslope' = 22, 'Ridgeline' = 24)) +
  scale_color_manual(values = c('black','red')) +
  guides(color = 'none', fill = 'none') +
  theme_void() +
  theme(text = element_text(family = 'Liberation Sans'),
        legend.key.size = unit(0.15, "in"),
        legend.position = c(2, 3.05),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

# MESA
p4 = ggplot(data = as.data.frame(MESA_raster, xy = T)) +
  geom_tile(aes(x, y, fill = NFEC_10m_TWI)) +
  scale_fill_viridis(option = 'H', limits = c(min_TWI, 12), direction = -1, oob = scales::squish) +
  geom_sf(data = sites %>% filter(Hillslope == 'MESA'), fill = 'white', size = 2,
          aes(shape = Position, color = ID %in% allsites)) +
  geom_sf(data = MESA_outline, size = 1.5, color = '#0900ff') +
  scale_shape_manual(values = c('Hollow' = 21, 'Sideslope' = 22, 'Ridgeline' = 24)) +
  scale_color_manual(values = c('black','red')) +
  guides(color = 'none', fill = 'none', shape = 'none') +
  theme_void()

# MENA
p5 = ggplot(data = as.data.frame(MENA_raster, xy = T)) +
  geom_tile(aes(x, y, fill = NFEC_10m_TWI)) +
  scale_fill_viridis(option = 'H', limits = c(min_TWI, 12), direction = -1, oob = scales::squish) +
  geom_sf(data = sites %>% filter(Hillslope == 'MENA'), fill = 'white', size = 2,
          aes(shape = Position, color = ID %in% allsites)) +
  geom_sf(data = MENA_outline, size = 1.5, color = '#0900ff') +
  scale_shape_manual(values = c('Hollow' = 21, 'Sideslope' = 22, 'Ridgeline' = 24)) +
  scale_color_manual(values = c('black','red')) +
  guides(color = 'none', fill = 'none', shape = 'none') +
  theme_void()

# HESA
p6 = ggplot(data = as.data.frame(HESA_raster, xy = T)) +
  geom_tile(aes(x, y, fill = NFEC_10m_TWI)) +
  scale_fill_viridis(option = 'H', limits = c(min_TWI, 12), direction = -1, oob = scales::squish) +
  geom_sf(data = sites %>% filter(Hillslope == 'HESA'), fill = 'white', size = 2,
          aes(shape = Position, color = ID %in% allsites)) +
  geom_sf(data = HESA_outline, size = 1.5, color = '#0900ff') +
  scale_shape_manual(values = c('Hollow' = 21, 'Sideslope' = 22, 'Ridgeline' = 24)) +
  scale_color_manual(values = c('black','red')) +
  guides(color = 'none', fill = 'none', shape = 'none') +
  theme_void()

# HENA
p7 = ggplot(data = as.data.frame(HENA_raster, xy = T)) +
  geom_tile(aes(x, y, fill = NFEC_10m_TWI)) +
  scale_fill_viridis(option = 'H', limits = c(min_TWI, 12), direction = -1, oob = scales::squish) +
  geom_sf(data = sites %>% filter(Hillslope == 'HENA'), fill = 'white', size = 2,
          aes(shape = Position, color = ID %in% allsites)) +
  geom_sf(data = HENA_outline, size = 1.5, color = '#0900ff') +
  scale_shape_manual(values = c('Hollow' = 21, 'Sideslope' = 22, 'Ridgeline' = 24)) +
  scale_color_manual(values = c('black','red')) +
  guides(color = 'none', fill = 'none', shape = 'none') +
  theme_void()


# Add Montana Map

point = st_point(c(-113.335,46.879)) 
point = st_sfc(point, crs = st_crs(Montana))

p8 = ggplot(data = Montana) +
  geom_sf(fill = 'white') +
  geom_sf(data = point) +
  geom_sf_label(data = point, label = '46.879, -113.335', nudge_x = 3, nudge_y = 1, size = 2.5, label.size = NA) +
  theme_void() +
  theme(text = element_text(family = 'Liberation Sans'))


# Middle Map of NFEC

p1 = ggplot(data = NFEC_outline) +
  # Hillshade and Elevation
  geom_tile(data = as.data.frame(NFEC_hill, xy = T), aes(x, y, fill = hillshade)) +
  scale_fill_gradient(high = 'white', low = 'black') +
  guides(fill = 'none') +
  new_scale_fill() +
  geom_tile(data = as.data.frame(NFEC_DEM, xy = T) %>% na.omit(), aes(x, y, fill = LEF_10m_DEM), alpha = 0.6) +
  scale_fill_viridis(begin = 1, end = 0,
                     limits = c(NA,2100), breaks = c(1250,1500,1750,2000)) +
  geom_sf() +
  # Site points
  geom_sf(data = sites, size = 1.5, fill = 'white', aes(shape = Position, color = ID %in% allsites)) +
  scale_shape_manual(values = c('Hollow' = 21, 'Sideslope' = 22, 'Ridgeline' = 24)) +
  scale_color_manual(values = c('black','red')) +
  # Hillslope zoom-ins
  geom_sf(data = LESA_outline, color = '#0900ff') +
  geom_sf(data = LENA_outline, color = '#0900ff') +
  geom_sf(data = MESA_outline, color = '#0900ff') +
  geom_sf(data = MENA_outline, color = '#0900ff') +
  geom_sf(data = HESA_outline, color = '#0900ff') +
  geom_sf(data = HENA_outline, color = '#0900ff') +
  # Hillslope outlines
  geom_sf(data = LESA_line) +
  geom_sf(data = LENA_line) +
  geom_sf(data = MESA_line) +
  geom_sf(data = MENA_line) +
  geom_sf(data = HESA_line) +
  geom_sf(data = HENA_line) +
  # Legends, etc.
  guides(fill = guide_colorbar('Elevation (m)', frame.colour = 'black', ticks.colour = 'black'), shape = F, color = F) +
  theme_void() +
  
  # Add compass and scale
  annotation_north_arrow(aes(location = 'tl'),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme(text = element_text(family = 'Liberation Sans'),
        legend.key.size = unit(0.15, "in"),
        legend.position = c(1.03, 1.19),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  annotation_scale(plot_unit = "km")

layout = '
BDF
BDF
AAA
AAA
AAA
AAA
CEG
CEG'

NFEC_Map <- p1 + p2 + p3 + p4 + p5 + p6 + p7 +
  plot_layout(design = layout)#, guides = 'collect')


Final_Map = ggdraw(NFEC_Map) + draw_plot(p8, width = 0.2, height = 0.2, x = 0.06, y = .57)


ggsave(filename = paste0(work.dir,'/Figures/Figure_1.tiff'),
       plot = Final_Map,
       dpi = 805, units = "in", width = 6, height = 6)


