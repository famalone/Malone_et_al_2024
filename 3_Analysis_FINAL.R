if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
if ("cowplot" %in% rownames(installed.packages()) == FALSE) {install.packages("cowplot")}
if ("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")}
if ("openxlsx2" %in% rownames(installed.packages()) == FALSE) {install.packages("openxlsx2")}
if ("viridis" %in% rownames(installed.packages()) == FALSE) {install.packages("viridis")}
if ("MASS" %in% rownames(installed.packages()) == FALSE) {install.packages("MASS")}
if ("visreg" %in% rownames(installed.packages()) == FALSE) {install.packages("visreg")}
if ("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")}
if ("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}

###### Data Transformation, Analysis, and Figures for Main Text (Author: Fin Malone; R version 4.4.1)
library(tidyverse)
library(cowplot)
library(readxl)
library(openxlsx2)
library(viridis)
library(MASS)
library(visreg)
library(gridExtra)
library(patchwork)


###################################### ALL DATA INPUT AND TRANSFORMATIONS ################################################

work.dir = '/home/fmalone/Documents/Malone_et_al_2024' # Folder Directory


# Precipitation data from the Lubrecht Flume SNOTEL station.
Lflume <- readxl::read_excel(paste0(work.dir,'/_2_lubrecht_data_thesis.xlsx'),
                             sheet = 'lubrecht_precip_daily', col_names = T) %>%
  mutate(Day = as.Date(Day,    # Convert Julian day to date
                       origin = as.Date("2016-01-01")))

# Geospatial and Cessation Data
site_data = readxl::read_excel(paste0(work.dir,'/_4_processed_data_thesis.xlsx'),
                               sheet = 'post_curve_data', col_names = T)

# Daily environmental data and growth curves.
dendro_data <- readxl::read_excel(paste0(work.dir,'/_4_processed_data_thesis.xlsx'),
                                  sheet = 'time_series_daily_and_curves', col_names = T) %>%
  mutate(topo = case_when(as.numeric(stringr::str_sub(ID,-1)) %in% c(1, 3, 5) ~ 'Hollow', # looks at site ID to determine topographic position,
                                            T ~ 'Upslope'),                               # aspect, and elevation types.
         elev = case_when(stringr::str_sub(ID,-5,-5) %in% 'L' ~ 'Low',
                               stringr::str_sub(ID,-5,-5) %in% 'M' ~ 'Middle', T ~ 'High'),
         aspect = case_when(stringr::str_sub(ID,-3,-3) %in% 'N' ~ 'North', T ~ 'South'),
         TWI = NA,
         cessation_day_gomp = NA,
         mean_TDR = (mean_TDR1 + mean_TDR2) / 2, mean_TDR_T = (mean_TDR1_T + mean_TDR2_T) / 2, # average soil temp and moisture using both depths.
         day = as.Date(day,    # Convert Julian day to date
                       origin = as.Date("2016-01-01")))


# Average site conditions for entire season
averages <- dendro_data %>% filter(month(day) %in% c(5,6,7,8)) %>% # Standardizes the time period of environmental data to include only months where all sites had
                                                                   # beginning and ending measurements (equipment was installed and taken down at different times)
  group_by(ID) %>% dplyr::summarise(avg_temp = mean(mean_temp, na.rm = T),
                                    avg_soil_temp05 = mean(mean_TDR1_T, na.rm = T),
                                    avg_soil_temp50 = mean(mean_TDR2_T, na.rm = T),
                                    avg_VPD = mean(mean_VPD, na.rm = T),
                                    avg_TDR1 = mean(mean_TDR1, na.rm = T),
                                    avg_TDR2 = mean(mean_TDR2, na.rm = T))

# Monthly averages used in analysis of temporal differences in microclimatic organization.

may_averages <- dendro_data %>% filter(month(day) %in% c(5)) %>% # May
  group_by(ID) %>% dplyr::summarise(may_avg_temp = mean(mean_temp, na.rm = T),
                                    may_avg_soil_temp05 = mean(mean_TDR1_T, na.rm = T),
                                    may_avg_soil_temp50 = mean(mean_TDR2_T, na.rm = T),
                                    may_avg_VPD = mean(mean_VPD, na.rm = T),
                                    may_avg_TDR1 = mean(mean_TDR1, na.rm = T),
                                    may_avg_TDR2 = mean(mean_TDR2, na.rm = T))

june_averages <- dendro_data %>% filter(month(day) %in% c(6)) %>% # June
  group_by(ID) %>% dplyr::summarise(june_avg_temp = mean(mean_temp, na.rm = T),
                                    june_avg_soil_temp05 = mean(mean_TDR1_T, na.rm = T),
                                    june_avg_soil_temp50 = mean(mean_TDR2_T, na.rm = T),
                                    june_avg_VPD = mean(mean_VPD, na.rm = T),
                                    june_avg_TDR1 = mean(mean_TDR1, na.rm = T),
                                    june_avg_TDR2 = mean(mean_TDR2, na.rm = T))

july_averages <- dendro_data %>% filter(month(day) %in% c(7)) %>% # July
  group_by(ID) %>% dplyr::summarise(july_avg_temp = mean(mean_temp, na.rm = T),
                                    july_avg_soil_temp05 = mean(mean_TDR1_T, na.rm = T),
                                    july_avg_soil_temp50 = mean(mean_TDR2_T, na.rm = T),
                                    july_avg_VPD = mean(mean_VPD, na.rm = T),
                                    july_avg_TDR1 = mean(mean_TDR1, na.rm = T),
                                    july_avg_TDR2 = mean(mean_TDR2, na.rm = T))

aug_averages <- dendro_data %>% filter(month(day) %in% c(8)) %>% # August
  group_by(ID) %>% dplyr::summarise(aug_avg_temp = mean(mean_temp, na.rm = T),
                                    aug_avg_soil_temp05 = mean(mean_TDR1_T, na.rm = T),
                                    aug_avg_soil_temp50 = mean(mean_TDR2_T, na.rm = T),
                                    aug_avg_VPD = mean(mean_VPD, na.rm = T),
                                    aug_avg_TDR1 = mean(mean_TDR1, na.rm = T),
                                    aug_avg_TDR2 = mean(mean_TDR2, na.rm = T))

# Add average conditions to site data
site_data <- left_join(site_data, averages, by = 'ID')
site_data <- left_join(site_data, may_averages, by = 'ID') # May
site_data <- left_join(site_data, june_averages, by = 'ID') # June
site_data <- left_join(site_data, july_averages, by = 'ID') # July
site_data <- left_join(site_data, aug_averages, by = 'ID') # August

# Average for two soil depths
site_data = site_data %>% mutate(avg_soil_temp = (avg_soil_temp05 + avg_soil_temp50)/2,
                                 avg_TDR = (avg_TDR1 + avg_TDR2)/2,
                                 
                                 may_avg_soil_temp = (may_avg_soil_temp05 + may_avg_soil_temp50)/2,
                                 may_avg_TDR = (may_avg_TDR1 + may_avg_TDR2)/2,
                                 
                                 june_avg_soil_temp = (june_avg_soil_temp05 + june_avg_soil_temp50)/2,
                                 june_avg_TDR = (june_avg_TDR1 + june_avg_TDR2)/2,
                                 
                                 july_avg_soil_temp = (july_avg_soil_temp05 + july_avg_soil_temp50)/2,
                                 july_avg_TDR = (july_avg_TDR1 + july_avg_TDR2)/2,
                                 
                                 aug_avg_soil_temp = (aug_avg_soil_temp05 + aug_avg_soil_temp50)/2,
                                 aug_avg_TDR = (aug_avg_TDR1 + aug_avg_TDR2)/2,
                                 HDI = avg_VPD/avg_TDR*-1)

# Add TWI and cessation day to the daily data for groupings.
for(i in 1:length(dendro_data$TWI)) {
  dendro_data$TWI[i] = site_data$TWI[which(dendro_data$ID[i] == site_data$ID)]
  dendro_data$cessation_day_gomp[i] = site_data$cessation_day_gomp[which(dendro_data$ID[i] == site_data$ID)]
}

dendro_data = dendro_data %>% arrange(desc(TWI)) # reorder by TWI for time series overlap.

###################### Figure Parameters (Color and Text)

# scale_color_viridis
alpha = 1
begin = 0.1
end = 0.75
# line thickness
linesize = 1.2
# font sizes
numsize = 10
labsize = 12
tagsize = 14
# geom_point
pointsize = 2

##################################################################################################################
############################### Figure 2 ###################################

# Precipitation Hyetograph
p0 <- ggplot() +
  geom_bar(data = Lflume, aes(x = Day, y = rain),stat = 'identity', fill = '#0072B2') +
  geom_bar(data = Lflume, aes(x = Day, y = melt),stat = 'identity', fill = 'lightblue') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8, family = 'Liberation Sans'), axis.title.y = element_text(margin=margin(r=4))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_reverse('Precipitation (mm)', expand = expansion(mult = c(0.05,0)), breaks = c(0,10,20)) +
  guides(color = FALSE)


# Air Temperature
p1 <- ggplot() +
  geom_line(data = dendro_data, aes(x = day, y = mean_temp, group = desc(TWI), color = TWI), size = .75) +
  ggtitle(NULL) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha, direction = -1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8, family = 'Liberation Sans'), axis.title.y = element_text(margin=margin(r=5))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_continuous('Air Temp. (\u00B0C)',expand = expansion(mult = c(0,0.05))) +
  guides(color = FALSE)


# Vapor Pressure Deficit
p2 <- ggplot() +
  geom_line(data = dendro_data, aes(x = day, y = mean_VPD*-1, group = desc(TWI), color = TWI), size = .75) +
  ggtitle(NULL) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha, direction = -1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8, family = 'Liberation Sans'), axis.title.y = element_text(margin=margin(r=2))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_continuous('VPD (-kPa)',expand = expansion(mult = c(0,0.05))) +
  guides(color = FALSE)


# Soil Temperature
p3 <- ggplot() +
  geom_line(data = dendro_data, aes(x = day, y = mean_TDR_T, group = desc(TWI), color = TWI), size = .75) +
  ggtitle(NULL) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha, direction = -1) +
  theme_bw() +
  guides(color = guide_colorbar(frame.colour = 'black', ticks.colour = 'black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.height = unit(.45, 'cm'),
        text = element_text(size = 8, family = 'Liberation Sans'), axis.title.y = element_text(margin=margin(r=5))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_continuous('Soil Temp. (\u00B0C)',expand = expansion(mult = c(0,0.05)))


# Soil Moisture
p4 <- ggplot() +
  geom_line(data = dendro_data, aes(x = day, y = mean_TDR, group = desc(TWI), color = TWI), size = .75) +
  ggtitle(NULL) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha, direction = -1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8, family = 'Liberation Sans'), axis.title.y = element_text(margin=margin(r=1.5))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_continuous('Soil Moisture (%)', expand = expansion(mult = c(0,0.05))) +
  guides(color = FALSE)


# Tree Growth
p5 <- ggplot() +
  geom_line(data = dendro_data,
            aes(x = day, y = gompvals/1000, group = desc(TWI), color = TWI), size = .75) +
  ggtitle(NULL) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha, direction = -1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8, family = 'Liberation Sans'), axis.title.y = element_text(margin=margin(r=4))) +
  scale_x_date(name = NULL, breaks = NULL, limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  scale_y_continuous('Radial Growth (mm)',expand = expansion(mult = c(0.02,0.05))) +
  guides(color = FALSE)


# Growth Cessation
p6 <- ggplot() +
  geom_vline(data = site_data, aes(xintercept = as.Date(cessation_day_gomp,    # Convert Julian day to date
                                                        origin = as.Date("2016-01-01")),
                                   group = desc(TWI), color = TWI), size = 2) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha, direction = -1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 8, family = 'Liberation Sans'), axis.title.y = element_text(margin=margin(r=18.5))) +
  scale_x_date(name = 'Time', limits = as.Date(c('2016-05-01', '2016-09-02')),
               expand = expansion(mult = c(0,0))) +
  guides(color = FALSE) +
  scale_y_continuous('Cessation Day')


# Combine all time series

timeseries <- plot_grid(p0,p4,p2,p1,p3,p5,p6, nrow = 7, rel_heights = c(1.3, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
                        axis = 'rl', align = 'v')

ggsave(filename = paste0(work.dir,'/Figures/Figure_2.tiff'),
       plot = timeseries,
       dpi = 800, units = "in", width = 6.5, height = 7.5)

#####################################################################################################################
############################### FIGURE 3 ####################################
# Daily Growth Curves

color = c('#D55E00','#009E73','#0072B2')
breaks = c("Low", "Middle", "High")

p1 <- ggplot(data = dendro_data %>% filter(topo == 'Hollow' & max_dendro > -500 & max_dendro < 3000) %>% # for visual purposes gets rid of values below -500 microns
               arrange(elev)) +
  geom_point(aes(x = day, y = max_dendro/1000, # convert to mm
                 color = elev),
             size = .5) +
  geom_line(aes(x = day, y = gompvals/1000), size = 1) + # convert to mm
  geom_vline(data = site_data %>% filter(topo == 'Hollow') %>% arrange(elev),
             aes(xintercept = as.Date(cessation_day_gomp,origin = as.Date("2016-01-01"))), size = 1,
             linetype = 'dashed') +
  xlab(NULL) +
  scale_color_manual(breaks = breaks, values = color) +
  scale_y_continuous(NULL,
                     limits = c(NA,NA)) +
  scale_x_date(limits = as.Date(c('2016-04-01','2016-11-01'))) +
  facet_wrap(~ factor(ID, levels = c('LENA1','LENA2','LENA3','LENA7',
                                     'LESA1','LESA3','LESA5','LESA6',
                                     'MENA1','MENA2','MENA3','MENA6',
                                     'MESA1','MESA2','MESA4','MESA8','MESA9',
                                     'HENA1','HENA3','HENA5','HENA8','HENA9',
                                     'HESA2','HESA3','HESA4','HESA5','HESA9')), scales = 'free_y', ncol = 2) +
  ggtitle('Hollow') +
  theme_bw() +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = 9),
        strip.text.x = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = numsize),
        plot.tag = element_text(size = tagsize),
        plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data = dendro_data %>% filter(!(topo == 'Hollow') & max_dendro > -500 & max_dendro < 3000) %>% # for visual purposes gets rid of values below -500 microns
               arrange(elev)) +
  geom_point(aes(x = day, y = max_dendro/1000, # convert to mm
                 color = elev),
             size = .5) +
  geom_line(aes(x = day, y = gompvals/1000), size = 1) + # convert to mm
  geom_vline(data = site_data %>% filter(!(topo == 'Hollow')) %>% arrange(elev),
             aes(xintercept = as.Date(cessation_day_gomp,origin = as.Date("2016-01-01"))), size = 1,
             linetype = 'dashed') +
  xlab(NULL) +
  scale_color_manual(breaks = breaks, values = color) +
  scale_y_continuous(NULL,
                     limits = c(NA,NA)) +
  scale_x_date(limits = as.Date(c('2016-04-01','2016-11-01'))) +
  facet_wrap(~ factor(ID, levels = c('LENA1','LENA2','LENA3','LENA7',
                                     'LESA1','LESA3','LESA5','LESA6',
                                     'MENA1','MENA2','MENA3','MENA6',
                                     'MESA1','MESA2','MESA4','MESA8','MESA9',
                                     'HENA1','HENA3','HENA5','HENA8','HENA9',
                                     'HESA2','HESA3','HESA4','HESA5','HESA9')), scales = 'free_y', ncol = 2) +
  ggtitle('Upslope') +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 5),
                              title = 'Elevation')) +
  theme(text = element_text(size = labsize,  family = "Liberation Sans"),
        axis.text =  element_text(size = 9),
        strip.text.x = element_blank(),
        # legend.position = 'none',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = tagsize),
        plot.title = element_text(hjust = 0.5))


Growth_Curves <- grid.arrange(patchworkGrob(p1 + p2), left = 'Daily Maximum Radial Growth (mm)', bottom = 'Time')


ggsave(filename = paste0(work.dir,'/Figures/Figure_3.tiff'),
       plot = Growth_Curves,
       dpi = 800, units = "in", width = 6.5, height = 7)

########################################################################################################################
############################### FIGURE 4 #################################
# Function to extract overall p-value of models
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

                                                                          ##### SM #####
LM1 = lm(formula = avg_TDR ~ TWI + Elevation + Aspect, data = site_data)
summary(LM1)
# Stepwise AIC indicates elevation is not important here
LM1 = stepAIC(LM1, trace = T)
# Therefore:
summary(LM1)
capture.output(summary(LM1), file = paste0(work.dir,"/Stat_Output/1_SoilMoisture_Regression.txt")) #.txt file with summary output.
##### Monthly Regressions
SM_may = lm(formula = may_avg_TDR ~ TWI + Elevation + Aspect, data = site_data)
SM_may = stepAIC(SM_may, trace = T)
capture.output(summary(SM_may), file = paste0(work.dir,"/Stat_Output/1_SoilMoisture_Regression_May.txt")) #.txt file with summary output.
SM_mayR2 = summary(SM_may)$adj.r.squared
SM_mayP = overall_p(SM_may)

SM_june = lm(formula = june_avg_TDR ~ TWI + Elevation + Aspect, data = site_data)
SM_june = stepAIC(SM_june, trace = T)
capture.output(summary(SM_june), file = paste0(work.dir,"/Stat_Output/1_SoilMoisture_Regression_June.txt")) #.txt file with summary output.
SM_juneR2 = summary(SM_june)$adj.r.squared
SM_juneP = overall_p(SM_june)

SM_july = lm(formula = july_avg_TDR ~ TWI + Elevation + Aspect, data = site_data)
SM_july = stepAIC(SM_july, trace = T)
capture.output(summary(SM_july), file = paste0(work.dir,"/Stat_Output/1_SoilMoisture_Regression_July.txt")) #.txt file with summary output.
SM_julyR2 = summary(SM_july)$adj.r.squared
SM_julyP = overall_p(SM_july)

SM_aug = lm(formula = aug_avg_TDR ~ TWI + Elevation + Aspect, data = site_data)
SM_aug = stepAIC(SM_aug, trace = T)
capture.output(summary(SM_aug), file = paste0(work.dir,"/Stat_Output/1_SoilMoisture_Regression_Aug.txt")) #.txt file with summary output.
SM_augR2 = summary(SM_aug)$adj.r.squared
SM_augP = overall_p(SM_aug)
#####

yhat1 = predict(LM1)

p1 <- ggplot(data = site_data , aes(x = yhat1*100, y = avg_TDR*100)) +
  scale_y_continuous('Obs. Soil Moisture (%)', limits = c(8,38), breaks = seq(0,100,by = 10)) +
  scale_x_continuous('Pred. Soil Moisture (%)', limits = c(8,38),  breaks = seq(0,100,by = 10)) +
  geom_label(label = deparse(bquote(paste('R\u00B2'['adj']*' = ', .(signif(summary(LM1)$adj.r.squared,2))))), parse = T,
             x = 8 + (36-8)*(1/5), y = 36 - (36-8)*(1/8), label.size = NA,
             size = 9/.pt) +
  geom_label(label = 'TWI \u2191\nTSRI \u2193',
             x = 36 - (36-8)*(1/6), y = 8 + (36-8)*(1/10), label.size = NA,
             size = 9/.pt) +
  geom_point(data = site_data, stroke = 1, aes(color = TWI, shape = Aspect < 0.5, size = Aspect < 0.5)) +
  scale_size_manual(values = c(pointsize, pointsize + 0.2)) +
  scale_shape_manual(values = c(22,15)) +
  scale_color_viridis_c(option = 'H', begin = end, end = begin, alpha = alpha) +
  geom_abline(slope = 1, size = 1, linetype = 'dashed') +
  guides(color = guide_colorbar(frame.colour = 'black', ticks.colour = 'black'),
         shape = F,
         size = F) +
  labs(tag = 'B') +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = labsize),
        plot.tag = element_text(size = tagsize))

                                                                          ##### VPD #####
LM2 = lm(formula = avg_VPD ~ TWI + Elevation + Aspect, data = site_data )
summary(LM2)
# Stepwise AIC indicates all three are important
LM2 = stepAIC(LM2)
capture.output(summary(LM2), file = paste0(work.dir,"/Stat_Output/2_VPD_Regression.txt")) #.txt file with summary output.
##### Monthly Regressions
VPD_may = lm(formula = may_avg_VPD ~ TWI + Elevation + Aspect, data = site_data)
VPD_may = stepAIC(VPD_may, trace = T)
capture.output(summary(VPD_may), file = paste0(work.dir,"/Stat_Output/2_VPD_Regression_May.txt")) #.txt file with summary output.
VPD_mayR2 = summary(VPD_may)$adj.r.squared
VPD_mayP = overall_p(VPD_may)

VPD_june = lm(formula = june_avg_VPD ~ TWI + Elevation + Aspect, data = site_data)
VPD_june = stepAIC(VPD_june, trace = T)
capture.output(summary(VPD_june), file = paste0(work.dir,"/Stat_Output/2_VPD_Regression_June.txt")) #.txt file with summary output.
VPD_juneR2 = summary(VPD_june)$adj.r.squared
VPD_juneP = overall_p(VPD_june)

VPD_july = lm(formula = july_avg_VPD ~ TWI + Elevation + Aspect, data = site_data)
VPD_july = stepAIC(VPD_july, trace = T)
capture.output(summary(VPD_july), file = paste0(work.dir,"/Stat_Output/2_VPD_Regression_July.txt")) #.txt file with summary output.
VPD_julyR2 = summary(VPD_july)$adj.r.squared
VPD_julyP = overall_p(VPD_july)

VPD_aug = lm(formula = aug_avg_VPD ~ TWI + Elevation + Aspect, data = site_data)
VPD_aug = stepAIC(VPD_aug, trace = T)
capture.output(summary(VPD_aug), file = paste0(work.dir,"/Stat_Output/2_VPD_Regression_Aug.txt")) #.txt file with summary output.
VPD_augR2 = summary(VPD_aug)$adj.r.squared
VPD_augP = overall_p(VPD_aug)
#####

yhat2 = predict(LM2)*-1

p2 <- ggplot(data = site_data , aes(x = yhat2, y = avg_VPD*-1)) +
  scale_y_continuous('Obs. VPD (-kPa)', limits = c(1.3, 2.8), breaks = seq(0,3, by = 0.5)) +
  scale_x_continuous('Pred. VPD (-kPa)', limits = c(1.3, 2.8), breaks = seq(0,3, by = 0.5)) +
  geom_label(label = deparse(bquote(paste('R\u00B2'['adj']*' = ', .(signif(summary(LM2)$adj.r.squared,2))))), parse = T,
             x = 1.3 + (2.8-1.3)*(1/5), y = 2.8 - (2.8-1.3)*(1/8), label.size = NA,
             size = 9/.pt) +
  geom_label(label = 'Elevation \u2193\nTWI \u2193\nTSRI \u2191',
             x = 2.8 - (2.8-1.3)*(1/6), y = 1.3 + (2.8-1.3)*(1/6), label.size = NA,
             size = 9/.pt) +
  geom_point(data = site_data, stroke = 1, aes(color = TWI, shape = Aspect < 0.5, size = Aspect < 0.5)) +
  scale_size_manual(values = c(pointsize, pointsize)) +
  scale_shape_manual(values = c(21,19), labels = c('South\n(TSRI > 0.5)','North\n(TSRI < 0.5)')) +
  scale_color_viridis_c(option = 'H', begin = end, end = begin, alpha = alpha) +
  geom_abline(slope = 1, size = 1, linetype = 'dashed') +
  guides(color = F,
         shape = guide_legend(title = 'Aspect', override.aes = list(size = 2)),
         size = F) +
  labs(tag = 'A') +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = labsize),
        plot.tag = element_text(size = tagsize),
        legend.key.height = unit(2, "lines"))

                                                                        ##### Air Temp #####
LM3 = lm(formula = avg_temp ~ TWI + Elevation + Aspect, data = site_data )
summary(LM3)
# Stepwise AIC indicates all three are important
LM3 = stepAIC(LM3)
capture.output(summary(LM3), file = paste0(work.dir,"/Stat_Output/3_AirTemp_Regression.txt")) #.txt file with summary output.
##### Monthly Regressions
temp_may = lm(formula = may_avg_temp ~ TWI + Elevation + Aspect, data = site_data)
temp_may = stepAIC(temp_may, trace = T)
capture.output(summary(temp_may), file = paste0(work.dir,"/Stat_Output/3_AirTemp_Regression_May.txt")) #.txt file with summary output.
temp_mayR2 = summary(temp_may)$adj.r.squared
temp_mayP = overall_p(temp_may)

temp_june = lm(formula = june_avg_temp ~ TWI + Elevation + Aspect, data = site_data)
temp_june = stepAIC(temp_june, trace = T)
capture.output(summary(temp_june), file = paste0(work.dir,"/Stat_Output/3_AirTemp_Regression_June.txt")) #.txt file with summary output.
temp_juneR2 = summary(temp_june)$adj.r.squared
temp_juneP = overall_p(temp_june)

temp_july = lm(formula = july_avg_temp ~ TWI + Elevation + Aspect, data = site_data)
temp_july = stepAIC(temp_july, trace = T)
capture.output(summary(temp_july), file = paste0(work.dir,"/Stat_Output/3_AirTemp_Regression_July.txt")) #.txt file with summary output.
temp_julyR2 = summary(temp_july)$adj.r.squared
temp_julyP = overall_p(temp_july)

temp_aug = lm(formula = aug_avg_temp ~ TWI + Elevation + Aspect, data = site_data)
temp_aug = stepAIC(temp_aug, trace = T)
capture.output(summary(temp_aug), file = paste0(work.dir,"/Stat_Output/3_AirTemp_Regression_Aug.txt")) #.txt file with summary output.
temp_augR2 = summary(temp_aug)$adj.r.squared
temp_augP = overall_p(temp_aug)
#####

yhat3 = predict(LM3)

p3 <- ggplot(data = site_data , aes(x = yhat3, y = avg_temp)) +
  scale_y_continuous('Obs. Air Temp. (\u00B0C)', limits = c(11, 15), breaks = c(1:15)) +
  scale_x_continuous('Pred. Air Temp. (\u00B0C)', limits = c(11, 15), breaks = c(1:15)) +
  geom_label(label = deparse(bquote(paste('R\u00B2'['adj']*' = ', .(signif(summary(LM3)$adj.r.squared,2))))), parse = T,
             x = 11 + (15-11)*(1/5), y = 15 - (15-11)*(1/8), label.size = NA,
             size = 9/.pt) +
  geom_label(label = 'Elevation \u2193\nTWI \u2193\nTSRI \u2191',
             x = 15 - (15-11)*(1/6), y = 11 + (15-11)*(1/6), label.size = NA,
             size = 9/.pt) +
  geom_point(data = site_data, stroke = 1, aes(color = TWI, shape = Aspect < 0.5, size = Aspect < 0.5)) +
  scale_size_manual(values = c(pointsize, pointsize + 1)) +
  scale_shape_manual(values = c(23,18), labels = c('> 0.5','< 0.5')) +
  scale_color_viridis_c(option = 'H', begin = end, end = begin, alpha = alpha) +
  geom_abline(slope = 1, size = 1, linetype = 'dashed') +
  labs(tag = 'C') +
  guides(color = F,
         shape = F,
         size = F) +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = labsize),
        plot.tag = element_text(size = tagsize))

                                                                          ##### Soil Temp #####
LM4 = lm(formula = avg_soil_temp ~ TWI + Elevation + Aspect, data = site_data )
summary(LM4)
# Stepwise AIC indicates all three are important
LM4 = stepAIC(LM4)
capture.output(summary(LM4), file = paste0(work.dir,"/Stat_Output/4_SoilTemp_Regression.txt")) #.txt file with summary output.
##### Monthly Regressions
soil_temp_may = lm(formula = may_avg_soil_temp ~ TWI + Elevation + Aspect, data = site_data)
soil_temp_may = stepAIC(soil_temp_may, trace = T)
capture.output(summary(soil_temp_may), file = paste0(work.dir,"/Stat_Output/4_SoilTemp_Regression_May.txt")) #.txt file with summary output.
soil_temp_mayR2 = summary(soil_temp_may)$adj.r.squared
soil_temp_mayP = overall_p(soil_temp_may)

soil_temp_june = lm(formula = june_avg_soil_temp ~ TWI + Elevation + Aspect, data = site_data)
soil_temp_june = stepAIC(soil_temp_june, trace = T)
capture.output(summary(soil_temp_june), file = paste0(work.dir,"/Stat_Output/4_SoilTemp_Regression_June.txt")) #.txt file with summary output.
soil_temp_juneR2 = summary(soil_temp_june)$adj.r.squared
soil_temp_juneP = overall_p(soil_temp_june)

soil_temp_july = lm(formula = july_avg_soil_temp ~ TWI + Elevation + Aspect, data = site_data)
soil_temp_july = stepAIC(soil_temp_july, trace = T)
capture.output(summary(soil_temp_july), file = paste0(work.dir,"/Stat_Output/4_SoilTemp_Regression_July.txt")) #.txt file with summary output.
soil_temp_julyR2 = summary(soil_temp_july)$adj.r.squared
soil_temp_julyP = overall_p(soil_temp_july)

soil_temp_aug = lm(formula = aug_avg_soil_temp ~ TWI + Elevation + Aspect, data = site_data)
soil_temp_aug = stepAIC(soil_temp_aug, trace = T)
capture.output(summary(soil_temp_july), file = paste0(work.dir,"/Stat_Output/4_SoilTemp_Regression_July.txt")) #.txt file with summary output.
soil_temp_augR2 = summary(soil_temp_aug)$adj.r.squared
soil_temp_augP = overall_p(soil_temp_aug)
#####

yhat4 = predict(LM4)

p4 <- ggplot(data = site_data , aes(x = yhat4, y = avg_soil_temp)) +
  scale_y_continuous('Obs. Soil Temp. (\u00B0C)', limits = c(6.5, 13), breaks = c(1:15)) +
  scale_x_continuous('Pred. Soil Temp. (\u00B0C)', limits = c(6.5, 13), breaks = c(1:15)) +
  geom_label(label = deparse(bquote(paste('R\u00B2'['adj']*' = ', .(signif(summary(LM4)$adj.r.squared,2))))), parse = T,
             x = 6.5 + (13-6.5)*(1/5), y = 13 - (13-6.5)*(1/8), label.size = NA,
             size = 9/.pt) +
  geom_label(label = 'Elevation \u2193\nTWI \u2193\nTSRI \u2191',
             x = 13 - (13-6.5)*(1/6), y = 6.5 + (13-6.5)*(1/6), label.size = NA,
             size = 9/.pt) +
  geom_point(data = site_data, stroke = 1, aes(color = TWI, shape = Aspect < 0.5, size = Aspect < 0.5)) +
  scale_size_manual(values = c(pointsize, pointsize + 0.5)) +
  scale_shape_manual(values = c(24,17), labels = c('> 0.5','< 0.5')) +
  scale_color_viridis_c(option = 'H', begin = end, end = begin, alpha = alpha) +
  geom_abline(slope = 1, size = 1, linetype = 'dashed') +
  labs(tag = 'D') +
  guides(color = F,
         shape = F,
         size = F) +
  theme_classic() +
  theme(text = element_text(size = labsize,  family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = labsize),
        plot.tag = element_text(size = tagsize))

temporal_R2 = tibble(month = c('May','May','May','May',
                               'June','June','June','June',
                               'July','July','July','July',
                               'August','August','August','August'),
                     monthdate = c(as.Date('2016-05-01'),as.Date('2016-05-01'),as.Date('2016-05-01'),as.Date('2016-05-01'),
                                  as.Date('2016-06-01'),as.Date('2016-06-01'),as.Date('2016-06-01'),as.Date('2016-06-01'),
                                  as.Date('2016-07-01'),as.Date('2016-07-01'),as.Date('2016-07-01'),as.Date('2016-07-01'),
                                  as.Date('2016-08-01'),as.Date('2016-08-01'),as.Date('2016-08-01'),as.Date('2016-08-01')),
                     var = c('SM','VPD','temp','soil_temp',
                             'SM','VPD','temp','soil_temp',
                             'SM','VPD','temp','soil_temp',
                             'SM','VPD','temp','soil_temp'),
                     R2 = c(SM_mayR2,VPD_mayR2,temp_mayR2,soil_temp_mayR2,
                            SM_juneR2,VPD_juneR2,temp_juneR2,soil_temp_juneR2,
                            SM_julyR2,VPD_julyR2,temp_julyR2,soil_temp_julyR2,
                            SM_augR2,VPD_augR2,temp_augR2,soil_temp_augR2),
                     P = c(SM_mayP,VPD_mayP,temp_mayP,soil_temp_mayP,
                            SM_juneP,VPD_juneP,temp_juneP,soil_temp_juneP,
                            SM_julyP,VPD_julyP,temp_julyP,soil_temp_julyP,
                            SM_augP,VPD_augP,temp_augP,soil_temp_augP))

p5 <- ggplot(data = temporal_R2, aes(x = monthdate, y = R2)) +
  geom_point(aes(shape = var), size = 2.5) +
  ylab(bquote(paste('R\u00B2'['adj']))) +
  xlab('Month') +
  scale_shape_manual(values = c(21,22,23,24),
                     breaks = c('VPD','SM','temp','soil_temp'),
                     labels = c('VPD', 'Soil Moisture', 'Air Temp.', 'Soil Temp.')) +
  geom_line(aes(group = var), linewidth = 0.5) +
  theme_bw() +
  guides(shape = guide_legend(title = "Variable")) +
  labs(tag = 'E') +
  theme(text = element_text(size = labsize,  family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = labsize),
        plot.tag = element_text(size = tagsize),
        panel.grid.minor = element_blank())

layout = '
AB
AB
AB
CD
CD
CD
EE
EE'

TPI_regressions <- p2 + p1 + p3 + p4 + p5 + plot_layout(design = layout, guides = 'collect')

ggsave(filename = paste0(work.dir,'/Figures/Figure_4.tiff'),
       plot = TPI_regressions,
       dpi = 800, units = "in", width = 6.5, height = 6.75)

#############################################################################################################################
############################## Results: t-tests of cessation day by group #################################

(test1 = t.test(cessation_day_gomp ~ topo, data = site_data)) # Differs by topographic position
(test2 = t.test(cessation_day_gomp ~ elev == 'High', data = site_data)) # Differs by elevation between high and middle + low.
(test3 = t.test(cessation_day_gomp ~ aspect, data = site_data)) # Does not differ by Aspect
capture.output(print(test1),print(test2),print(test3), file = paste0(work.dir,"/Stat_Output/5_Topo_Elev_Aspect_Compare.txt")) #.txt file with t-test results.

filtered = site_data %>% filter(avg_VPD < mean(site_data$avg_VPD)) # filter to above average VPD (average being the average of all measured sites)
(test4 = t.test(cessation_day_gomp ~ topo == 'Hollow', data = filtered)) # Differs by topography at high VPDs
capture.output(print(test4), file = paste0(work.dir,"/Stat_Output/6_Hollow_at_High_VPD_Compare.txt")) #.txt file with t-test results.
# average TWI of these 5 sites
filtered %>%
  filter(topo == 'Hollow') %>%
  pull(TWI) %>%
  mean()

###############################################################################################################################
######################################## FIGURE 5 ###################################################

# Cessation Predicted by Microclimate #
site_data_VPDpositive = site_data %>% mutate(avg_VPD = avg_VPD*-1)
LM5 = lm(formula = cessation_day_gomp ~ poly(avg_TDR,2) + avg_VPD + avg_temp + avg_soil_temp,
         data = site_data_VPDpositive)
# Stepwise AIC identifies soil temperature as not contributing additional explanatory power
LM5 <- stepAIC(LM5)
summary(LM5)

capture.output(summary(LM5), file = paste0(work.dir,"/Stat_Output/7_Cessation_by_Microclimate_Regression.txt")) #.txt file with summary output.

# Use the fitted model to predict cessation
yhat7 = predict(LM5)

################ Observed vs. Predicted
ces_env <- ggplot(data = site_data, aes(x = yhat7, y = cessation_day_gomp)) +
  geom_abline(slope = 1, size = 1, linetype = 'dashed') +
  geom_point(shape = if_else(site_data$ID %in% c('HENA1','LENA3','MENA3'), 24, 21),
             size = pointsize, stroke = 1.5, color = if_else(site_data$ID %in% c('HENA1','LENA3','MENA3'), '#0072B2', '#009e73')) +
  scale_y_continuous('Observed Cessation Day', limits = c(155, 230), breaks = seq(0,300, by = 10)) +
  scale_x_continuous('Predicted Cessation Day', limits = c(155, 230), breaks = seq(0,300, by = 10)) +
  geom_label(label = deparse(bquote(paste('R\u00B2'['adj']*' = ', .(signif(summary(LM5)$adj.r.squared,2))))), parse = T,
                 aes(x = 155 + (230-155)*(1/8), y = 230 - (230-155)*(1/12)), label.size = NA) +
  theme_classic() +
  labs(tag = 'A') +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = numsize),
        legend.position = c(0.065, 0.8),
        legend.justification = c("left", "top"),
        plot.tag = element_text(size = tagsize))

############# Partial Effects Plots
response_var1 <- visreg(LM5, "avg_TDR", scale = "linear", rug = TRUE,
                        xlab = 'Soil Moisture (%)', line.par = list(col = '#A04800'),
                        # points.par = list(col = '#009E73'),
                        gg = T) +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20)) +
  theme_classic() +
  labs(tag = 'B') +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = numsize),
        plot.tag = element_text(size = tagsize))

response_var2 <- visreg(LM5, "avg_VPD", scale = "linear", rug = TRUE,
                        xlab = 'VPD (-kPa)', line.par = list(col = '#A04800'),
                        # points.par = list(col = '#009E73'),
                        gg = T) +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20), limits = c(150,240)) +
  theme_classic() +
  labs(tag = 'C') +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = numsize),
        plot.tag = element_text(size = tagsize))

response_var3 <- visreg(LM5, "avg_temp", scale = "linear", rug = TRUE,
                        xlab = 'Air Temperature (\u00B0C)', line.par = list(col = '#A04800'),
                        # points.par = list(col = '#009E73'),
                        gg = T) +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20), limits = c(150,240)) +
  theme_classic() +
  labs(tag = 'D') +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = numsize),
        plot.tag = element_text(size = tagsize))

ces_env_vars <- grid.arrange(response_var1, response_var2, response_var3, ces_env,
                             layout_matrix = cbind(c(4, 4, 4), c(1, 2, 3)),
                             nrow = 3, ncol = 2, widths = c(1, .5))

ggsave(filename = paste0(work.dir,'/Figures/Figure_5.tiff'),
       plot = ces_env_vars,
       dpi = 800, units = "in", width = 6.5, height = 4.25)

#########################################################################################
