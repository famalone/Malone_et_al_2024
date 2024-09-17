if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
if ("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
if ("cowplot" %in% rownames(installed.packages()) == FALSE) {install.packages("cowplot")}
if ("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")}
if ("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")}

###### Data Transformation, Analysis, and Figures for Supplimentary Information (Author: Fin Malone; R version 4.4.1)
library(tidyverse)
library(patchwork)
library(cowplot)
library(readxl)
library(gridExtra)

############################## Supporting Information ###################################

work.dir = '/home/fmalone/Documents/Malone_et_al_2024' # Folder Directory

# Daily environmental data and growth curves.
dendro_data <- readxl::read_excel(paste0(work.dir,'/_4_processed_data_thesis.xlsx'),
                                  sheet = 'time_series_daily_and_curves', col_names = T) %>%
  mutate(cessation_day_gomp = NA,
         day = as.Date(day,    # Convert Julian day to date
                       origin = as.Date("2016-01-01")),
         max_dendro = max_dendro / 1000, # Convert to mm
         gompvals = gompvals / 1000) %>% # Convert to mm
  
  mutate(`Topographic Position` = as.numeric(stringr::str_sub(ID,-1)), # looks at site ID to determine topographic position,
         
         Aspect = stringr::str_sub(ID,-3,-3),
         
         Elevation = stringr::str_sub(ID,-5,-5)) %>%
  
  mutate(`Topographic Position` = case_when(
    `Topographic Position` %in% c(1, 3, 5) ~ 'Hollow',
    T ~ 'Upslope'),
    Aspect = case_when(
      Aspect %in% 'N' ~ 'North', T ~ 'South'),
    Elevation = factor(case_when(
      Elevation %in% 'L' ~ 'Low', Elevation %in% 'M' ~ 'Middle', T ~ 'High'), levels = c('Low', 'Middle', 'High')))

site_data <- readxl::read_excel(paste0(work.dir,'/_4_processed_data_thesis.xlsx'),
                                sheet = 'post_curve_data', col_names = T)

# Add cessation day to the daily data for groupings.
for(i in 1:length(dendro_data$cessation_day_gomp)) {
  dendro_data$cessation_day_gomp[i] = site_data$cessation_day_gomp[which(dendro_data$ID[i] == site_data$ID)]
}

# scale_color_viridis
alpha = 1
begin = 0.1
end = 0.75
# font sizes
numsize = 11
labsize = 12
tagsize = 14
# geom_point
pointsize = 2
#######################################################################
############## Supplemental FIGURES ###############

################################### SI Figures 1-3 #######################################
##### SOIL WATER RETENTION FOR SATURATED SITES

WP_data <- read_csv(paste0(work.dir,'/SI Data/Water Potential.csv')) %>%
  mutate(day = as.Date(day, origin = as.Date('2016-01-01')))

curves = list.files(paste0(work.dir, '/SI Data'), pattern = ".RDS$", full.names = T)

# Retention Curve Plot Function #
plot_retention = function(data){
  site_VWC = dendro_data %>% filter(ID == substr(data[[6]],1,5))
  depth = substr(data[[6]],7,10)
  
  ggplot_data = data.frame(kPa = data[[2]]$raw_kPa, VWC = data[[2]]$raw_VWC)
  
  model_data = data.frame(model_predict2 = data[[8]]$fit_VWC, dummy_kPa2 = data[[8]]$kPa)
  
  plain <- format(c(.0001,.001,.01,.1,1,10,100,1000), scientific = FALSE, drop0trailing = TRUE) # Neat format for y labels
  
  Model = ggplot(data = ggplot_data, aes(x = VWC*100 , y = kPa*0.001), na.rm = T) + #Converted kPa to MPa for readability
    geom_point(size = 3, color = "#BFEFFF") +
    geom_point(size = 3, color = "black", fill = NA, shape = 21, alpha = 0.3) +
    scale_y_log10(breaks = c(.0001,.001,.01,.1,1,10,100,1000), labels = plain, limits = c(0.0001, 6309.573)) +
    scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10),limits = c(0, max(model_data$model_predict2*100))) +
    ylab("Water Potential (-MPa)") +
    xlab("Soil Moisture (%)") +
    geom_line(data = model_data, aes(x = model_predict2*100, y = dummy_kPa2*0.001), size = 1.05) + #Converted kPa to MPa for readability
    theme_bw() +
    labs(tag = case_when(depth == '05cm' ~ "B", T ~ 'C')) +
    ggtitle(paste0('(', data[[5]], ')')) +
    theme(text = element_text(size = 10, family = "Liberation Sans"),
          axis.text =  element_text(size = 9),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 7),
          plot.tag = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, size = 7))
  
  VWC_range = ggplot(data = site_VWC) +
    geom_violin(aes(x = case_when(depth == '05cm' ~ mean_TDR1*100, depth == '50cm' ~ mean_TDR2*100),
                    y = 0), na.rm = T,
                 fill = case_when(depth == '05cm' ~ '#D55E00', depth == '50cm' ~ '#009E73'), alpha = 0.3, ) +
    geom_boxplot(aes(x = case_when(depth == '05cm' ~ mean_TDR1*100, depth == '50cm' ~ mean_TDR2*100),
                                      y = 0), width = 0.1) +
    ylab(NULL) +
    xlab(NULL) +
    theme_minimal() +
    theme(axis.text.y=element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.text.x=element_blank()) +
    scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5),
                       limits = c(0, layer_scales(Model)$x$range$range[2]))
  
  aligned = cowplot::align_plots(Model, VWC_range, align = 'hv')
  Model = ggdraw(aligned[[1]]) + draw_plot(aligned[[2]], y = 0)
  Model
}

sites = c('LENA3','MENA3')
for(i in 1:length(sites)) {
  
  p1 <- ggplot(data = WP_data %>% filter(ID == sites[i])) +
    geom_line(aes(x = day, y = mean_WP2*.001),
              color = '#999999', size = 1) +
    geom_line(aes(x = day, y = mean_WP1*.001),
              color = '#999999', size = 1) +
    geom_point(aes(x = day, y = mean_WP2*.001), color = '#009E73',size = 1.5) +
    geom_point(aes(x = day, y = mean_WP1*.001), color = '#D55E00',size = 1.5) +
    geom_vline(data = dendro_data %>% filter(ID == sites[i]),
               aes(xintercept = as.Date(cessation_day_gomp, origin = as.Date("2016-01-01"))), size = 1,
               linetype = 'dashed', color = 'black') +
    scale_x_date(name = 'Time', limits = c(as.Date('2016-05-01'), as.Date('2016-09-01'))) +
    scale_y_continuous(name = 'Soil Water Potential (-MPa)', limits = c(0,0.1)) +
    labs(tag = 'A') +
    ggtitle(case_when(sites[i] == 'LENA3' ~ 'Low Elevation; North Aspect; Convergent Hollow',
            T ~ 'Middle Elevation; North Aspect; Convergent Hollow')) +
    theme_bw() +
    theme(text = element_text(family = "Liberation Sans"),
          plot.tag = element_text(size = 14))
  
  which_models <- which(str_sub(curves, start = -20, end = -16) == sites[i])
  
  p2 <- plot_retention(readRDS(curves[which_models[1]]))
  
  p3 <- plot_retention(readRDS(curves[which_models[2]]))
  
  layout = '
  AA
  AA
  BC
  BC
  BC'
  
  retention <- p1 + p2 + p3 + plot_layout(design = layout)
  
  ggsave(filename = paste0(work.dir, '/Figures/SI_Figure_', i, '.tiff'),
         plot = retention,
         dpi = 800, units = "in", width = 7.5, height = 5)
}

S4 <- ggplot(data = dendro_data %>% filter(ID == 'HENA1')) +
  geom_line(aes(x = day, y = mean_TDR2*100),
            color = '#999999', size = 1) +
  geom_line(aes(x = day, y = mean_TDR1*100),
            color = '#999999', size = 1) +
  geom_point(aes(x = day, y = mean_TDR2*100), color = '#009E73',size = 1.5) +
  geom_point(aes(x = day, y = mean_TDR1*100), color = '#D55E00',size = 1.5) +
  geom_vline(data = dendro_data %>% filter(ID == 'HENA1'),
             aes(xintercept = as.Date(cessation_day_gomp, origin = as.Date("2016-01-01"))), size = 1,
             linetype = 'dashed', color = 'black') +
  scale_x_date(name = 'Time', limits = c(as.Date('2016-05-01'), as.Date('2016-09-01'))) +
  scale_y_continuous(name = 'Soil Moisture (%)', limits = c(0,40)) +
  ggtitle(case_when(sites[i] == 'LENA3' ~ 'Low Elevation; North Aspect; Convergent Hollow',
                    T ~ 'High Elevation; North Aspect; Convergent Hollow')) +
  theme_bw() +
  theme(text = element_text(family = "Liberation Sans"),
        plot.tag = element_text(size = 14))

ggsave(filename = paste0(work.dir, '/Figures/SI_Figure_3.tiff'),
       plot = S4,
       dpi = 800, units = "in", width = 7.5, height = 2.5)

################################## SI FIGURE 4 ###########################################

# Cessation Predicted by Landscape Position Under All Conditions #
LM6.1 = lm(formula = cessation_day_gomp ~ TWI + Elevation + Aspect,
           data = site_data)
summary(LM6.1)
# Stepwise AIC identifies TWI and Elevation variables as statistically important. Excludes Aspect.
LM6.1 <- stepAIC(LM6.1)
summary(LM6.1)

capture.output(summary(LM6.1), file = paste0(work.dir,"/Stat_Output/SI_Cessation_by_Landscape_Regression_All.txt")) #.txt file with summary output.


# Cessation Predicted by Landscape Position Under Moderate Conditions #
site_exclude <- site_data %>% filter(!(ID %in% c('HENA1','LENA3','MENA3'))) # Excludes hollow sites with saturated soils

LM6.2 = lm(formula = cessation_day_gomp ~ TWI + Elevation + Aspect,
         data = site_exclude)
# Stepwise AIC identifies Aspect, TWI, and Elevation variables as statistically important
LM6.2 <- stepAIC(LM6.2)
summary(LM6.2)

capture.output(summary(LM6.2), file = paste0(work.dir,"/Stat_Output/SI_Cessation_by_Landscape_Regression_ExcludeSat.txt")) #.txt file with summary output.

# Use the fitted model to predict cessation
yhat5 = predict(LM6.2)

# Three saturation-limited trees
VWC_excluded <- as.data.frame(site_data %>% filter(ID %in% c('HENA1','LENA3','MENA3')))
# Use the fitted model to predict cessation for the excluded trees
yhat6 = predict(LM6.2, newdata = VWC_excluded)     # To clarify: these three data points do not influence the model.
                                                   # Their predictions are displayed to show their deviance from the model fit.

############# Observed vs. Predicted
ces_pos <- ggplot(data = site_exclude, aes(x = yhat5, y = cessation_day_gomp)) +
  geom_abline(slope = 1, size = 1, linetype = 'dashed') +
  geom_point(size = pointsize, shape = 21, stroke = 1.5, aes(color = TWI)) +
  geom_point(data = VWC_excluded, aes(x = yhat6, y = cessation_day_gomp, color = TWI),
             size = pointsize, shape = 4, stroke = 1.5) +
  scale_color_viridis_c(option = 'H', begin = begin, end = end, alpha = alpha, direction = -1) +
  scale_y_continuous('Observed Cessation Day', limits = c(155, 230), breaks = seq(0,300, by = 10)) +
  scale_x_continuous('Predicted Cessation Day', limits = c(155, 230), breaks = seq(0,300, by = 10)) +
  geom_label(label = deparse(bquote(paste('R\u00B2'['adj']*' = ', .(signif(summary(LM6.2)$adj.r.squared,2))))), parse = T,
             x = 155 + (230-155)*(1/12), y = 230 - (230-155)*(1/12), label.size = NA) +
  geom_label(label = 'Saturated\nSites Excluded', color = '#d00000ff',
             x = 218, y = 175, label.size = NA) +
  theme_classic() +
  guides(color = guide_colorbar(frame.colour = 'black', ticks.colour = 'black')) +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = numsize),
        legend.position = c(0.065, 0.8),
        legend.justification = c("left", "top"),
        plot.tag = element_text(size = tagsize))

################# Partial Effects Plots

response_var1 <- visreg(LM6.2, "Elevation", scale = "linear", rug = TRUE,
                        xlab = 'Elevation (m)', line.par = list(col = '#A04800'), gg = T) +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20)) +
  theme_classic() +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = numsize),
        plot.tag = element_text(size = tagsize))

response_var2 <- visreg(LM6.2, "TWI", scale = "linear", rug = TRUE,
                        line.par = list(col = '#A04800'), xlab = 'TWI', gg = T) +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20)) +
  theme_classic() +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = numsize),
        plot.tag = element_text(size = tagsize))

response_var3 <- visreg(LM6.2, "Aspect", scale = "linear", rug = TRUE,
                        xlab = 'TSRI', line.par = list(col = '#A04800'), gg = T) +
  scale_y_continuous('Cessation Day', breaks = seq(100,240,20)) +
  theme_classic() +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = numsize),
        legend.text = element_text(size = numsize),
        legend.title = element_text(size = numsize),
        plot.tag = element_text(size = tagsize))

ces_pos_vars <- grid.arrange(response_var1, response_var2, response_var3, ces_pos,
                             layout_matrix = cbind(c(4, 4, 4), c(1, 3, 2)),
                             nrow = 3, ncol = 2, widths = c(1, .5))

ggsave(filename = paste0(work.dir,'/Figures/SI_Figure_4.tiff'),
       plot = ces_pos_vars,
       dpi = 800, units = "in", width = 8, height = 5)

################################## SI FIGURE 5 ###########################################

LM7.1 = lm(gomp_length ~ start_day_gomp, data = site_data)
summary(LM7.1)

capture.output(summary(LM7.1), file = paste0(work.dir,"/Stat_Output/SI_Duration_by_Start.txt")) #.txt file with summary output.

LM7.2 = lm(gomp_length ~ cessation_day_gomp, data = site_data)
summary(LM7.2)

capture.output(summary(LM7.2), file = paste0(work.dir,"/Stat_Output/SI_Duration_by_Cessation.txt")) #.txt file with summary output.

# list of site IDs for trees that started growth immediately following dendrometer installation
start_prior_IDs = c('HESA4','HESA5','HESA9','HESA2', 'MESA8','MENA2','LENA1','LENA2','MENA6')

Duration_Start = ggplot(data = site_data, aes(y = gomp_length, x = start_day_gomp)) +
  geom_point(aes(group = ID %in% start_prior_IDs), shape = if_else(site_data$ID %in% start_prior_IDs, 19, 21),
             color = '#0072B2', size = pointsize, stroke = 1.5) +
  geom_smooth(method = 'lm', formula = y ~ x, se = F, color = 'black') +
  ylab('Duration of Growth (Days)') +
  xlab('Start Day') +
  geom_label(label = deparse(bquote(paste('R\u00B2'['adj']*' = ', .(signif(summary(LM7.1)$adj.r.squared,2))))), parse = T,
             aes(x = 140 + (230-140)*(1/8), y = 125 - (125-0)*(1/12)), label.size = NA) +
  theme_classic() +
  labs(tag = 'A') +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = 9))

Duration_Ces = ggplot(data = site_data, aes(y = gomp_length, x = cessation_day_gomp)) +
  geom_point(aes(group = ID %in% start_prior_IDs), shape = if_else(site_data$ID %in% start_prior_IDs, 19, 21),
             color = '#D55E00', size = pointsize, stroke = 1.5) +
  geom_smooth(method = 'lm', formula = y ~ x, se = F, color = 'black') +
  ylab('Duration of Growth (Days)') +
  xlab('Cessation Day') +
  geom_label(label = deparse(bquote(paste('R\u00B2'['adj']*' = ', .(signif(summary(LM7.2)$adj.r.squared,2))))), parse = T,
             aes(x = 155 + (230-155)*(1/6), y = 125 - (125-0)*(1/12)), label.size = NA) +
  theme_classic() +
  labs(tag = 'B') +
  theme(text = element_text(size = labsize, family = "Liberation Sans"),
        axis.text =  element_text(size = 9))

Duration = Duration_Start + Duration_Ces

ggsave(filename = paste0(work.dir,'/Figures/SI_Figure_5.tiff'),
       plot = Duration,
       dpi = 800, units = "in", width = 7.5, height = 3.75)



