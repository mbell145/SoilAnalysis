#############################################################################################################
######################   Recovery Standard Quant  #############################################################
#############################################################################################################
##############################################################################################################

## Let's start with a black slate - Let's re-start the R session

# Ctrl + Shift + Fn + F10  (PC / Linux)
# Command + Shift + Fn+ F10 (Mac OS)

#AUTHOR: Madison Bell
#Date updated: 2020-04-21


#================= Packages needed ===========================================================================


#Data Handling
library(tidyverse)
library(janitor)

#Plotting
library(extrafont)
extrafont::loadfonts(device="win")
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)
library(svglite)
library(GGally)

#Data Analysis
library(moderndive)
library(car)
library(rstatix)




#=============== Import Data ==========================================================================================================

#Import Sample matrix of intensity values

#Quant Method Filename SampleNumber  RS     Treatmnet   Time  Env_rep  Tech_rep   Integ_type  Area  cali_conc calc_Conc_(mg/ml)  RT
#------------------------------------------------------------------------------------------------------------------------------------
#S1           str       num           str   str         num   str       num       str         num   num       num               num
#S1           str       num           str   str         num   str       num       str         num   num       num               num
#S1           str       num           str   str         num   str       num       str         num   num       num               num
#S1           str       num           str   str         num   str       num       str         num   num       num               num
#S1           str       num           str   str         num   str       num       str         num   num       num               num
#S1           str       num           str   str         num   str       num       str         num   num       num               num
#S1           str       num           str   str         num   str       num       str         num   num       num               num
#S1           str       num           str   str         num   str       num       str         num   num       num               num
#S1           str       num           str   str         num   str       num       str         num   num       num               num
#S1           str       num           str   str         num   str       num       str         num   num       num               num


rs <-read_csv("data_raw/SoilRSTest_LDOPA_DCIP_MUBph_Quant_20200228.csv")
rs <- clean_names(rs)

#Quant Method  regression  slope  y_intercept   r2
#---------------------------------------------------
#S1            str         num    num           num  
#S1            str         num    num           num
#S1            str         num    num           num
#S1            str         num    num           num
#S1            str         num    num           num
#S1            str         num    num           num

rs_reg <-read_csv("data_raw/SoilRSTest_LDOPA_DCIP_MUBph_Quant_Regression_20200228.csv")
rs_reg <- clean_names(rs_reg)

#Quant Method  regression  slope  y_intercept   r2
#---------------------------------------------------
#S1            str         num    num           num  
#S1            str         num    num           num
#S1            str         num    num           num
#S1            str         num    num           num
#S1            str         num    num           num
#S1            str         num    num           num

rs_types <-read_csv("data_raw/SoilTypeSummary.csv")
rs_types <- clean_names(rs_types)

#=============== Determine LOD and LOQ ==================================================================================================
#Make a dataframe for calibration data
rs_quant <- rs %>%
  filter(sample_type == "Cal") %>%
  select("quant_method", "treatment", "area", "cali_conc", "calc_conc", "rt") %>%
  group_by(quant_method, treatment) %>%
  summarise_at(vars("area", "cali_conc", "calc_conc", "rt"), list(mean = mean, sd = sd), na.rm = TRUE) 

#Make a dataframe for blank data
rs_blank <- rs %>%
  filter(sample_type == "Blank") %>%
  select("quant_method","treatment", "area", "calc_conc", "rt") %>%
  group_by(quant_method, treatment) %>%
  summarise_at(vars("area", "calc_conc", "rt"), list(mean = mean, sd = sd), na.rm = TRUE)



#Calculate the LOD and LOQ at the lowest point of the calibration curve
rs_quant_lod_loq_full <- rs_quant %>%
  filter(cali_conc_mean <= 0.008) %>%
  right_join(rs_reg, by = NULL) %>%
  mutate(fitted_area = m*cali_conc_mean) %>%
  mutate(lod = 3*(abs(area_mean - fitted_area)/m)) %>% #3*(SE of the curve/slope) = LOD
  mutate(loq = 10*(abs(area_mean - fitted_area)/m)) 


rs_quant_lod_loq <- rs_quant_lod_loq_full %>%
  select("quant_method", "treatment", "lod", "loq") %>%
  group_by(quant_method) %>% 
  slice(which.min(lod)) #Extract the rows containing the minumim lod value for each quant_method

write.csv(rs_quant_lod_loq,'results/RS_DCIP_LDOPA_MUBph_lod_loq_table.csv')


#===============Process the data using LOD and LOQ ==================================================================================================
#Make a dataframe for quantification data
rs2 <- rs %>%
  filter(sample_type == "Sample") %>%
  select("quant_method", "filename", "rs", "treatment", "time", "env_rep", "tech_rep", "area", "calc_conc", "rt") %>%
  right_join(rs_quant_lod_loq, by = "quant_method") %>% #Add in LOD and LOQ data
  replace(is.na(.), 0) %>% #Replace all NAs with 0
  mutate(adj_conc = ifelse(calc_conc <= loq, lod/sqrt(2), calc_conc)) #New column where values <= loq are replace with lod/sqrt(2)

#=============== Bar plots of RS Quant data ==================================================================================================
rs2_bar <- rs2 %>% 
  filter(treatment.x != "QC") %>%
  filter(treatment.x != "ori") %>%
  unite(treatment, treatment.x, time, remove = FALSE) %>%
  group_by(quant_method, treatment.x, treatment) %>%
  summarise_at(vars("area", "adj_conc", "rt"), list(mean = mean, sd = sd), na.rm = TRUE)

#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")

#Plot using facet_wrap (Plots for all quant_methods)
all_quant_Barplots <- ggplot(data = rs2_bar,aes(x = treatment, y = adj_conc_mean, fill = treatment.x)) + 
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_errorbar(aes(ymin=adj_conc_mean - adj_conc_sd, ymax=adj_conc_mean + adj_conc_sd), width=0.2, linetype = "solid", size= 0.2) +
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Concentration (mg/mL)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method, scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_Allquantmethods_RStest.svg", all_quant_Barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_Allquantmethods_RStest.pdf", all_quant_Barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)






#Bar Plots for MS quant_methods
rs2_bar_ms <- rs2_bar %>% 
  filter(quant_method != "DCIP_270nm") %>%
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "DCIP_267.99mz" = "DCIP",
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

ms_quant_Barplots <- ggplot(data = rs2_bar_ms,aes(x = treatment, y = adj_conc_mean, fill = treatment.x)) + 
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_errorbar(aes(ymin=adj_conc_mean - adj_conc_sd, ymax=adj_conc_mean + adj_conc_sd), width=0.2, linetype = "solid", size= 0.2) +
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Concentration (mg/mL)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_MSquantmethods_RStest.svg", ms_quant_Barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_MSquantmethods_RStest.pdf", ms_quant_Barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)







#Bar Plots for nm quant_methods
rs2_bar_nm <- rs2_bar %>% 
  filter(quant_method != "DCIP_267.99mz") %>%
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "DCIP_270nm" = "DCIP",
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")


nm_quant_Barplots <- ggplot(data = rs2_bar_nm,aes(x = treatment, y = adj_conc_mean, fill = treatment.x)) + 
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_errorbar(aes(ymin=adj_conc_mean - adj_conc_sd, ymax=adj_conc_mean + adj_conc_sd), width=0.2, linetype = "solid", size= 0.2) +
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Concentration (mg/mL)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_NMquantmethods_RStest.svg", nm_quant_Barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_NMquantmethods_RStest.pdf", nm_quant_Barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)







#=============== Calculate the recoveries ================================================================================
#Using the cooncentrations from the original spike mixes summarize the original concentrations
rs2_ori <- rs2 %>%
  filter(treatment.x == "ori") %>%
  unite(treatment, treatment.x, time, remove = FALSE) %>%
  group_by(quant_method, treatment.x, treatment) %>%
  summarise_at(vars("area", "adj_conc", "rt"), list(mean = mean, sd = sd), na.rm = TRUE)


rs2_recoveries <- rs2 %>% 
  filter(treatment.x != "ori") %>%
  filter(treatment.x != "QC") %>% 
  unite(treatment, treatment.x, time, remove = FALSE) %>%
  right_join(rs2_ori, by = "quant_method")  %>%  #Add in ori data
  select("quant_method", "filename", "rs", "treatment.x.x", "treatment.x.x.x", "time", "env_rep", "tech_rep", "adj_conc", "adj_conc_mean", "adj_conc_sd") %>%
  rename(treatment_time = treatment.x.x,
         treatment= treatment.x.x.x,
         ori_adj_conc_mean = adj_conc_mean,
         ori_adj_sd_mean = adj_conc_sd) %>%
  mutate(mol_conc = adj_conc * 0.5) %>% #Moles in 500 uL (0.5 mL)
  mutate(mol_ori_conc = ori_adj_conc_mean * 0.25) %>% #Moles in spike of 250 uL (0.250 mL)
  mutate(recoveries = mol_conc/mol_ori_conc *100)

rs2_recoveries_summary <- rs2_recoveries %>% #Make a new data table with the averages and stdevs of the recoveries for each treatment
  group_by(quant_method, treatment_time, treatment) %>%
  summarise_at(vars("recoveries"), list(mean = mean, sd = sd), na.rm = TRUE)


#Export a table of the recoveries
write.csv(rs2_recoveries_summary,'results/RS_DCIP_LDOPA_MUBph_recoveries_table.csv')

#Boxplot of the recoveries
#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")

#Plot using facet_wrap (Plots for all quant_methods)
all_recoveries_barplots <- ggplot(data = rs2_recoveries_summary, aes(x = treatment_time, y = mean, fill = treatment)) + 
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd), width=0.2, linetype = "solid", size= 0.2) +
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recoveries (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method, scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_Allrecoveriesmethods_RStest.svg", all_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_Allrecoveriesmethods_RStest.pdf", all_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)




#Recovery Box Plots for MS quant_methods
rs2_recoveries_summary_ms <- rs2_recoveries_summary %>% 
  filter(quant_method != "DCIP_270nm") %>%
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "DCIP_267.99mz" = "DCIP",
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

ms_recoveries_barplots <- ggplot(data = rs2_recoveries_summary_ms, aes(x = treatment_time, y = mean, fill = treatment)) + 
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd), width=0.2, linetype = "solid", size= 0.2) +
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recoveries (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_MSrecoveriesmethods_RStest.svg", ms_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_MSrecoveriesmethods_RStest.pdf", ms_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)







#Box Plots for nm quant_methods
rs2_recoveries_summary_nm <- rs2_recoveries_summary %>% 
  filter(quant_method != "DCIP_267.99mz") %>%
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "DCIP_270nm" = "DCIP",
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")


nm_recoveries_barplots <- ggplot(data = rs2_recoveries_summary_nm, aes(x = treatment_time, y = mean, fill = treatment)) + 
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd), width=0.2, linetype = "solid", size= 0.2) +
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recoveries (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_NMrecoveriesmethods_RStest.svg", nm_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_NMrecoveriesmethods_RStest.pdf", nm_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)





#=============== Partition %Losses ================================================================================
#===#Calculate %Total Loss
rs2_loss <- rs2_recoveries %>% #For treatments and time points
  select("quant_method", "filename", "rs", "treatment_time", "treatment", "time", "env_rep", "tech_rep", "recoveries") %>%
  mutate(loss_total = 100 - recoveries)


#===#Calculate %Extraction Loss
rs2_loss_extr_A <- rs2_loss %>%
  filter(treatment == "A") %>%
  group_by(quant_method, treatment_time, treatment, time) %>%
  summarise_at(vars("loss_total"), list(mean = mean, sd = sd), na.rm = TRUE) %>%
  mutate(mean = ifelse(mean < 0, 0, mean)) %>%
  rename(A_loss_mean = mean,
         A_loss_sd = sd) 

rs2_loss_extr <- rs2_loss %>%
  right_join(rs2_loss_extr_A, by = c("quant_method", "time"))  %>%  #Add in Extraction loss calculated from group A
  select("quant_method", "filename", "rs", "treatment_time.x", "treatment.x", "time", "env_rep", "tech_rep", 
         "recoveries", "loss_total", "A_loss_mean") %>%
  rename(treatment_time = treatment_time.x,
         treatment = treatment.x,
         loss_extr = A_loss_mean)
  
  
#===#Calculate %Adsorption Loss based on linear regression using groups B and C
rs2_loss_ads_BC <- rs2_loss_extr %>%
  filter(treatment == "B" | treatment == "C") %>%
  right_join(rs_types, by = "treatment") %>%
  filter(soil_series != "Grenville" ) %>%
  filter(soil_series != "Rideau" ) %>%
  filter(soil_series != "Granby" ) %>%
  mutate(loss_abs_BC = loss_total - loss_extr) %>%
  mutate(loss_abs_BC_2 = ifelse(loss_abs_BC < 0, 0, loss_abs_BC))

#Look for linear correlations (obvious in this case b/c only 2 point curve)

ggscatter(rs2_loss_ads_BC, x = "cec", y = "loss_abs_BC",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)

ggscatter(rs2_loss_ads_BC, x = "specific_sa", y = "loss_abs_BC",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)

#Linear regression for each quant method for CEC
rs2_loss_ads_BC_DCIPmz <- rs2_loss_ads_BC %>%
  filter(quant_method == "DCIP_267.99mz")

loss_DCIPmz_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                        cec, 
                      data = rs2_loss_ads_BC_DCIPmz )

loss_DCIPmz_cec_table <- get_regression_table(loss_DCIPmz_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "DCIP_267.99mz")




rs2_loss_ads_BC_DCIPnm <- rs2_loss_ads_BC %>%
  filter(quant_method == "DCIP_270nm")

loss_DCIPnm_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                        cec, 
                      data = rs2_loss_ads_BC_DCIPnm )

loss_DCIPnm_cec_table <- get_regression_table(loss_DCIPnm_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "DCIP_270nm")




rs2_loss_ads_BC_LDOPAmz <- rs2_loss_ads_BC %>%
  filter(quant_method == "LDOPA_198mz")

loss_LDOPAmz_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                        cec, 
                      data = rs2_loss_ads_BC_LDOPAmz )

loss_LDOPAmz_cec_table <- get_regression_table(loss_LDOPAmz_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "LDOPA_198mz")






rs2_loss_ads_BC_LDOPAnm <- rs2_loss_ads_BC %>%
  filter(quant_method == "LDOPA_280nm")

loss_LDOPAnm_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         cec, 
                       data = rs2_loss_ads_BC_LDOPAnm )

loss_LDOPAnm_cec_table <- get_regression_table(loss_LDOPAnm_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "LDOPA_280nm")






rs2_loss_ads_BC_MUBphmz <- rs2_loss_ads_BC %>%
  filter(quant_method == "MUBph_257mz")

loss_MUBphmz_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         cec, 
                       data = rs2_loss_ads_BC_MUBphmz )

loss_MUBphmz_cec_table <- get_regression_table(loss_MUBphmz_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "MUBph_257mz")






rs2_loss_ads_BC_MUBphnm <- rs2_loss_ads_BC %>%
  filter(quant_method == "MUBph_315nm")

loss_MUBphnm_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         cec, 
                       data = rs2_loss_ads_BC_MUBphnm )

loss_MUBphnm_cec_table <- get_regression_table(loss_MUBphnm_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "MUBph_315nm")

cec_lm <- rbind(loss_MUBphnm_cec_table, loss_MUBphmz_cec_table, 
                loss_LDOPAnm_cec_table, loss_LDOPAmz_cec_table,
                loss_DCIPnm_cec_table, loss_DCIPmz_cec_table)


#Linear regression for each quant method for specific_SA

loss_DCIPmz_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                        specific_sa, 
                      data = rs2_loss_ads_BC_DCIPmz )

loss_DCIPmz_specific_sa_table <- get_regression_table(loss_DCIPmz_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "DCIP_267.99mz")






loss_DCIPnm_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                        specific_sa, 
                      data = rs2_loss_ads_BC_DCIPnm )

loss_DCIPnm_specific_sa_table <- get_regression_table(loss_DCIPnm_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "DCIP_270nm")





loss_LDOPAmz_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         specific_sa, 
                       data = rs2_loss_ads_BC_LDOPAmz )

loss_LDOPAmz_specific_sa_table <- get_regression_table(loss_LDOPAmz_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "LDOPA_198mz")







loss_LDOPAnm_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         specific_sa, 
                       data = rs2_loss_ads_BC_LDOPAnm )

loss_LDOPAnm_specific_sa_table <- get_regression_table(loss_LDOPAnm_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "LDOPA_280nm")







loss_MUBphmz_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         specific_sa, 
                       data = rs2_loss_ads_BC_MUBphmz )

loss_MUBphmz_specific_sa_table <- get_regression_table(loss_MUBphmz_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "MUBph_257mz")






loss_MUBphnm_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         specific_sa, 
                       data = rs2_loss_ads_BC_MUBphnm )

loss_MUBphnm_specific_sa_table <- get_regression_table(loss_MUBphnm_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "MUBph_315nm")

specific_sa_lm <- rbind(loss_MUBphnm_specific_sa_table, loss_MUBphmz_specific_sa_table, 
                loss_LDOPAnm_specific_sa_table, loss_LDOPAmz_specific_sa_table,
                loss_DCIPnm_specific_sa_table, loss_DCIPmz_specific_sa_table)



sa_cec_lms <- right_join(specific_sa_lm, cec_lm, by = "quant_method")








#Merge regression information into main dataset
rs2_loss_extr_abs <- rs2_loss_extr %>%
  right_join(sa_cec_lms, by = "quant_method") %>%
  full_join(rs_types, by = "treatment") %>%
  mutate(loss_abs_cec = slope_cec*cec + yint_cec) %>%
  mutate(loss_abs_sa = slope_specific_sa*specific_sa + yint_specific_sa) %>%
  select("quant_method", "filename", "rs", "treatment_time", "treatment", "time", "env_rep", "tech_rep", 
         "recoveries", "loss_total", "loss_extr", "loss_abs_cec") %>% 
  replace(is.na(.), 0)
  

#Check the plots
ggscatter(rs2_loss_extr_abs, x = "cec", y = "loss_abs_cec",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)

ggscatter(rs2_loss_extr_abs, x = "specific_sa", y = "loss_abs_sa",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)
  




#===#Calculate %Bio Loss based on linear regression using groups D
rs2_loss_extr_abs_bio <- rs2_loss_extr_abs %>%
  group_by(quant_method, treatment, treatment_time) %>%
  summarise_at(vars("recoveries", "loss_total", "loss_extr", "loss_abs_cec"), 
               list(mean = mean), na.rm = TRUE) %>%
  mutate(loss_bio = loss_total_mean - loss_extr_mean - loss_abs_cec_mean) %>%
  mutate(loss_bio2 = ifelse(loss_bio  < 0, 0, loss_bio )) %>%
  mutate(loss_bio3 = ifelse(treatment == "A", 0, loss_bio2 )) %>%
  mutate(loss_bio4 = ifelse(treatment == "B", 0, loss_bio3 )) %>%
  mutate(loss_bio5 = ifelse(treatment == "C", 0, loss_bio4 )) %>%
  select("quant_method", "treatment", "treatment_time", 
         "recoveries_mean", "loss_total_mean", "loss_extr_mean", "loss_abs_cec_mean", "loss_bio5") 


#===#Plot the recoveries and losses
#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")


rs2_all_loss <- rs2_loss_extr_abs_bio %>%
  select(-"loss_total_mean") %>%
  rename(recovered = recoveries_mean,
         "extraction loss" = loss_extr_mean,
         "absorption loss" = loss_abs_cec_mean,
         "other loss" = loss_bio5) %>%
  pivot_longer(-c(quant_method, treatment, treatment_time), names_to = "conditions", values_to = "percentage")


barplot_losses <- ggplot(data = rs2_all_loss, aes(x = treatment_time, y = percentage, 
                                fill = factor(conditions, levels = c("other loss", "absorption loss", "extraction loss", "recovered")))) + 
  geom_bar(stat="identity", position = "stack", alpha = 0.6) + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Percentage (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/BarplotStack_LossPartition_RStest.svg", barplot_losses, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotStack_LossPartition_RStest.pdf", barplot_losses, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Loss Partition Box Plots for MS quant_methods
rs2_all_loss_mz <- rs2_all_loss %>% 
  filter(quant_method != "DCIP_270nm") %>%
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "DCIP_267.99mz" = "DCIP",
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

barplot_losses_mz <- ggplot(data = rs2_all_loss_mz, aes(x = treatment_time, y = percentage, 
                                                  fill = factor(conditions, levels = c("other loss", "absorption loss", "extraction loss", "recovered")))) + 
  geom_bar(stat="identity", position = "stack", alpha = 0.6) + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Percentage (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/BarplotStack_LossPartitionMZ_RStest.svg", barplot_losses_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotStack_LossPartitionMZ_RStest.pdf", barplot_losses_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Bar Plots for nm quant_methods
rs2_all_loss_nm <- rs2_all_loss %>% 
  filter(quant_method != "DCIP_267.99mz") %>%
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "DCIP_270nm" = "DCIP",
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

barplot_losses_nm <- ggplot(data = rs2_all_loss_nm, aes(x = treatment_time, y = percentage, 
                                                  fill = factor(conditions, levels = c("other loss", "absorption loss", "extraction loss", "recovered")))) + 
  geom_bar(stat="identity", position = "fill", alpha = 0.6) + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Percentage (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/BarplotFill_LossPartitionNM_RStest.svg", barplot_losses_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotFill_LossPartitionNM_RStest.pdf", barplot_losses_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)





























#=============== Calculate the recoveries for REDUCED DATASET (NO TIME) ================================================================================
#Using the cooncentrations from the original spike mixes summarize the original concentrations
rs2_ori <- rs2 %>%
  filter(treatment.x == "ori") %>%
  unite(treatment, treatment.x, time, remove = FALSE) %>%
  group_by(quant_method, treatment.x, treatment) %>%
  summarise_at(vars("area", "adj_conc", "rt"), list(mean = mean, sd = sd), na.rm = TRUE)


rs2_recoveries <- rs2 %>% 
  filter(treatment.x != "ori") %>%
  filter(treatment.x != "QC") %>% 
  unite(treatment, treatment.x, time, remove = FALSE) %>%
  right_join(rs2_ori, by = "quant_method")  %>%  #Add in ori data
  select("quant_method", "filename", "rs", "treatment.x.x", "treatment.x.x.x", "time", "env_rep", "tech_rep", "adj_conc", "adj_conc_mean", "adj_conc_sd") %>%
  rename(treatment_time = treatment.x.x,
         treatment= treatment.x.x.x,
         ori_adj_conc_mean = adj_conc_mean,
         ori_adj_sd_mean = adj_conc_sd) %>%
  mutate(mol_conc = adj_conc * 0.5) %>% #Moles in 500 uL (0.5 mL)
  mutate(mol_ori_conc = ori_adj_conc_mean * 0.25) %>% #Moles in spike of 250 uL (0.250 mL)
  mutate(recoveries = mol_conc/mol_ori_conc *100)

rs2_recoveries_summary <- rs2_recoveries %>% #Make a new data table with the averages and stdevs of the recoveries for each treatment
  group_by(quant_method, treatment) %>%
  summarise_at(vars("recoveries"), list(mean = mean, sd = sd), na.rm = TRUE)


#Export a table of the recoveries
write.csv(rs2_recoveries_summary,'results/RS_DCIP_LDOPA_MUBph_Reduced_recoveries_table.csv')

#Boxplot of the recoveries
#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")

#Plot using facet_wrap (Plots for all quant_methods)
all_recoveries_Barplots <- ggplot(data = rs2_recoveries_summary, aes(x = treatment, y = mean, fill = treatment)) + 
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd), width=0.2, linetype = "solid", size= 0.2) +
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recoveries (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method, scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_AllrecoveriesmethodsREDUCED_RStest.svg", all_recoveries_Barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_AllrecoveriesmethodsREDUCED_RStest.pdf", all_recoveries_Barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)




#Recovery Bar Plots for MS quant_methods
rs2_recoveries_summary_ms <- rs2_recoveries_summary %>% 
  filter(quant_method != "DCIP_270nm") %>%
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "DCIP_267.99mz" = "DCIP",
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

ms_recoveries_Barplots <- ggplot(data = rs2_recoveries_summary_ms, aes(x = treatment, y = mean, fill = treatment)) + 
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd), width=0.2, linetype = "solid", size= 0.2) +
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recoveries (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_MSrecoveriesmethodsREDUCED_RStest.svg", ms_recoveries_Barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_MSrecoveriesmethodsREDUCED_RStest.pdf", ms_recoveries_Barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)







#Bar Plots for nm quant_methods
rs2_recoveries_summary_nm <- rs2_recoveries_summary %>% 
  filter(quant_method != "DCIP_267.99mz") %>%
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "DCIP_270nm" = "DCIP",
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")


nm_recoveries_Barplots <- ggplot(data = rs2_recoveries_summary_nm, aes(x = treatment, y = mean, fill = treatment)) + 
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd), width=0.2, linetype = "solid", size= 0.2) +
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recoveries (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_NMrecoveriesmethodsREDUCED_RStest.svg", nm_recoveries_Barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_NMrecoveriesmethodsREDUCED_RStest.pdf", nm_recoveries_Barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)





#=============== Partition %Losses for REDUCED DATASET (NO TIME) ================================================================================
#===#Calculate %Total Loss
rs2_loss <- rs2_recoveries %>% #For treatments and time points
  select("quant_method", "treatment", "recoveries") %>%
  mutate(loss_total = 100 - recoveries)


#===#Calculate %Extraction Loss
rs2_loss_extr_A <- rs2_loss %>%
  filter(treatment == "A") %>%
  group_by(quant_method, treatment) %>%
  summarise_at(vars("loss_total"), list(mean = mean, sd = sd), na.rm = TRUE) %>%
  mutate(mean = ifelse(mean < 0, 0, mean)) %>%
  rename(A_loss_mean = mean,
         A_loss_sd = sd) 

rs2_loss_extr <- rs2_loss %>%
  right_join(rs2_loss_extr_A, by = c("quant_method"))  %>%  #Add in Extraction loss calculated from group A
  select("quant_method", "treatment.x", 
         "recoveries", "loss_total", "A_loss_mean") %>%
  rename(treatment = treatment.x,
         loss_extr = A_loss_mean)


#===#Calculate %Adsorption Loss based on linear regression using groups B and C
rs2_loss_ads_BC <- rs2_loss_extr %>%
  filter(treatment == "B" | treatment == "C") %>%
  right_join(rs_types, by = "treatment") %>%
  filter(soil_series != "Grenville" ) %>%
  filter(soil_series != "Rideau" ) %>%
  filter(soil_series != "Granby" ) %>%
  mutate(loss_abs_BC = loss_total - loss_extr) %>%
  mutate(loss_abs_BC_2 = ifelse(loss_abs_BC < 0, 0, loss_abs_BC))

#Look for linear correlations (obvious in this case b/c only 2 point curve)

ggscatter(rs2_loss_ads_BC, x = "cec", y = "loss_abs_BC",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)

ggscatter(rs2_loss_ads_BC, x = "specific_sa", y = "loss_abs_BC",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)

#Linear regression for each quant method for CEC
rs2_loss_ads_BC_DCIPmz <- rs2_loss_ads_BC %>%
  filter(quant_method == "DCIP_267.99mz")

loss_DCIPmz_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                        cec, 
                      data = rs2_loss_ads_BC_DCIPmz )

loss_DCIPmz_cec_table <- get_regression_table(loss_DCIPmz_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "DCIP_267.99mz")




rs2_loss_ads_BC_DCIPnm <- rs2_loss_ads_BC %>%
  filter(quant_method == "DCIP_270nm")

loss_DCIPnm_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                        cec, 
                      data = rs2_loss_ads_BC_DCIPnm )

loss_DCIPnm_cec_table <- get_regression_table(loss_DCIPnm_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "DCIP_270nm")




rs2_loss_ads_BC_LDOPAmz <- rs2_loss_ads_BC %>%
  filter(quant_method == "LDOPA_198mz")

loss_LDOPAmz_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         cec, 
                       data = rs2_loss_ads_BC_LDOPAmz )

loss_LDOPAmz_cec_table <- get_regression_table(loss_LDOPAmz_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "LDOPA_198mz")






rs2_loss_ads_BC_LDOPAnm <- rs2_loss_ads_BC %>%
  filter(quant_method == "LDOPA_280nm")

loss_LDOPAnm_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         cec, 
                       data = rs2_loss_ads_BC_LDOPAnm )

loss_LDOPAnm_cec_table <- get_regression_table(loss_LDOPAnm_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "LDOPA_280nm")






rs2_loss_ads_BC_MUBphmz <- rs2_loss_ads_BC %>%
  filter(quant_method == "MUBph_257mz")

loss_MUBphmz_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         cec, 
                       data = rs2_loss_ads_BC_MUBphmz )

loss_MUBphmz_cec_table <- get_regression_table(loss_MUBphmz_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "MUBph_257mz")






rs2_loss_ads_BC_MUBphnm <- rs2_loss_ads_BC %>%
  filter(quant_method == "MUBph_315nm")

loss_MUBphnm_cec <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                         cec, 
                       data = rs2_loss_ads_BC_MUBphnm )

loss_MUBphnm_cec_table <- get_regression_table(loss_MUBphnm_cec) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_cec = cec,
         yint_cec = intercept) %>%
  mutate(quant_method = "MUBph_315nm")

cec_lm <- rbind(loss_MUBphnm_cec_table, loss_MUBphmz_cec_table, 
                loss_LDOPAnm_cec_table, loss_LDOPAmz_cec_table,
                loss_DCIPnm_cec_table, loss_DCIPmz_cec_table)


#Linear regression for each quant method for specific_SA

loss_DCIPmz_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                                specific_sa, 
                              data = rs2_loss_ads_BC_DCIPmz )

loss_DCIPmz_specific_sa_table <- get_regression_table(loss_DCIPmz_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "DCIP_267.99mz")






loss_DCIPnm_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                                specific_sa, 
                              data = rs2_loss_ads_BC_DCIPnm )

loss_DCIPnm_specific_sa_table <- get_regression_table(loss_DCIPnm_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "DCIP_270nm")





loss_LDOPAmz_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                                 specific_sa, 
                               data = rs2_loss_ads_BC_LDOPAmz )

loss_LDOPAmz_specific_sa_table <- get_regression_table(loss_LDOPAmz_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "LDOPA_198mz")







loss_LDOPAnm_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                                 specific_sa, 
                               data = rs2_loss_ads_BC_LDOPAnm )

loss_LDOPAnm_specific_sa_table <- get_regression_table(loss_LDOPAnm_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "LDOPA_280nm")







loss_MUBphmz_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                                 specific_sa, 
                               data = rs2_loss_ads_BC_MUBphmz )

loss_MUBphmz_specific_sa_table <- get_regression_table(loss_MUBphmz_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "MUBph_257mz")






loss_MUBphnm_specific_sa <- lm(loss_abs_BC ~ #Linear regression with no transformations of the X or Y
                                 specific_sa, 
                               data = rs2_loss_ads_BC_MUBphnm )

loss_MUBphnm_specific_sa_table <- get_regression_table(loss_MUBphnm_specific_sa) %>%
  select("term", "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(slope_specific_sa = specific_sa,
         yint_specific_sa = intercept) %>%
  mutate(quant_method = "MUBph_315nm")

specific_sa_lm <- rbind(loss_MUBphnm_specific_sa_table, loss_MUBphmz_specific_sa_table, 
                        loss_LDOPAnm_specific_sa_table, loss_LDOPAmz_specific_sa_table,
                        loss_DCIPnm_specific_sa_table, loss_DCIPmz_specific_sa_table)



sa_cec_lms <- right_join(specific_sa_lm, cec_lm, by = "quant_method")








#Merge regression information into main dataset
rs2_loss_extr_abs <- rs2_loss_extr %>%
  right_join(sa_cec_lms, by = "quant_method") %>%
  full_join(rs_types, by = "treatment") %>%
  mutate(loss_abs_cec = slope_cec*cec + yint_cec) %>%
  mutate(loss_abs_sa = slope_specific_sa*specific_sa + yint_specific_sa) %>%
  select("quant_method","treatment", 
         "recoveries", "loss_total", "loss_extr", "loss_abs_cec") %>% 
  replace(is.na(.), 0)



#Check the plots
ggscatter(rs2_loss_extr_abs, x = "cec", y = "loss_abs_cec",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)

ggscatter(rs2_loss_extr_abs, x = "specific_sa", y = "loss_abs_sa",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)





#===#Calculate %Bio Loss based on linear regression using groups D
rs2_loss_extr_abs_bio <- rs2_loss_extr_abs %>%
  group_by(quant_method, treatment) %>%
  summarise_at(vars("recoveries", "loss_total", "loss_extr", "loss_abs_cec"), 
               list(mean = mean), na.rm = TRUE) %>%
  mutate(loss_bio = loss_total_mean - loss_extr_mean - loss_abs_cec_mean) %>%
  mutate(loss_bio2 = ifelse(loss_bio  < 0, 0, loss_bio )) %>%
  mutate(loss_bio3 = ifelse(treatment == "A", 0, loss_bio2 )) %>%
  mutate(loss_bio4 = ifelse(treatment == "B", 0, loss_bio3 )) %>%
  mutate(loss_bio5 = ifelse(treatment == "C", 0, loss_bio4 )) %>%
  select("quant_method", "treatment",  
         "recoveries_mean", "loss_total_mean", "loss_extr_mean", "loss_abs_cec_mean", "loss_bio5") 


#===#Plot the recoveries and losses
#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")


rs2_all_loss <- rs2_loss_extr_abs_bio %>%
  select(-"loss_total_mean") %>%
  rename(recovered = recoveries_mean,
         "extraction loss" = loss_extr_mean,
         "absorption loss" = loss_abs_cec_mean,
         "other loss" = loss_bio5) %>%
  pivot_longer(-c(quant_method, treatment), names_to = "conditions", values_to = "percentage")


Barplot_losses <- ggplot(data = rs2_all_loss, aes(x = treatment, y = percentage, 
                                                  fill = factor(conditions, levels = c("other loss", "absorption loss", "extraction loss", "recovered")))) + 
  geom_bar(stat="identity", position = "fill", alpha = 0.6) + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Percentage (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/BarplotFill_LossPartitionREDUCED_RStest.svg", Barplot_losses, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotFill_LossPartitionREDUCED_RStest.pdf", Barplot_losses, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Loss Partition Bar Plots for MS quant_methods
rs2_all_loss_mz <- rs2_all_loss %>% 
  filter(quant_method != "DCIP_270nm") %>%
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "DCIP_267.99mz" = "DCIP",
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

Barplot_losses_mz <- ggplot(data = rs2_all_loss_mz, aes(x = treatment, y = percentage, 
                                                        fill = factor(conditions, levels = c("other loss", "absorption loss", "extraction loss", "recovered")))) + 
  geom_bar(stat="identity", position = "fill", alpha = 0.6) + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Percentage (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/BarplotFill_LossPartitionMZREDUCED_RStest.svg", Barplot_losses_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotFill_LossPartitionMZREDUCED_RStest.pdf", Barplot_losses_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Bar Plots for nm quant_methods
rs2_all_loss_nm <- rs2_all_loss %>% 
  filter(quant_method != "DCIP_267.99mz") %>%
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "DCIP_270nm" = "DCIP",
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

Barplot_losses_nm <- ggplot(data = rs2_all_loss_nm, aes(x = treatment, y = percentage, 
                                                        fill = factor(conditions, levels = c("other loss", "absorption loss", "extraction loss", "recovered")))) + 
  geom_bar(stat="identity", position = "fill", alpha = 0.6) + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_fill_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Percentage (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/BarplotFill_LossPartitionNMREDUCED_RStest.svg", Barplot_losses_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotFill_LossPartitionNMREDUCED_RStest.pdf", Barplot_losses_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)
















#=============== ANOVAs and Kruskal-Wallis ==================================================================================================
#ANOVA = Analysis of variance: requires a continuous dependent variable (Y), and one independent categorical variable (X)
#===#Generate dataframes for linear regressions (only for ms methods b/c most senstive)

rs2_anovas <- rs2_recoveries %>%
  select("quant_method", "rs", "treatment_time", "treatment", "time", "env_rep", "tech_rep", "recoveries")

rs2_anovas$time <-as.factor(rs2_anovas$time)

# Data transformations if the data is heteroscedastic and/or the residuals are non-normal than must transform the y-variable and/or x-variable. 
# Below different methods for trasnforming variables

p1 <- powerTransform(recoveries ~ treatment, data = rs2_anovas) #BoxCox transormation
rs2_anovas <- cbind(rs2_anovas, y_boxcox=bcPower(rs2_anovas$recoveries, p1$roundlam))

rs2_anovas <- rs2_anovas %>% #Log Transformation of Y
  mutate(y_log = log(recoveries)) 

rs2_anovas <- rs2_anovas %>% #Reciprocal of Y
  mutate(y_recip = recoveries^-1)



#===#Look for linear correlations

ggpairs(data=rs2_anovas)    #Looking for linear correlations

ggscatter(rs2_anovas, x = "treatment_time", y = "recoveries",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)


#===#Performing Linear regressions

#Linear regressions on all groups
fit_basic <- rs2_anovas %>% 
  group_by(quant_method) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(recoveries ~ treatment*time, data = .))) 

#fit_basic_table <- map(fit_basic$model, 
 #                      function(x) {
  #                       p <- get_regression_table(x)
   #                      cbind(par=rownames(p), p)
    #                   }) %>% 
  #map_df(as_tibble, .id = "id") %>%
  #pivot_wider(names_from = "id",
   #         values_from = c("term", "estimate", "std_error", "statistic","p_value", "lower_ci", "upper_ci")) ## all parameters and error estimates as table


#fit_basic <- rs2_anovas %>% 
  #Group by quant_method column
#  group_by(quant_method) %>%
  #do the linear models by grouping var
#  do(model = lm(recoveries ~ treatment, data = .)) %>%
  #tidy lm object and order it as tibble
#  tidy(model)


#Linear regressions on all groups
fit_basic_DCIPmz <- fit_basic[[1,"model"]]

DCIPmz_regCoef_table <- get_regression_table(fit_basic_DCIPmz) %>%
  select("term", "estimate", "std_error", "statistic", "p_value", "lower_ci", "upper_ci") %>%
  mutate(quant_method = "DCIP_267.99mz")

DCIPmz_regSum_table <- get_regression_summaries(fit_basic_DCIPmz) %>%
  select("r_squared", "adj_r_squared", "mse", "rmse", "sigma") %>%
  mutate(quant_method = "DCIP_267.99mz")




fit_basic_DCIPnm <- fit_basic[[2,"model"]]

DCIPnm_regCoef_table <- get_regression_table(fit_basic_DCIPnm) %>%
  select("term", "estimate", "std_error", "statistic", "p_value", "lower_ci", "upper_ci") %>%
  mutate(quant_method = "DCIP_270nm")

DCIPnm_regSum_table <- get_regression_summaries(fit_basic_DCIPnm) %>%
  select("r_squared", "adj_r_squared", "mse", "rmse", "sigma") %>%
  mutate(quant_method = "DCIP_270nm")




fit_basic_LDOPAmz <- fit_basic[[3,"model"]]

LDOPAmz_regCoef_table <- get_regression_table(fit_basic_LDOPAmz) %>%
  select("term", "estimate", "std_error", "statistic", "p_value", "lower_ci", "upper_ci") %>%
  mutate(quant_method = "LDOPA_198mz")

LDOPAmz_regSum_table <- get_regression_summaries(fit_basic_LDOPAmz) %>%
  select("r_squared", "adj_r_squared", "mse", "rmse", "sigma") %>%
  mutate(quant_method = "LDOPA_198mz")




fit_basic_LDOPAnm <- fit_basic[[4,"model"]]

LDOPAnm_regCoef_table <- get_regression_table(fit_basic_LDOPAnm) %>%
  select("term", "estimate", "std_error", "statistic", "p_value", "lower_ci", "upper_ci") %>%
  mutate(quant_method = "LDOPA_280nm")

LDOPAnm_regSum_table <- get_regression_summaries(fit_basic_LDOPAnm) %>%
  select("r_squared", "adj_r_squared", "mse", "rmse", "sigma") %>%
  mutate(quant_method = "LDOPA_280nm")




fit_basic_MUBphmz <- fit_basic[[5,"model"]]

MUBphmz_regCoef_table <- get_regression_table(fit_basic_MUBphmz) %>%
  select("term", "estimate", "std_error", "statistic", "p_value", "lower_ci", "upper_ci") %>%
  mutate(quant_method = "MUBph_257mz")

MUBphmz_regSum_table <- get_regression_summaries(fit_basic_MUBphmz) %>%
  select("r_squared", "adj_r_squared", "mse", "rmse", "sigma") %>%
  mutate(quant_method = "MUBph_257mz")




fit_basic_MUBphnm <- fit_basic[[6,"model"]]

MUBphnm_regCoef_table <- get_regression_table(fit_basic_MUBphnm) %>%
  select("term", "estimate", "std_error", "statistic", "p_value", "lower_ci", "upper_ci") %>%
  mutate(quant_method = "MUBph_315nm")

MUBphnm_regSum_table <- get_regression_summaries(fit_basic_MUBphnm) %>%
  select("r_squared", "adj_r_squared", "mse", "rmse", "sigma") %>%
  mutate(quant_method = "MUBph_315nm")



#Export Master Coefficients Table
RegCoef_Treatment_table <- rbind(DCIPmz_regCoef_table, DCIPnm_regCoef_table, LDOPAmz_regCoef_table , LDOPAnm_regCoef_table, 
                                 MUBphmz_regCoef_table, MUBphnm_regCoef_table)

write.csv(RegCoef_Treatment_table,'results/RS_RegCoef_TreatmentTime_table.csv')
write.csv(RegCoef_Treatment_table,'results/RS_RegCoef_TreatmentTime_table_boxcox.csv') #Use if boxcox transformation applied
write.csv(RegCoef_Treatment_table,'results/RS_RegCoef_TreatmentTime_table_log.csv') #Use if log transformation applied
write.csv(RegCoef_Treatment_table,'results/RS_RegCoef_TreatmentTime_table_recip.csv') #Use if reciprocal transformation applied

#Export Master Model Summaries Table
RegSum_Treatment_table <- rbind(DCIPmz_regSum_table, DCIPnm_regSum_table, LDOPAmz_regSum_table , LDOPAnm_regSum_table, 
                                 MUBphmz_regSum_table, MUBphnm_regSum_table)

write.csv(RegSum_Treatment_table,'results/RS_RegSum_TreatmentTime_table.csv')
write.csv(RegSum_Treatment_table,'results/RS_RegSum_TreatmentTime_table_boxcox.csv') #Use if boxcox transformation applied
write.csv(RegSum_Treatment_table,'results/RS_RegSum_TreatmentTime_table_log.csv') #Use if log transformation applied
write.csv(RegSum_Treatment_table,'results/RS_RegSum_TreatmentTime_table_recip.csv') #Use if reciprocal transformation applied


#===#Test Assumptions are valid for ANOVA by:
###Select the model
o <- fit_basic_MUBphnm
      #Add in model being tested #fit_basic_DCIPmz, fit_basic_DCIPnm, fit_basic_LDOPAmz, fit_basic_LDOPAnm, fit_basic_MUBphmz, fit_basic_MUBphnm

#### Inspect the model diagnostic metrics
model.metrics <- augment(o) %>%   #Change the name of the regression
  select(-.hat, -.sigma, -.fitted, -.se.fit) 
head(model.metrics)

####1. Assess assumptions for ANOVA; The residuals must be homoscedastic
ncvTest(o) #NCV test for heteroscedascitity where p-value < 0.5 indicates heteroscedascity

bartlett.test(.resid ~ interaction(treatment, time), data = model.metrics)  #Barlett test for heteroscedascitity where p-value < 0.5 indicates heteroscedascity

leveneTest(.resid ~ treatment*time, data = model.metrics)  #Levene Test for heteroscedascity where p-value < 0.5 indicates heteroscedastic

plot(o, 1) #Look at the chart of residuals vs fitted values. The residuals should be uniformly random around the zero x-axes and do not form clusters (red line should be horizonal)
plot(o, 3) #plot of standardised residuals on Y axis (scale-location). If there is absolutely no heteroscedastity, you should see a completely random, equal distribution of points throughout the range of X axis and a flat red line.
plot(o, 5) #Identifies Outliers; where points in upper and/or lower right corners are points, outside of the red dashed cooks distance line can be removed

####2. Assess assumptions for ANCOVA: Residuals must be normal

shapiro_test(residuals(o)) # Check normality assumption by analyzing the model residuals.

ks.test(model.metrics$.resid, "pnorm", mean=mean(model.metrics$.resid), sd=sd(model.metrics$.resid)) #More general test; rejects Ho less often

jarque.bera.test(model.metrics$.resid)

ggplot(data=o, aes(model.metrics$.resid)) +  #Assess Normality of residuals; looking for a Gaussian shaped curve
  geom_histogram()

plot(o,2) #Look at the Q-Q plot which should have a straight line


#===#Performing a ANOVA's for all data (Ref https://www.datanovia.com/en/lessons/ancova-in-r/)

#Run Anova's for each group
rs2_anova_results <- rs2_anovas  %>%
  group_by(quant_method) %>%
  anova_test(y_boxcox ~ treatment*time)

write.csv(rs2_anova_results,'results/RS_ANOVA_TreatmentTime_table_boxcox.csv')


#Run Kruskal Wallis if data is heteroscedastic and/or non-normal

rs2_kw_results <- rs2_anovas  %>%
  group_by(quant_method) %>%
  kruskal_test(recoveries ~ time) #Can't do interaction terms in Kruskal Wallis so use merged factor

write.csv(rs2_kw_results,'results/RS_KW_RecoveriesVsTime_table.csv')

#===#Pairwise Comparisons
#####Effect of treatment on each sample using tukey hsd (must be homoscedastic and normal)
pwc <- rs2_anovas %>% 
  group_by(quant_method) %>%
  tukey_hsd(recoveries ~ treatment*time) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

write.csv(pwc ,'results/RS_TUKEYHSD_TreatmentTime_table.csv')

#Run Wilcoxon pairwise comparison if data is heteroscedastic and/or non-normal
my_comparisons <- list(c("A_1","A_2"), c("A_1","A_4"),c("A_2","A_4"),
                       c("B_1","B_2"), c("B_1","B_4"),c("B_2","B_4"),
                       c("C_1","C_2"), c("C_1","C_4"),c("C_2","C_4"),
                       c("Dgs_1","Dgs_2"), c("Dgs_1","Dgs_4"),c("Dgs_2","Dgs_4"),
                       c("Dfo_1","Dfo_2"), c("Dfo_1","Dfo_4"),c("Dfo_2","Dfo_4"),
                       c("Drc_1","Drc_2"), c("Drc_1","Drc_4"),c("Drc_2","Drc_4"))

my_comparisons2 <- list(c("A","B"), c("A","C"), c("A","Dgs"), c("A","Dfo"), c("A","Drc"), 
                        c("B","C"), c("B","Dgs"), c("B","Dfo"), c("B","Drc"),
                        c("C","Dgs"), c("C","Dfo"), c("C","Drc"),
                        c("Dgs","Dfo"), c("Dgs","Drc"),
                        c("Dfo","Drc"))

pkw <- rs2_anovas %>% 
  group_by(quant_method) %>%
  wilcox_test(data =., recoveries ~ treatment_time, 
              comparisons = my_comparisons, 
              p.adjust.method = "BH") 

write.csv(pkw ,'results/RS_WILCOX_Treatment_Time_table.csv')


#=============== Boxplots ==================================================================================================
#===#Generate dataframes for linear regressions (only for ms methods b/c most senstive)

rs2_boxplots <- rs2_recoveries %>%
  select("quant_method", "rs", "treatment_time", "treatment", "time", "env_rep", "tech_rep", "recoveries")

rs2_boxplots$time <-as.factor(rs2_boxplots$time)

# Data transformations if the data is heteroscedastic and/or the residuals are non-normal than must transform the y-variable and/or x-variable. 
# Below different methods for trasnforming variables

p1 <- powerTransform(recoveries ~ treatment, data = rs2_boxplots) #BoxCox transormation
rs2_boxplots<- cbind(rs2_boxplots, y_boxcox=bcPower(rs2_boxplots$recoveries, p1$roundlam))

#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")

#Boxplot for all samples
boxplot_recoveries <- ggplot(data = rs2_boxplots, aes(x = treatment_time, y = recoveries, color = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_Allrecoveriesmethods_RStest.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_Allrecoveriesmethods_RStest.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_AllrecoveriesmethodsREDUCED_RStest.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_AllrecoveriesmethodsREDUCED_RStest.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_Allrecoveriesmethods_RStest_boxcox.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_Allrecoveriesmethods_RStest_boxcox.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_AllrecoveriesmethodsREDUCED_RStest_boxcox.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_AllrecoveriesmethodsREDUCED_RStest_boxcox.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#Recovery Box Plots for MS quant_methods
rs2_boxplots_mz <- rs2_boxplots %>% 
  filter(quant_method != "DCIP_270nm") %>%
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "DCIP_267.99mz" = "DCIP",
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")


boxplot_recoveries_mz <- ggplot(data = rs2_boxplots_mz, aes(x = treatment_time, y = recoveries, color = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method, scales = "free_y", labeller = as_labeller(method.labs))


#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_MSrecoveriesmethods_RStest.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSrecoveriesmethods_RStest.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_MSrecoveriesmethodsREDUCED_RStest.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSrecoveriesmethodsREDUCED_RStest.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_MSrecoveriesmethods_RStest_boxcox.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSrecoveriesmethods_RStest_boxcox.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_MSrecoveriesmethodsREDUCED_RStest_boxcox.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSrecoveriesmethodsREDUCED_RStest_boxcox.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Recovery Box Plots for nm quant_methods
rs2_boxplots_nm <- rs2_boxplots %>% 
  filter(quant_method != "DCIP_267.99mz") %>%
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "DCIP_270nm" = "DCIP",
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

boxplot_recoveries_nm <- ggplot(data = rs2_boxplots_nm, aes(x = treatment_time, y = recoveries, color = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_NMrecoveriesmethods_RStest.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMrecoveriesmethods_RStest.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_NMrecoveriesmethodsREDUCED_RStest.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMrecoveriesmethodsREDUCED_RStest.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_NMrecoveriesmethods_RStest_boxcox.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMrecoveriesmethods_RStest_boxcox.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_NMrecoveriesmethodsREDUCED_RStest_boxcox.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMrecoveriesmethodsREDUCED_RStest_boxcox.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)











#=============== Checking Technical Replicates and Boxplots ==================================================================================================
#===#Generate dataframes for linear regressions (only for ms methods b/c most senstive)
rs2_anovas_tech <- rs2_recoveries %>%
  select("quant_method", "rs", "treatment_time","treatment","time", "env_rep", "tech_rep", "recoveries") %>%
  unite("time_tech", c("time", "tech_rep"), remove = FALSE) %>%
  unite("treatment_tech", c("treatment", "tech_rep"), remove = FALSE) %>%
  unite("treatment_time_tech", c("treatment_time", "tech_rep"), remove = FALSE)

#Run Kruskal Wallis if data is heteroscedastic and/or non-normal
RStest_kw_TR_results <- rs2_anovas_tech  %>%
  group_by(quant_method) %>%
  kruskal_test(recoveries ~ tech_rep) #Can't do interaction terms in Kruskal Wallis so use merged factor

write.csv(RStest_kw_TR_results,'results/RStest_KW_RecoveriesVsTechRep_table.csv')

RStest_kw_TimeTR_results <- rs2_anovas_tech  %>%
  group_by(quant_method) %>%
  kruskal_test(recoveries ~ time_tech) #Can't do interaction terms in Kruskal Wallis so use merged factor

write.csv(RStest_kw_TimeTR_results,'results/RStest_KW_RecoveriesVsTimeTechRep_table.csv')

RStest_kw_TreatmentTR_results <- rs2_anovas_tech  %>%
  group_by(quant_method) %>%
  kruskal_test(recoveries ~ treatment_tech) #Can't do interaction terms in Kruskal Wallis so use merged factor

write.csv(RStest_kw_TreatmentTR_results,'results/RStest_KW_RecoveriesVsTreatmentTechRep_table.csv')

RStest_kw_TreatmentTimeTR_results <- rs2_anovas_tech  %>%
  group_by(quant_method) %>%
  kruskal_test(recoveries ~ treatment_time_tech) #Can't do interaction terms in Kruskal Wallis so use merged factor

write.csv(RStest_kw_TreatmentTimeTR_results,'results/RStest_KW_RecoveriesVsTreatmentTimeTechRep_table.csv')

#Run Wilcoxon pairwise comparison if data is heteroscedastic and/or non-normal
techrep_comparisons <- list(c("1","2"))

pkw_TR <- rs2_anovas_tech %>% 
  group_by(quant_method) %>%
  wilcox_test(data =., recoveries ~ tech_rep, 
              comparisons = techrep_comparisons, 
              p.adjust.method = "BH") 

write.csv(pkw_TR,'results/RStest_WILCOX_RecoveriesVsTechRep_table.csv')



timetech_comparisons <- list(c("1_1","1_2"), c("2_1","2_2"), c("4_1","4_2")) 

pkw_TimeTR <- rs2_anovas_tech %>% 
  group_by(quant_method) %>%
  wilcox_test(data =., recoveries ~ time_tech, 
              comparisons = timetech_comparisons, 
              p.adjust.method = "BH") 

write.csv(pkw_TimeTR,'results/RStest_WILCOX_RecoveriesVsTimeTechRep_table.csv')




treatmenttech_comparisons <- list(c("A_1","A_2"), c("B_1","B_2"), c("C_1","C_2"),
                                  c("Dfo_1","Dfo_2"), c("Dgs_1","Dgs_2"), c("Drc_1","Drc_2"))

pkw_TreatmentTR <- rs2_anovas_tech %>% 
  group_by(quant_method) %>%
  wilcox_test(data =., recoveries ~ treatment_tech, 
              comparisons = treatmenttech_comparisons, 
              p.adjust.method = "BH") 

write.csv(pkw_TreatmentTR,'results/RStest_WILCOX_RecoveriesVsTreatmentTechRep_table.csv')




treatmenttimetech_comparisons <- list(c("A_1_1","A_1_2"), c("A_2_1","A_2_2"), c("A_4_1","A_4_2"),
                                      c("B_1_1","B_1_2"), c("B_2_1","B_2_2"), c("B_4_1","B_4_2"), 
                                      c("C_1_1","C_1_2"), c("C_2_1","C_2_2"), c("C_4_1","C_4_2"), 
                                      c("Dfo_1_1","Dfo_1_2"), c("Dfo_2_1","Dfo_2_2"), c("Dfo_4_1","Dfo_4_2"), 
                                      c("Dgs_1_1","Dgs_1_2"), c("Dgs_2_1","Dgs_2_2"), c("Dgs_4_1","Dgs_4_2"), 
                                      c("Drc_1_1","Drc_1_2"), c("Drc_2_1","Drc_2_2"), c("Drc_4_1","Drc_4_2"))

pkw_TreatmentTimeTR <- rs2_anovas_tech %>% 
  group_by(quant_method) %>%
  wilcox_test(data =., recoveries ~ treatment_time_tech, 
              comparisons = treatmenttimetech_comparisons, 
              p.adjust.method = "BH") 

write.csv(pkw_TreatmentTimeTR,'results/RStest_WILCOX_RecoveriesVsTreatmentTimeTechRep_table.csv')

#=============== Boxplots for techrep comparisons ==================================================================================================
#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")

###===TECH_REPS BOXPLOT===###
#Set order of variables
rs2_anovas_tech$tech_rep2 <- factor(rs2_anovas_tech$tech_rep, levels = c("1", "2"))

#Boxplot for all tech replicates for all quant_methods
boxplot_recoveries <- ggplot(data = rs2_anovas_tech, aes(x = tech_rep2, y = recoveries, color = tech_rep2)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method , scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_TechRepComp_TR_RStest.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_TR_RStest.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#Recovery Box Plots for MS quant_methods
rs2_anovas_tech_mz <- rs2_anovas_tech %>% 
  filter(quant_method != "DCIP_270nm") %>%
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "DCIP_267.99mz" = "DCIP",
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

boxplot_recoveries_mz <- ggplot(data = rs2_anovas_tech_mz, aes(x = tech_rep2, y = recoveries, color = tech_rep2)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method, scales = "free_y", labeller = as_labeller(method.labs))


#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_MSTechRepComp_TR_RStest.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_TR_RStest.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Recovery Box Plots for nm quant_methods
rs2_anovas_tech_nm <- rs2_anovas_tech %>% 
  filter(quant_method != "DCIP_267.99mz") %>%
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "DCIP_270nm" = "DCIP",
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

boxplot_recoveries_nm <- ggplot(data = rs2_anovas_tech_nm, aes(x = tech_rep2, y = recoveries, color = tech_rep2)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_NMTechRepComp_TR_RStest.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMTechRepComp_TR_RStest.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)











###===TIME_TECH BOXPLOT===###
rs2_anovas_tech$time2 <- factor(rs2_anovas_tech$time, levels = c("1", "2", "4"))

#Boxplot for all tech replicates for all quant_methods
boxplot_recoveries <- ggplot(data = rs2_anovas_tech, aes(x = time_tech, y = recoveries, color = time2)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method , scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_TechRepComp_TimeTR_RStest.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_TimeTR_RStest.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#Recovery Box Plots for MS quant_methods
rs2_anovas_tech_mz <- rs2_anovas_tech %>% 
  filter(quant_method != "DCIP_270nm") %>%
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "DCIP_267.99mz" = "DCIP",
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

boxplot_recoveries_mz <- ggplot(data = rs2_anovas_tech_mz, aes(x = time_tech, y = recoveries, color = time2)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method, scales = "free_y", labeller = as_labeller(method.labs))


#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_MSTechRepComp_TimeTR_RStest.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_TimeTR_RStest.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Recovery Box Plots for nm quant_methods
rs2_anovas_tech_nm <- rs2_anovas_tech %>% 
  filter(quant_method != "DCIP_267.99mz") %>%
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "DCIP_270nm" = "DCIP",
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

boxplot_recoveries_nm <- ggplot(data = rs2_anovas_tech_nm, aes(x = time_tech, y = recoveries, color = time2)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_NMTechRepComp_TimeTR_RStest.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMTechRepComp_TimeTR_RStest.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)













###===TREATMENT_TECH BOXPLOT===###
#Boxplot for all tech replicates for all quant_methods
boxplot_recoveries <- ggplot(data = rs2_anovas_tech, aes(x = treatment_tech, y = recoveries, color = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method , scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_TechRepComp_TreatmentTR_RStest.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_TreatmentTR_RStest.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#Recovery Box Plots for MS quant_methods
rs2_anovas_tech_mz <- rs2_anovas_tech %>% 
  filter(quant_method != "DCIP_270nm") %>%
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "DCIP_267.99mz" = "DCIP",
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

boxplot_recoveries_mz <- ggplot(data = rs2_anovas_tech_mz, aes(x = treatment_tech, y = recoveries, color = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method, scales = "free_y", labeller = as_labeller(method.labs))


#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_MSTechRepComp_TreatmentTR_RStest.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_TreatmentTR_RStest.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Recovery Box Plots for nm quant_methods
rs2_anovas_tech_nm <- rs2_anovas_tech %>% 
  filter(quant_method != "DCIP_267.99mz") %>%
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "DCIP_270nm" = "DCIP",
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

boxplot_recoveries_nm <- ggplot(data = rs2_anovas_tech_nm, aes(x = treatment_tech, y = recoveries, color = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_NMTechRepComp_TreatmentTR_RStest.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMTechRepComp_TreatmentTR_RStest.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)












###===TREATMENT_TIME_TECH BOXPLOT===###
#Boxplot for all tech replicates for all quant_methods
boxplot_recoveries <- ggplot(data = rs2_anovas_tech, aes(x = treatment_time_tech, y = recoveries, color = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method , scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_TechRepComp_TreatmentTimeTR_RStest.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_TreatmentTimeTR_RStest.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#Recovery Box Plots for MS quant_methods
rs2_anovas_tech_mz <- rs2_anovas_tech %>% 
  filter(quant_method != "DCIP_270nm") %>%
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "DCIP_267.99mz" = "DCIP",
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

boxplot_recoveries_mz <- ggplot(data = rs2_anovas_tech_mz, aes(x = treatment_time_tech, y = recoveries, color = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method, scales = "free_y", labeller = as_labeller(method.labs))


#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_MSTechRepComp_TreatmentTimeTR_RStest.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_TreatmentTimeTR_RStest.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Recovery Box Plots for nm quant_methods
rs2_anovas_tech_nm <- rs2_anovas_tech %>% 
  filter(quant_method != "DCIP_267.99mz") %>%
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "DCIP_270nm" = "DCIP",
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

boxplot_recoveries_nm <- ggplot(data = rs2_anovas_tech_nm, aes(x = treatment_time_tech, y = recoveries, color = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0,"cm"),
        axis.line=element_line(colour="black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size=10, colour = "black", angle = 90, vjust = 0.05, hjust = 1),
        # axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, colour = "black"),
        # axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, colour="black", face = "bold"),
        legend.text = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        text = element_text(size=12,  family="Arial")) +
  # plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
  scale_color_manual(values = cbbPalette) + ### here I tell R to use my custom colour palette
  ylab("Recovery (%)") + 
  xlab("Technical Replicates") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_NMTechRepComp_TreatmentTimeTR_RStest.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMTechRepComp_TreatmentTimeTR_RStest.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)
