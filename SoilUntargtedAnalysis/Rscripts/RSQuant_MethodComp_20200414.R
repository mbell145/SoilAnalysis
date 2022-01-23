#############################################################################################################
######################   Recovery Standard Quant  #############################################################
#############################################################################################################
##############################################################################################################

## Let's start with a black slate - Let's re-start the R session

# Ctrl + Shift + Fn + F10  (PC / Linux)
# Command + Shift + Fn+ F10 (Mac OS)

#AUTHOR: Madison Bell
#Date updated: 2020-04-16


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


rs <-read_csv("data_raw/SoilMethodTest_H2O_LDOPA_MUBph_Quant_20200414.csv")
#rs <-read_csv("data_raw/SoilMethodTest_MeOH_MUBph_Quant_20200414.csv")
rs <- clean_names(rs)

#Quant Method  regression  slope  y_intercept   r2
#---------------------------------------------------
#S1            str         num    num           num  
#S1            str         num    num           num
#S1            str         num    num           num
#S1            str         num    num           num
#S1            str         num    num           num
#S1            str         num    num           num

rs_reg <-read_csv("data_raw/SoilMethodTest_H2O_LDOPA_MUBph_Quant_Regression_20200414.csv")
#rs_reg <-read_csv("data_raw/SoilMethodTest_MeOH_MUBph_Quant_Regression_20200414.csv")
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

write.csv(rs_quant_lod_loq,'results/MethodComp_LDOPA_MUBph_lod_loq_table.csv')
write.csv(rs_quant_lod_loq,'results/MethodComp_MEOH_MUBph_lod_loq_table.csv')


#===============Process the data using LOD and LOQ ==================================================================================================
#Make a dataframe for quantification data
rs2 <- rs %>%
  filter(sample_type == "Sample") %>%
  select("quant_method", "filename", "rs", "treatment", "comp_method", "soil_type", "env_rep", "tech_rep", "calc_conc") %>%
  right_join(rs_quant_lod_loq, by = "quant_method") %>% #Add in LOD and LOQ data
  replace_na(list(calc_conc = 0)) %>% #Replace all NAs with 0
  mutate(adj_conc = ifelse(calc_conc <= loq, lod/sqrt(2), calc_conc)) #New column where values <= loq are replace with lod/sqrt(2)

#=============== Bar plots of RS Quant data ==================================================================================================
rs2_bar <- rs2 %>% 
  filter(treatment.x != "QC") %>%
  filter(treatment.x != "ori") %>%
  group_by(quant_method, comp_method, soil_type, treatment.x) %>%
  summarise_at(vars("adj_conc"), list(mean = mean, sd = sd), na.rm = TRUE) %>%
  unite("treatment2", c("soil_type", "comp_method"), remove = FALSE)

#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#57d0bc", "#D55E00", "#CC79A7","#dcce87",
                "#d0dc87", "#999999", "#dcb087", "#8962ab", "#ab628c", "#ab627a", "#1a6d41", "#e3e6e5", "#eed09b")

#Set order of variables
rs2_bar$treatment3 <- factor(rs2_bar$treatment.x, levels = c("CF_MB", "CF_kc", "CF_mc", "CF_fo", "CF_gs", "CF_rc",
                                                              "SO_MB", "SO_kc", "SO_mc", "SO_fo", "SO_gs", "SO_rc",
                                                              "SH_MB", "SH_kc", "SH_mc", "SH_fo", "SH_gs", "SH_rc"))

#Set order of variables
rs2_bar$treatment4 <- factor(rs2_bar$treatment2, levels = c("MB_CF", "MB_SO", "MB_SH",
                                                              "kc_CF", "kc_SO", "kc_SH",
                                                              "mc_CF", "mc_SO", "mc_SH",
                                                              "fo_CF", "fo_SO", "fo_SH",
                                                              "gs_CF", "gs_SO", "gs_SH",
                                                              "rc_CF", "rc_SO", "rc_SH"))

#Plot using facet_wrap (Plots for all quant_methods)
all_quant_barplots <- ggplot(data = rs2_bar,aes(x = treatment4, y = mean, fill = comp_method)) + 
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
  ylab("Concentration (mg/mL)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method, scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_Allquantmethods_3colours_MethodComp.svg", all_quant_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_Allquantmethods_3colours_MethodComp.pdf", all_quant_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_Allquantmethods_MEOH_MethodComp.svg", all_quant_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_Allquantmethods_MEOH_MethodComp.pdf", all_quant_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)






#Box Plots for MS quant_methods
rs2_bar_ms <- rs2_bar %>% 
  #filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  #"LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

ms_quant_barplots <- ggplot(data = rs2_bar_ms,aes(x = treatment4, y = mean, fill = comp_method)) + 
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
  ylab("Concentration (mg/mL)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_MSquantmethods_MethodComp.svg", ms_quant_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_MSquantmethods_MethodComp.pdf", ms_quant_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_MSquantmethods_MEOH_MethodComp.svg", ms_quant_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_MSquantmethods_MEOH_MethodComp.pdf", ms_quant_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)







#Box Plots for nm quant_methods
rs2_bar_nm <- rs2_bar %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")


nm_quant_barplots <- ggplot(data = rs2_bar_nm,aes(x = treatment4, y = mean, fill = comp_method)) + 
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
  ylab("Concentration (mg/mL)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_NMquantmethods_MethodComp.svg", nm_quant_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_NMquantmethods_MethodComp.pdf", nm_quant_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)







#=============== Calculate the recoveries ================================================================================
#Using the cooncentrations from the original spike mixes summarize the original concentrations
rs2_ori <- rs2 %>%
  filter(treatment.x == "ori") %>%
  group_by(quant_method, treatment.x, comp_method) %>%
  summarise_at(vars("adj_conc"), list(mean = mean, sd = sd), na.rm = TRUE) %>%
  rename(ori_mean_conc = mean,
         ori_sd_conc = sd)


rs2_recoveries <- rs2 %>% 
  filter(treatment.x != "ori") %>%
  filter(treatment.x != "QC") %>% 
  right_join(rs2_ori, by = "quant_method")  %>%  #Add in ori data
  select("quant_method", "filename", "rs", "treatment.x.x", "comp_method.x", "soil_type", "env_rep", "tech_rep", "adj_conc", "ori_mean_conc", "ori_sd_conc") %>%
  rename(treatment = treatment.x.x,
         comp_method = comp_method.x,
         ori_adj_conc_mean = ori_mean_conc,
         ori_adj_sd_mean = ori_sd_conc) %>%
  unite("treatment2", c("soil_type", "comp_method"), remove = FALSE) %>%
  mutate(mol_conc = adj_conc * 0.5) %>% #Moles in 500 uL (0.5 mL)
  mutate(mol_ori_conc = ori_adj_conc_mean * 0.25) %>% #Moles in spike of 250 uL (0.250 mL)
  mutate(recoveries = mol_conc/mol_ori_conc *100)

rs2_recoveries_summary <- rs2_recoveries %>% #Make a new data table with the averages and stdevs of the recoveries for each treatment
  group_by(quant_method, treatment, treatment2, comp_method, soil_type) %>%
  summarise_at(vars("recoveries"), list(mean = mean, sd = sd), na.rm = TRUE)


#Export a table of the recoveries
write.csv(rs2_recoveries_summary,'results/MethodComp_LDOPA_MUBph_recoveries_table.csv')
write.csv(rs2_recoveries_summary,'results/MethodComp_MEOH_MUBph_recoveries_table.csv')

#Barplot of the recoveries
#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")

#Set order of variables
rs2_recoveries_summary$treatment3 <- factor(rs2_recoveries_summary$treatment, levels = c("CF_MB", "CF_kc", "CF_mc", "CF_fo", "CF_gs", "CF_rc",
                                                              "SO_MB", "SO_kc", "SO_mc", "SO_fo", "SO_gs", "SO_rc",
                                                              "SH_MB", "SH_kc", "SH_mc", "SH_fo", "SH_gs", "SH_rc"))

#Set order of variables
rs2_recoveries_summary$treatment4 <- factor(rs2_recoveries_summary$treatment2, levels = c("MB_CF", "MB_SO", "MB_SH",
                                                            "kc_CF", "kc_SO", "kc_SH",
                                                            "mc_CF", "mc_SO", "mc_SH",
                                                            "fo_CF", "fo_SO", "fo_SH",
                                                            "gs_CF", "gs_SO", "gs_SH",
                                                            "rc_CF", "rc_SO", "rc_SH"))
#Plot using facet_wrap (Plots for all quant_methods)
all_recoveries_barplots <- ggplot(data = rs2_recoveries_summary, aes(x = treatment4, y = mean, fill = comp_method)) + 
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
ggsave("figures/Barplot_Allrecoveriesmethods_MethodComp.svg", all_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_Allrecoveriesmethods_MethodComp.pdf", all_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_Allrecoveriesmethods_MEOH_MethodComp.svg", all_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_Allrecoveriesmethods_MEOH_MethodComp.pdf", all_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#Recovery Bar Plots for MS quant_methods
rs2_recoveries_summary_ms <- rs2_recoveries_summary %>% 
  #filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  #"LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

ms_recoveries_barplots <- ggplot(data = rs2_recoveries_summary_ms, aes(x = treatment4, y = mean, fill = comp_method)) + 
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
ggsave("figures/Barplot_MSrecoveriesmethods_MethodComp.svg", ms_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_MSrecoveriesmethods_MethodComp.pdf", ms_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_MSrecoveriesmethods_MEOH_MethodComp.svg", ms_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_MSrecoveriesmethods_MEOH_MethodComp.pdf", ms_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)






#Bar Plots for nm quant_methods
rs2_recoveries_summary_nm <- rs2_recoveries_summary %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")


nm_recoveries_barplots <- ggplot(data = rs2_recoveries_summary_nm, aes(x = treatment4, y = mean, fill = comp_method)) + 
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
ggsave("figures/Barplot_NMrecoveriesmethods_MethodComp.svg", nm_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_NMrecoveriesmethods_MethodComp.pdf", nm_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#=============== Partition %Losses ================================================================================
#===#Calculate %Total Loss
rs2_loss <- rs2_recoveries %>% #For treatments and time points
  select("quant_method", "filename", "rs", "comp_method", "soil_type", "treatment", "treatment2", "env_rep", "tech_rep", "recoveries") %>%
  mutate(loss_total = 100 - recoveries)


#===#Calculate %Extraction Loss
rs2_loss_extr_A <- rs2_loss %>%
  filter(soil_type == "MB") %>%
  group_by(quant_method, treatment, comp_method, soil_type) %>%
  summarise_at(vars("loss_total"), list(mean = mean, sd = sd), na.rm = TRUE) %>%
  mutate(mean = ifelse(mean < 0, 0, mean)) %>%
  rename(A_loss_mean = mean,
         A_loss_sd = sd) 

rs2_loss_extr <- rs2_loss %>%
  right_join(rs2_loss_extr_A, by = c("quant_method", "comp_method"))  %>%  #Add in Extraction loss calculated from group A
  select("quant_method", "filename", "rs", "treatment.x", "comp_method", "treatment2", "soil_type.x", "env_rep", "tech_rep", 
         "recoveries", "loss_total", "A_loss_mean") %>%
  rename(treatment = treatment.x,
         soil_type = soil_type.x,
         loss_extr = A_loss_mean)
  
  
#===#Calculate %Adsorption Loss based on linear regression using groups B and C
rs2_loss_ads_BC <- rs2_loss_extr %>%
  filter(soil_type == "kc" | soil_type == "mc") %>%
  right_join(rs_types, by = "soil_type") %>%
  filter(soil_series != "Grenville" ) %>%
  filter(soil_series != "Rideau" ) %>%
  filter(soil_series != "Granby" ) %>%
  mutate(loss_abs_BC = loss_total - loss_extr) %>%
  mutate(loss_abs_BC_2 = ifelse(loss_abs_BC < 0, 0, loss_abs_BC))

#Look for linear correlations (obvious in this case b/c only 2 point curve)

ggscatter(rs2_loss_ads_BC, x = "cec", y = "loss_abs_BC_2",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)

ggscatter(rs2_loss_ads_BC, x = "specific_sa", y = "loss_abs_BC_2",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)

#Linear regression for each quant method and comp_method for CEC
fit_cec <- rs2_loss_ads_BC %>% 
  group_by(quant_method) %>%
  nest %>% 
  mutate(fit = map(data, ~ lm(loss_abs_BC_2 ~ cec, data = .)),
         parameters = map(fit, tidy), #provides estimate table for slope and y-intercept with std.error and p-values
         summary = map(fit, broom::glance), #provides R2, adj.R2, etc.
         predictions = map(fit, augment)) %>% #provides fitted values with residuals and errors
  unnest(parameters) %>%
  pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
  unnest(summary) %>%
  #unnest(predictions) 
  select("quant_method", "adj.r.squared","AIC", "estimate_(Intercept)", "estimate_cec" ) %>%
  rename(cec_adj.r2 = adj.r.squared, 
         cec_AIC = AIC,
         cec_yint = "estimate_(Intercept)",
         cec_slope = estimate_cec)


#Linear regression for each quant method and comp_method for specific_SA
fit_sa <- rs2_loss_ads_BC %>% 
  group_by(quant_method) %>%
  nest %>% 
  mutate(fit = map(data, ~ lm(loss_abs_BC_2 ~ specific_sa, data = .)),
         parameters = map(fit, tidy), #provides estimate table for slope and y-intercept with std.error and p-values
         summary = map(fit, broom::glance), #provides R2, adj.R2, etc.
         predictions = map(fit, augment)) %>% #provides fitted values with residuals and errors
  unnest(parameters) %>%
  pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
  unnest(summary) %>%
  #unnest(predictions)
  select("quant_method", "adj.r.squared","AIC", "estimate_(Intercept)", "estimate_specific_sa" ) %>%
  rename(sa_adj.r2 = adj.r.squared, 
         sa_AIC = AIC,
         sa_yint = "estimate_(Intercept)",
         sa_slope = estimate_specific_sa)

#Merge regression information into main dataset
rs2_loss_extr_abs <- rs2_loss_extr %>%
  right_join(fit_cec, by = c("quant_method")) %>%
  right_join(fit_sa, by = c("quant_method")) %>%
  full_join(rs_types, by = "soil_type") %>%
  mutate(loss_abs_cec = cec_slope*cec + cec_yint) %>%
  mutate(loss_abs_sa = sa_slope*specific_sa + sa_yint) %>%
  rename(treatment = treatment.x) %>%
  select("quant_method", "filename", "rs", "treatment", "treatment2", "comp_method", "soil_type", "env_rep", "tech_rep", 
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
  group_by(quant_method, treatment, treatment2, comp_method, soil_type) %>%
  summarise_at(vars("recoveries", "loss_total", "loss_extr", "loss_abs_cec"), 
               list(mean = mean), na.rm = TRUE) %>%
  mutate(loss_bio = loss_total_mean - loss_extr_mean - loss_abs_cec_mean) %>%
  mutate(loss_bio2 = ifelse(loss_bio  < 0, 0, loss_bio )) %>%
  mutate(loss_bio3 = ifelse(soil_type == "MB", 0, loss_bio2 )) %>%
  mutate(loss_bio4 = ifelse(soil_type == "kc", 0, loss_bio3 )) %>%
  mutate(loss_bio5 = ifelse(soil_type == "mc", 0, loss_bio4 )) %>%
  select("quant_method", "treatment", "treatment2", "comp_method", "soil_type",  
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
  pivot_longer(-c(quant_method, treatment, treatment2, comp_method, soil_type), names_to = "conditions", values_to = "percentage")

#Set order of variables
rs2_all_loss$treatment3 <- factor(rs2_all_loss$treatment, levels = c("CF_MB", "CF_kc", "CF_mc", "CF_fo", "CF_gs", "CF_rc",
                                                                      "SO_MB", "SO_kc", "SO_mc", "SO_fo", "SO_gs", "SO_rc",
                                                                      "SH_MB", "SH_kc", "SH_mc", "SH_fo", "SH_gs", "SH_rc"))

#Set order of variables
rs2_all_loss$treatment4 <- factor(rs2_all_loss$treatment2, levels = c("MB_CF", "MB_SO", "MB_SH",
                                                                                          "kc_CF", "kc_SO", "kc_SH",
                                                                                          "mc_CF", "mc_SO", "mc_SH",
                                                                                          "fo_CF", "fo_SO", "fo_SH",
                                                                                          "gs_CF", "gs_SO", "gs_SH",
                                                                                          "rc_CF", "rc_SO", "rc_SH"))

barplot_losses <- ggplot(data = rs2_all_loss, aes(x = treatment4, y = percentage, 
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
ggsave("figures/BarplotStack_LossPartition_MethodComp.svg", barplot_losses, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotStack_LossPartition_MethodComp.pdf", barplot_losses, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/BarplotFill_LossPartition_MEOH_MethodComp.svg", barplot_losses, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotFill_LossPartition_MEOH_MethodComp.pdf", barplot_losses, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)




#Loss Partition Box Plots for MS quant_methods
rs2_all_loss_mz <- rs2_all_loss %>% 
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

barplot_losses_mz <- ggplot(data = rs2_all_loss_mz, aes(x = treatment4, y = percentage, 
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
ggsave("figures/BarplotStack_LossPartitionMZ_MethodComp.svg", barplot_losses_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotStack_LossPartitionMZ_MethodComp.pdf", barplot_losses_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/BarplotFill_LossPartitionMZ_MEOH_MethodComp.svg", barplot_losses_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotFill_LossPartitionMZ_MEOH_MethodComp.pdf", barplot_losses_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)





#Bar Plots for nm quant_methods
rs2_all_loss_nm <- rs2_all_loss %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

barplot_losses_nm <- ggplot(data = rs2_all_loss_nm, aes(x = treatment4, y = percentage, 
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
ggsave("figures/BarplotStack_LossPartitionNM_MethodComp.svg", barplot_losses_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotStack_LossPartitionNM_MethodComp.pdf", barplot_losses_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)





























#=============== Bar plots of RS Quant data grouped by TECHNICAL REPLICATES ==================================================================================================
rs2_bar <- rs2 %>% 
  filter(treatment.x != "QC") %>%
  filter(treatment.x != "ori") %>%
  unite("treatment_tech", c("treatment.x", "tech_rep"), remove = FALSE, sep="_") %>%
  group_by(quant_method, comp_method, soil_type, treatment.x, treatment_tech, tech_rep) %>%
  summarise_at(vars("adj_conc"), list(mean = mean, sd = sd), na.rm = TRUE)

#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#57d0bc", "#D55E00", "#131181","#a01a1a",
                "#d0dc87", "#999999", "#dcb087", "#8962ab", "#702064", "#ab627a", "#1a6d41", "#e3e6e5", "#eed09b")

#Set order of variables
rs2_bar$treatment2 <- factor(rs2_bar$treatment.x, levels = c("CF_MB_1", "CF_kc_1", "CF_mc_1", "CF_fo_1", "CF_gs_1", "CF_rc_1",
                                                             "CF_MB_2", "CF_kc_2", "CF_mc_2", "CF_fo_2", "CF_gs_2", "CF_rc_2",
                                                             "CF_MB_3", "CF_kc_3", "CF_mc_3", "CF_fo_3", "CF_gs_3", "CF_rc_3",
                                                             "SO_MB_1", "SO_kc_1", "SO_mc_1", "SO_fo_1", "SO_gs_1", "SO_rc_1",
                                                             "SO_MB_2", "SO_kc_2", "SO_mc_2", "SO_fo_2", "SO_gs_2", "SO_rc_2",
                                                             "SO_MB_3", "SO_kc_3", "SO_mc_3", "SO_fo_3", "SO_gs_3", "SO_rc_3",
                                                             "SH_MB_1", "SH_kc_1", "SH_mc_1", "SH_fo_1", "SH_gs_1", "SH_rc_1",
                                                             "SH_MB_2", "SH_kc_2", "SH_mc_2", "SH_fo_2", "SH_gs_2", "SH_rc_2",
                                                             "SH_MB_3", "SH_kc_3", "SH_mc_3", "SH_fo_3", "SH_gs_3", "SH_rc_3"))

#Plot using facet_wrap (Plots for all quant_methods)
all_quant_barplots <- ggplot(data = rs2_bar,aes(x = treatment_tech, y = mean, fill = treatment.x)) + 
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
  ylab("Concentration (mg/mL)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method, scales = "free_y")

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_Allquantmethods_Techreps_MethodComp.svg", all_quant_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_Allquantmethods_Techreps_MethodComp.pdf", all_quant_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_Allquantmethods_Techreps_MEOH_MethodComp.svg", all_quant_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_Allquantmethods_Techreps_MEOH_MethodComp.pdf", all_quant_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)




#Bar Plots for MS quant_methods
rs2_bar_ms <- rs2_bar %>% 
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

ms_quant_barplots <- ggplot(data = rs2_bar_ms,aes(x = treatment_tech, y = mean, fill = treatment.x)) + 
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
  ylab("Concentration (mg/mL)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_MSquantmethods_Techreps_MethodComp.svg", ms_quant_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_MSquantmethods_Techreps_MethodComp.pdf", ms_quant_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)







#Box Plots for nm quant_methods
rs2_bar_nm <- rs2_bar %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")


nm_quant_barplots <- ggplot(data = rs2_bar_nm,aes(x = treatment_tech, y = mean, fill = treatment.x)) + 
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
  ylab("Concentration (mg/mL)") + 
  xlab("Treatments") +
  facet_grid(~ quant_method , scales = "free_y", labeller = as_labeller(method.labs))

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Barplot_NMquantmethods_Techreps_MethodComp.svg", nm_quant_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_NMquantmethods_Techreps_MethodComp.pdf", nm_quant_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#=============== Calculate the recoveries for TECHNICAL REPLICATES and REDUCED ================================================================================
#Using the cooncentrations from the original spike mixes summarize the original concentrations
rs2_ori <- rs2 %>%
  filter(treatment.x == "ori") %>%
  group_by(quant_method, treatment.x, comp_method) %>%
  summarise_at(vars("adj_conc"), list(mean = mean, sd = sd), na.rm = TRUE) %>%
  rename(ori_mean_conc = mean,
         ori_sd_conc = sd)


rs2_recoveries_techreps <- rs2 %>% 
  filter(treatment.x != "ori") %>%
  filter(treatment.x != "QC") %>% 
  right_join(rs2_ori, by = "quant_method")  %>%  #Add in ori data
  select("quant_method", "filename", "rs", "treatment.x.x", "comp_method.x", "soil_type", "env_rep", "tech_rep", "adj_conc", "ori_mean_conc", "ori_sd_conc") %>%
  rename(treatment = treatment.x.x,
         comp_method = comp_method.x,
         ori_adj_conc_mean = ori_mean_conc,
         ori_adj_sd_mean = ori_sd_conc) %>%
  unite("treatment2", c("soil_type", "comp_method"), remove = FALSE) %>%
  unite("treatment_tech", c("treatment2", "tech_rep"), remove = FALSE) %>%
  mutate(mol_conc = adj_conc * 0.5) %>% #Moles in 500 uL (0.5 mL)
  mutate(mol_ori_conc = ori_adj_conc_mean * 0.25) %>% #Moles in spike of 250 uL (0.250 mL)
  mutate(recoveries = mol_conc/mol_ori_conc *100)

rs2_recoveries_summary_techreps <- rs2_recoveries_techreps %>% #Make a new data table with the averages and stdevs of the recoveries for each treatment
  group_by(quant_method, comp_method, soil_type, treatment2, treatment, treatment_tech, tech_rep) %>%
  summarise_at(vars("recoveries"), list(mean = mean, sd = sd), na.rm = TRUE) 

rs2_recoveries <- rs2 %>% 
  filter(treatment.x != "ori") %>%
  filter(treatment.x != "QC") %>% 
  right_join(rs2_ori, by = "quant_method")  %>%  #Add in ori data
  filter(tech_rep == 1) %>%
  select("quant_method", "filename", "rs", "treatment.x.x", "comp_method.x", "soil_type", "env_rep", "tech_rep", "adj_conc", "ori_mean_conc", "ori_sd_conc") %>%
  rename(treatment = treatment.x.x,
         comp_method = comp_method.x,
         ori_adj_conc_mean = ori_mean_conc,
         ori_adj_sd_mean = ori_sd_conc) %>%
  unite("treatment2", c("soil_type", "comp_method"), remove = FALSE) %>%
  mutate(mol_conc = adj_conc * 0.5) %>% #Moles in 500 uL (0.5 mL)
  mutate(mol_ori_conc = ori_adj_conc_mean * 0.25) %>% #Moles in spike of 250 uL (0.250 mL)
  mutate(recoveries = mol_conc/mol_ori_conc *100)

rs2_recoveries_summary <- rs2_recoveries %>% #Make a new data table with the averages and stdevs of the recoveries for each treatment
  group_by(quant_method, comp_method, soil_type, treatment, treatment2) %>%
  summarise_at(vars("recoveries"), list(mean = mean, sd = sd), na.rm = TRUE)


#Export a table of the recoveries
write.csv(rs2_recoveries_summary,'results/MethodComp_LDOPA_MUBph_REDUCED_recoveries_table.csv')




#Barplot of the technical replicate recoveries
#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#57d0bc", "#D55E00", "#131181","#a01a1a",
                "#d0dc87", "#999999", "#dcb087", "#8962ab", "#702064", "#ab627a", "#1a6d41", "#e3e6e5", "#eed09b")


#Plot using facet_wrap (Plots for all quant_methods)
all_recoveries_barplots <- ggplot(data = rs2_recoveries_summary_techreps, aes(x = treatment_tech, y = mean, fill = treatment2)) + 
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
ggsave("figures/Barplot_Allrecoveriesmethods_Techreps_MethodComp.svg", all_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_Allrecoveriesmethods_Techreps_MethodComp.pdf", all_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)




#Recovery Bar Plots for MS quant_methods
rs2_recoveries_summary_techreps_ms <- rs2_recoveries_summary_techreps %>% 
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

ms_recoveries_barplots <- ggplot(data = rs2_recoveries_summary_techreps_ms, aes(x = treatment_tech, y = mean, fill = treatment2)) + 
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
ggsave("figures/Barplot_MSrecoveriesmethods_Techreps_MethodComp.svg", ms_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_MSrecoveriesmethods_Techreps_MethodComp.pdf", ms_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)







#Bar Plots for nm quant_methods
rs2_recoveries_summary_techreps_nm <- rs2_recoveries_summary_techreps %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")


nm_recoveries_barplots <- ggplot(data = rs2_recoveries_summary_techreps_nm, aes(x = treatment_tech, y = mean, fill = treatment2)) + 
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
ggsave("figures/Barplot_NMrecoveriesmethods_Techreps_MethodComp.svg", nm_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_NMrecoveriesmethods_Techreps_MethodComp.pdf", nm_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)




#Barplot of the REDUCED recoveries
#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")

#Set order of variables
rs2_recoveries_summary$treatment3 <- factor(rs2_recoveries_summary$treatment2, levels = c("MB_CF", "MB_SO", "MB_SH",
                                                                      "kc_CF", "kc_SO", "kc_SH",
                                                                      "mc_CF", "mc_SO", "mc_SH",
                                                                      "fo_CF", "fo_SO", "fo_SH",
                                                                      "gs_CF", "gs_SO", "gs_SH",
                                                                      "rc_CF", "rc_SO", "rc_SH"))


#Plot using facet_wrap (Plots for all quant_methods)
all_recoveries_barplots <- ggplot(data = rs2_recoveries_summary, aes(x = treatment3, y = mean, fill = comp_method)) + 
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
ggsave("figures/Barplot_AllrecoveriesmethodsREDUCED_MethodComp.svg", all_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_AllrecoveriesmethodsREDUCED_MethodComp.pdf", all_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)




#Recovery Bar Plots for MS quant_methods
rs2_recoveries_summary_ms <- rs2_recoveries_summary %>% 
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

ms_recoveries_barplots <- ggplot(data = rs2_recoveries_summary_ms, aes(x = treatment3, y = mean, fill = comp_method)) + 
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
ggsave("figures/Barplot_MSrecoveriesmethodsREDUCED_MethodComp.svg", ms_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_MSrecoveriesmethodsREDUCED_MethodComp.pdf", ms_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)







#Bar Plots for nm quant_methods
rs2_recoveries_summary_nm <- rs2_recoveries_summary %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")


nm_recoveries_barplots <- ggplot(data = rs2_recoveries_summary_nm, aes(x = treatment3, y = mean, fill = comp_method)) + 
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
ggsave("figures/Barplot_NMrecoveriesmethodsREDUCED_MethodComp.svg", nm_recoveries_barplots, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Barplot_NMrecoveriesmethodsREDUCED_MethodComp.pdf", nm_recoveries_barplots, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)




#=============== Partition %Losses for REDUCED ================================================================================
#===#Calculate %Total Loss
rs2_loss <- rs2_recoveries %>% #For treatments and time points
  select("quant_method", "filename", "rs", "comp_method", "soil_type", "treatment", "treatment2", "env_rep", "tech_rep", "recoveries") %>%
  mutate(loss_total = 100 - recoveries)


#===#Calculate %Extraction Loss
rs2_loss_extr_A <- rs2_loss %>%
  filter(soil_type == "MB") %>%
  group_by(quant_method, treatment, comp_method, soil_type) %>%
  summarise_at(vars("loss_total"), list(mean = mean, sd = sd), na.rm = TRUE) %>%
  mutate(mean = ifelse(mean < 0, 0, mean)) %>%
  rename(A_loss_mean = mean,
         A_loss_sd = sd) 

rs2_loss_extr <- rs2_loss %>%
  right_join(rs2_loss_extr_A, by = c("quant_method", "comp_method"))  %>%  #Add in Extraction loss calculated from group A
  select("quant_method", "filename", "rs", "treatment.x","treatment2", "comp_method", "soil_type.x", "env_rep", "tech_rep", 
         "recoveries", "loss_total", "A_loss_mean") %>%
  rename(treatment = treatment.x,
         soil_type = soil_type.x,
         loss_extr = A_loss_mean)


#===#Calculate %Adsorption Loss based on linear regression using groups B and C
rs2_loss_ads_BC <- rs2_loss_extr %>%
  filter(soil_type == "kc" | soil_type == "mc") %>%
  right_join(rs_types, by = "soil_type") %>%
  filter(soil_series != "Grenville" ) %>%
  filter(soil_series != "Rideau" ) %>%
  filter(soil_series != "Granby" ) %>%
  mutate(loss_abs_BC = loss_total - loss_extr) %>%
  mutate(loss_abs_BC_2 = ifelse(loss_abs_BC < 0, 0, loss_abs_BC))

#Look for linear correlations (obvious in this case b/c only 2 point curve)

ggscatter(rs2_loss_ads_BC, x = "cec", y = "loss_abs_BC_2",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)

ggscatter(rs2_loss_ads_BC, x = "specific_sa", y = "loss_abs_BC_2",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)

#Linear regression for each quant method and comp_method for CEC
fit_cec <- rs2_loss_ads_BC %>% 
  group_by(quant_method) %>%
  nest %>% 
  mutate(fit = map(data, ~ lm(loss_abs_BC_2 ~ cec, data = .)),
         parameters = map(fit, tidy), #provides estimate table for slope and y-intercept with std.error and p-values
         summary = map(fit, broom::glance), #provides R2, adj.R2, etc.
         predictions = map(fit, augment)) %>% #provides fitted values with residuals and errors
  unnest(parameters) %>%
  pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
  unnest(summary) %>%
  #unnest(predictions) 
  select("quant_method", "adj.r.squared","AIC", "estimate_(Intercept)", "estimate_cec" ) %>%
  rename(cec_adj.r2 = adj.r.squared, 
         cec_AIC = AIC,
         cec_yint = "estimate_(Intercept)",
         cec_slope = estimate_cec)


#Linear regression for each quant method and comp_method for specific_SA
fit_sa <- rs2_loss_ads_BC %>% 
  group_by(quant_method) %>%
  nest %>% 
  mutate(fit = map(data, ~ lm(loss_abs_BC_2 ~ specific_sa, data = .)),
         parameters = map(fit, tidy), #provides estimate table for slope and y-intercept with std.error and p-values
         summary = map(fit, broom::glance), #provides R2, adj.R2, etc.
         predictions = map(fit, augment)) %>% #provides fitted values with residuals and errors
  unnest(parameters) %>%
  pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
  unnest(summary) %>%
  #unnest(predictions)
  select("quant_method", "adj.r.squared","AIC", "estimate_(Intercept)", "estimate_specific_sa" ) %>%
  rename(sa_adj.r2 = adj.r.squared, 
         sa_AIC = AIC,
         sa_yint = "estimate_(Intercept)",
         sa_slope = estimate_specific_sa)

#Merge regression information into main dataset
rs2_loss_extr_abs <- rs2_loss_extr %>%
  right_join(fit_cec, by = c("quant_method")) %>%
  right_join(fit_sa, by = c("quant_method")) %>%
  full_join(rs_types, by = "soil_type") %>%
  rename(treatment = treatment.x) %>%
  mutate(loss_abs_cec = cec_slope*cec + cec_yint) %>%
  mutate(loss_abs_sa = sa_slope*specific_sa + sa_yint) %>%
  select("quant_method", "filename", "rs", "treatment", "treatment2", "comp_method", "soil_type", "env_rep", "tech_rep", 
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
  group_by(quant_method, treatment, treatment2, comp_method, soil_type) %>%
  summarise_at(vars("recoveries", "loss_total", "loss_extr", "loss_abs_cec"), 
               list(mean = mean), na.rm = TRUE) %>%
  mutate(loss_bio = loss_total_mean - loss_extr_mean - loss_abs_cec_mean) %>%
  mutate(loss_bio2 = ifelse(loss_bio  < 0, 0, loss_bio )) %>%
  mutate(loss_bio3 = ifelse(soil_type == "MB", 0, loss_bio2 )) %>%
  mutate(loss_bio4 = ifelse(soil_type == "kc", 0, loss_bio3 )) %>%
  mutate(loss_bio5 = ifelse(soil_type == "mc", 0, loss_bio4 )) %>%
  select("quant_method", "treatment", "comp_method", "soil_type", "treatment2", 
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
  pivot_longer(-c(quant_method, treatment, treatment2, comp_method, soil_type), names_to = "conditions", values_to = "percentage")

#Set order of variables
rs2_all_loss$treatment3 <- factor(rs2_all_loss$treatment, levels = c("CF_MB", "CF_kc", "CF_mc", "CF_fo", "CF_gs", "CF_rc",
                                                                     "SO_MB", "SO_kc", "SO_mc", "SO_fo", "SO_gs", "SO_rc",
                                                                     "SH_MB", "SH_kc", "SH_mc", "SH_fo", "SH_gs", "SH_rc"))

#Set order of variables
rs2_all_loss$treatment4 <- factor(rs2_all_loss$treatment2, levels = c("MB_CF", "MB_SO", "MB_SH",
                                                                      "kc_CF", "kc_SO", "kc_SH",
                                                                      "mc_CF", "mc_SO", "mc_SH",
                                                                      "fo_CF", "fo_SO", "fo_SH",
                                                                      "gs_CF", "gs_SO", "gs_SH",
                                                                      "rc_CF", "rc_SO", "rc_SH"))


barplot_losses <- ggplot(data = rs2_all_loss, aes(x = treatment4, y = percentage, 
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
ggsave("figures/BarplotFill_LossPartitionREDUCED_MethodComp.svg", barplot_losses, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotFill_LossPartitionREDUCED_MethodComp.pdf", barplot_losses, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Loss Partition Box Plots for MS quant_methods
rs2_all_loss_mz <- rs2_all_loss %>% 
  filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  "LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

barplot_losses_mz <- ggplot(data = rs2_all_loss_mz, aes(x = treatment4, y = percentage, 
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
ggsave("figures/BarplotFill_LossPartitionMZREDUCED_MethodComp.svg", barplot_losses_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotFill_LossPartitionMZREDUCED_MethodComp.pdf", barplot_losses_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Bar Plots for nm quant_methods
rs2_all_loss_nm <- rs2_all_loss %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

barplot_losses_nm <- ggplot(data = rs2_all_loss_nm, aes(x = treatment4, y = percentage, 
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
ggsave("figures/BarplotFill_LossPartitionNMREDUCED_MethodComp.svg", barplot_losses_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/BarplotFill_LossPartitionNMREDUCED_MethodComp.pdf", barplot_losses_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)









#=============== ANOVAs and Kruskal-Wallis ==================================================================================================
#ANOVA = Analysis of variance: requires a continuous dependent variable (Y), and one independent categorical variable (X)
#===#Generate dataframes for linear regressions (only for ms methods b/c most senstive)

rs2_anovas <- rs2_recoveries %>%
  select("quant_method", "rs", "treatment", "treatment2" ,"comp_method","soil_type", "env_rep", "tech_rep", "recoveries")

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

ggscatter(rs2_anovas, x = "treatment", y = "recoveries",  #Looking for linear correlations
          facet.by  = c("quant_method"), 
          short.panel.labs = FALSE) +
  stat_smooth(method = "loess", span = 0.9)


#===#Performing Linear regressions

#Linear regressions on all groups
fit_basic <- rs2_anovas %>% 
  group_by(quant_method) %>% 
  nest %>% 
  mutate(fit = map(data, ~ lm(recoveries ~ treatment, data = .)),
         parameters = map(fit, tidy), #provides estimate table for slope and y-intercept with std.error and p-values
         summary = map(fit, broom::glance), #provides R2, adj.R2, etc.
         predictions = map(fit, augment)) 

fit_basic_table <- fit_basic %>% #provides fitted values with residuals and errors
  unnest(parameters) %>%
  pivot_wider(names_from = term, values_from = c(estimate, std.error, statistic, p.value)) %>%
  unnest(summary) %>%
  #unnest(predictions) 
  select(-c("predictions", "fit", "data"))

#Pull out the seperate models
fit_basic_LDOPAmz <- fit_basic[[1, "fit"]]
fit_basic_LDOPAmz <- fit_basic_LDOPAmz[[1]]

fit_basic_LDOPAnm <- fit_basic[[2, "fit"]]
fit_basic_LDOPAnm <- fit_basic_LDOPAnm[[1]]

fit_basic_MUBphmz <- fit_basic[[3, "fit"]]
fit_basic_MUBphmz <- fit_basic_MUBphmz[[1]]

fit_basic_MUBphnm <- fit_basic[[4, "fit"]]
fit_basic_MUBphnm <- fit_basic_MUBphnm[[1]]


#Export Regression Table
write.csv(fit_basic_table,'results/MethodComp_Reg_Treatment_table.csv')
write.csv(fit_basic_table,'results/MethodComp_MEOH_Reg_Treatment_table.csv')
write.csv(fit_basic_table,'results/MethodComp_RegREDUCED_Treatment_table.csv') #Use if boxcox transformation applied


#===#Test Assumptions are valid for ANOVA by:
###Select the model
o <- fit_basic_MUBphmz
      #Add in model being tested #fit_basic_DCIPmz, fit_basic_DCIPnm, fit_basic_LDOPAmz, fit_basic_LDOPAnm, fit_basic_MUBphmz, fit_basic_MUBphnm

#### Inspect the model diagnostic metrics
model.metrics <- augment(o) %>%   #Change the name of the regression
  select(-.hat, -.sigma, -.fitted, -.se.fit) 
head(model.metrics)

####1. Assess assumptions for ANOVA; The residuals must be homoscedastic
ncvTest(o) #NCV test for heteroscedascitity where p-value < 0.5 indicates heteroscedascity

bartlett.test(.resid ~ treatment, data = model.metrics)  #Barlett test for heteroscedascitity where p-value < 0.5 indicates heteroscedascity

leveneTest(.resid ~ treatment, data = model.metrics)  #Levene Test for heteroscedascity where p-value < 0.5 indicates heteroscedastic

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
  anova_test(recoveries ~ treatment)

write.csv(rs2_anova_results,'results/MethodComp_ANOVA_RecoveriesVsTreatment_table.csv')


#Run Kruskal Wallis if data is heteroscedastic and/or non-normal

rs2_kw_results <- rs2_anovas  %>%
  group_by(quant_method) %>%
  kruskal_test(recoveries ~ treatment) #Can't do interaction terms in Kruskal Wallis so use merged factor

write.csv(rs2_kw_results,'results/MethodComp_KW_RecoveriesVsTreatment_table.csv')
write.csv(rs2_kw_results,'results/MethodComp_MEOH_KW_RecoveriesVsTreatment_table.csv')

#===#Pairwise Comparisons
#####Effect of treatment on each sample using tukey hsd (must be homoscedastic and normal)
pwc <- rs2_anovas %>% 
  group_by(quant_method) %>%
  tukey_hsd(recoveries ~ treatment) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

write.csv(pwc ,'results/MethodComp_TUKEYHSD_RecoveriesVsTreatment_table.csv')

#Run Wilcoxon pairwise comparison if data is heteroscedastic and/or non-normal
my_comparisons <- list(c("CF_MB","SO_MB"), c("CF_MB","SH_MB"),c("SO_MB","SH_MB"),
                       c("CF_kc","SO_kc"), c("CF_kc","SH_kc"),c("SO_kc","SH_kc"),
                       c("CF_mc","SO_mc"), c("CF_mc","SH_mc"),c("SO_mc","SH_mc"),
                       c("CF_fo","SO_fo"), c("CF_fo","SH_fo"),c("SO_fo","SH_fo"),
                       c("CF_gs","SO_gs"), c("CF_gs","SH_gs"),c("SO_gs","SH_gs"),
                       c("CF_rc","SO_rc"), c("CF_rc","SH_rc"),c("SO_rc","SH_rc"))


pkw <- rs2_anovas %>% 
  group_by(quant_method) %>%
  wilcox_test(data =., recoveries ~ treatment, 
              comparisons = my_comparisons, 
              p.adjust.method = "BH") 

write.csv(pkw ,'results/MethodComp_WILCOX_RecoveriesVsTreatment_table.csv')
write.csv(pkw ,'results/MethodComp_MEOH_WILCOX_RecoveriesVsTreatment_table.csv')


#=============== Boxplots ==================================================================================================
#===#Generate dataframes for linear regressions (only for ms methods b/c most senstive)

rs2_boxplots <- rs2_recoveries %>%
  select("quant_method", "rs", "treatment", "treatment2", "comp_method", "soil_type", "env_rep", "tech_rep", "recoveries")

rs2_boxplots$treatment2 <-as.factor(rs2_boxplots$treatment2)

# Data transformations if the data is heteroscedastic and/or the residuals are non-normal than must transform the y-variable and/or x-variable. 
# Below different methods for trasnforming variables

p1 <- powerTransform(recoveries ~ treatment, data = rs2_boxplots) #BoxCox transormation
rs2_boxplots<- cbind(rs2_boxplots, y_boxcox=bcPower(rs2_boxplots$recoveries, p1$roundlam))

#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")

#Set order of variables
rs2_boxplots$treatment3 <- factor(rs2_boxplots$treatment2, levels = c("MB_CF", "MB_SO", "MB_SH",
                                                                                          "kc_CF", "kc_SO", "kc_SH",
                                                                                          "mc_CF", "mc_SO", "mc_SH",
                                                                                          "fo_CF", "fo_SO", "fo_SH",
                                                                                          "gs_CF", "gs_SO", "gs_SH",
                                                                                          "rc_CF", "rc_SO", "rc_SH"))

#Boxplot for all samples
boxplot_recoveries <- ggplot(data = rs2_boxplots, aes(x = treatment3, y = recoveries, color = soil_type)) + 
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
ggsave("figures/Boxplot_Allrecoveriesmethods_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_Allrecoveriesmethods_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_AllrecoveriesmethodsREDUCED_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_AllrecoveriesmethodsREDUCED_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_Allrecoveriesmethods_MEOH_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_Allrecoveriesmethods_MEOH_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Recovery Box Plots for MS quant_methods
rs2_boxplots_mz <- rs2_boxplots %>% 
  #filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  #"LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

boxplot_recoveries_mz <- ggplot(data = rs2_boxplots_mz, aes(x = treatment3, y = recoveries, color = soil_type)) + 
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
ggsave("figures/Boxplot_MSrecoveriesmethods_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSrecoveriesmethods_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_MSrecoveriesmethodsREDUCED_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSrecoveriesmethodsREDUCED_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_MSrecoveriesmethods_MEOH_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSrecoveriesmethods_MEOH_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Recovery Box Plots for nm quant_methods
rs2_boxplots_nm <- rs2_boxplots %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

boxplot_recoveries_nm <- ggplot(data = rs2_boxplots_nm, aes(x = treatment3, y = recoveries, color = soil_type)) + 
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
ggsave("figures/Boxplot_NMrecoveriesmethods_MethodComp.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMrecoveriesmethods_MethodComp.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_NMrecoveriesmethodsREDUCED_MethodComp.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMrecoveriesmethodsREDUCED_MethodComp.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)














#=============== Checking Technical Replicates and Boxplots ==================================================================================================
#===#Generate dataframes for linear regressions (only for ms methods b/c most senstive)
rs2_anovas_tech <- rs2_recoveries %>%
  select("quant_method", "rs", "treatment", "treatment2" ,"comp_method","soil_type", "env_rep", "tech_rep", "recoveries") %>%
  unite("meth_tech", c("comp_method", "tech_rep"), remove = FALSE) %>%
  unite("soil_tech", c("soil_type", "tech_rep"), remove = FALSE) %>%
  unite("treatment_tech", c("treatment2", "tech_rep"), remove = FALSE)

#Run Kruskal Wallis if data is heteroscedastic and/or non-normal
MethodComp_kw_TR_results <- rs2_anovas_tech  %>%
  group_by(quant_method) %>%
  kruskal_test(recoveries ~ tech_rep) #Can't do interaction terms in Kruskal Wallis so use merged factor

write.csv(MethodComp_kw_TR_results,'results/MethodComp_KW_RecoveriesVsTechRep_table.csv')
write.csv(MethodComp_kw_TR_results,'results/MethodComp_MEOH_KW_RecoveriesVsTechRep_table.csv')

MethodComp_kw_MT_results <- rs2_anovas_tech  %>%
  group_by(quant_method) %>%
  kruskal_test(recoveries ~ meth_tech) #Can't do interaction terms in Kruskal Wallis so use merged factor

write.csv(MethodComp_kw_MT_results,'results/MethodComp_KW_RecoveriesVsMethodTechRep_table.csv')
write.csv(MethodComp_kw_MT_results,'results/MethodComp_MEOH_KW_RecoveriesVsMethodTechRep_table.csv')

MethodComp_kw_ST_results <- rs2_anovas_tech  %>%
  group_by(quant_method) %>%
  kruskal_test(recoveries ~ soil_tech) #Can't do interaction terms in Kruskal Wallis so use merged factor

write.csv(MethodComp_kw_ST_results,'results/MethodComp_KW_RecoveriesVsSoilTechRep_table.csv')
write.csv(MethodComp_kw_ST_results,'results/MethodComp_MEOH_KW_RecoveriesVsSoilTechRep_table.csv')

MethodComp_kw_TT_results <- rs2_anovas_tech  %>%
  group_by(quant_method) %>%
  kruskal_test(recoveries ~ treatment_tech) #Can't do interaction terms in Kruskal Wallis so use merged factor

write.csv(MethodComp_kw_TT_results,'results/MethodComp_KW_RecoveriesVsTreatmentTechRep_table.csv')
write.csv(MethodComp_kw_TT_results,'results/MethodComp_MEOH_KW_RecoveriesVsTreatmentTechRep_table.csv')

#Run Wilcoxon pairwise comparison if data is heteroscedastic and/or non-normal
techrep_comparisons <- list(c("1","2"), c("1","3"),c("2","3"))

pkw_TR <- rs2_anovas_tech %>% 
  group_by(quant_method) %>%
  wilcox_test(data =., recoveries ~ tech_rep, 
              comparisons = techrep_comparisons, 
              p.adjust.method = "BH") 

write.csv(pkw_TR,'results/MethodComp_WILCOX_RecoveriesVsTechRep_table.csv')
write.csv(pkw_TR,'results/MethodComp_MEOH_WILCOX_RecoveriesVsTechRep_table.csv')



methtech_comparisons <- list(c("CF_1","CF_2"), c("CF_1","CF_3"),c("CF_2","CF_3"),
                             c("SH_1","SH_2"), c("SH_1","SH_3"),c("SH_2","SH_3"),
                             c("SO_1","SO_2"), c("SO_1","SO_3"),c("SO_2","SO_3"))

pkw_MT <- rs2_anovas_tech %>% 
  group_by(quant_method) %>%
  wilcox_test(data =., recoveries ~ meth_tech, 
              comparisons = methtech_comparisons, 
              p.adjust.method = "BH") 

write.csv(pkw_MT,'results/MethodComp_WILCOX_RecoveriesVsMethodTechRep_table.csv')
write.csv(pkw_MT,'results/MethodComp_MEOH_WILCOX_RecoveriesVsMethodTechRep_table.csv')




soiltech_comparisons <- list(c("mc_1","mc_2"), c("mc_1","mc_3"),c("mc_2","mc_3"),
                             c("kc_1","kc_2"), c("kc_1","kc_3"),c("kc_2","kc_3"),
                             c("gs_1","gs_2"), c("gs_1","gs_3"),c("gs_2","gs_3"),
                             c("fo_1","fo_2"), c("fo_1","fo_3"),c("fo_2","fo_3"),
                             c("rc_1","rc_2"), c("rc_1","rc_3"),c("rc_2","rc_3"),
                             c("MB_1","MB_2"), c("MB_1","MB_3"),c("MB_2","MB_3"))

pkw_ST <- rs2_anovas_tech %>% 
  group_by(quant_method) %>%
  wilcox_test(data =., recoveries ~ soil_tech, 
              comparisons = soiltech_comparisons, 
              p.adjust.method = "BH") 

write.csv(pkw_ST,'results/MethodComp_WILCOX_RecoveriesVsSoilTechRep_table.csv')
write.csv(pkw_ST,'results/MethodComp_MEOH_WILCOX_RecoveriesVsSoilTechRep_table.csv')




treatmenttech_comparisons <- list(c("fo_CF_1","fo_CF_2"), c("fo_CF_1","fo_CF_3"),c("fo_CF_2","fo_CF_3"),
                                  c("gs_CF_1","gs_CF_2"), c("gs_CF_1","gs_CF_3"),c("gs_CF_2","gs_CF_3"),
                                  c("rc_CF_1","rc_CF_2"), c("rc_CF_1","rc_CF_3"),c("rc_CF_2","rc_CF_3"),
                                  c("mc_CF_1","mc_CF_2"), c("mc_CF_1","mc_CF_3"),c("mc_CF_2","mc_CF_3"),
                                  c("kc_CF_1","kc_CF_2"), c("kc_CF_1","kc_CF_3"),c("kc_CF_2","kc_CF_3"),
                                  c("MB_CF_1","MB_CF_2"), c("MB_CF_1","MB_CF_3"),c("MB_CF_2","MB_CF_3"),
                                  c("fo_SO_1","fo_SO_2"), c("fo_SO_1","fo_SO_3"),c("fo_SO_2","fo_SO_3"),
                                  c("gs_SO_1","gs_SO_2"), c("gs_SO_1","gs_SO_3"),c("gs_SO_2","gs_SO_3"),
                                  c("rc_SO_1","rc_SO_2"), c("rc_SO_1","rc_SO_3"),c("rc_SO_2","rc_SO_3"),
                                  c("mc_SO_1","mc_SO_2"), c("mc_SO_1","mc_SO_3"),c("mc_SO_2","mc_SO_3"),
                                  c("kc_SO_1","kc_SO_2"), c("kc_SO_1","kc_SO_3"),c("kc_SO_2","kc_SO_3"),
                                  c("MB_SO_1","MB_SO_2"), c("MB_SO_1","MB_SO_3"),c("MB_SO_2","MB_SO_3"),
                                  c("fo_SH_1","fo_SH_2"), c("fo_SH_1","fo_SH_3"),c("fo_SH_2","fo_SH_3"),
                                  c("gs_SH_1","gs_SH_2"), c("gs_SH_1","gs_SH_3"),c("gs_SH_2","gs_SH_3"),
                                  c("rc_SH_1","rc_SH_2"), c("rc_SH_1","rc_SH_3"),c("rc_SH_2","rc_SH_3"),
                                  c("mc_SH_1","mc_SH_2"), c("mc_SH_1","mc_SH_3"),c("mc_SH_2","mc_SH_3"),
                                  c("kc_SH_1","kc_SH_2"), c("kc_SH_1","kc_SH_3"),c("kc_SH_2","kc_SH_3"),
                                  c("MB_SH_1","MB_SH_2"), c("MB_SH_1","MB_SH_3"),c("MB_SH_2","MB_SH_3"))
                                  
pkw_TT <- rs2_anovas_tech %>% 
  group_by(quant_method) %>%
  wilcox_test(data =., recoveries ~ treatment_tech, 
              comparisons = treatmenttech_comparisons, 
              p.adjust.method = "BH") 

write.csv(pkw_TT,'results/MethodComp_WILCOX_RecoveriesVsTreatmentTechRep_table.csv')
write.csv(pkw_TT,'results/MethodComp_MEOH_WILCOX_RecoveriesVsTreatmentTechRep_table.csv')

#=============== Boxplots for techrep comparisons ==================================================================================================
#Set the Color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999")

###===TECH_REPS BOXPLOT===###
#Set order of variables
rs2_anovas_tech$tech_rep2 <- factor(rs2_anovas_tech$tech_rep, levels = c("1", "2", "3"))

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
ggsave("figures/Boxplot_TechRepComp_TR_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_TR_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_TechRepComp_MEOH_TR_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_MEOH_TR_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)




#Recovery Box Plots for MS quant_methods
rs2_anovas_tech_mz <- rs2_anovas_tech %>% 
  #filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  #"LDOPA_198mz" = "LDOPA",
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
ggsave("figures/Boxplot_MSTechRepComp_TR_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_TR_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/Boxplot_MSTechRepComp_MEOH_TR_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_MEOH_TR_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#Recovery Box Plots for nm quant_methods
rs2_anovas_tech_nm <- rs2_anovas_tech %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
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
ggsave("figures/Boxplot_NMTechRepComp_TR_MethodComp.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMTechRepComp_TR_MethodComp.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)











###===METH_TECH BOXPLOT===###
#Boxplot for all tech replicates for all quant_methods
boxplot_recoveries <- ggplot(data = rs2_anovas_tech, aes(x = meth_tech, y = recoveries, color = comp_method)) + 
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
ggsave("figures/Boxplot_TechRepComp_MT_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_MT_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_TechRepComp_MEOH_MT_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_MEOH_MT_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#Recovery Box Plots for MS quant_methods
rs2_anovas_tech_mz <- rs2_anovas_tech %>% 
  #filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  #"LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

boxplot_recoveries_mz <- ggplot(data = rs2_anovas_tech_mz, aes(x = meth_tech, y = recoveries, color = comp_method)) + 
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
ggsave("figures/Boxplot_MSTechRepComp_MT_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_MT_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_MSTechRepComp_MEOH_MT_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_MEOH_MT_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Recovery Box Plots for nm quant_methods
rs2_anovas_tech_nm <- rs2_anovas_tech %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

boxplot_recoveries_nm <- ggplot(data = rs2_anovas_tech_nm, aes(x = meth_tech, y = recoveries, color = comp_method)) + 
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
ggsave("figures/Boxplot_NMTechRepComp_MT_MethodComp.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMTechRepComp_MT_MethodComp.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)













###===SOIL_TECH BOXPLOT===###
#Set order of variables
rs2_anovas_tech$soil_tech2 <- factor(rs2_anovas_tech$soil_tech, levels = c("MB_1", "MB_2", "MB_3",
                                                                         "kc_1", "kc_2", "kc_3",
                                                                         "mc_1", "mc_2", "mc_3",
                                                                         "fo_1", "fo_2", "fo_3",
                                                                         "gs_1", "gs_2", "gs_3",
                                                                         "rc_1", "rc_2", "rc_3"))

#Boxplot for all tech replicates for all quant_methods
boxplot_recoveries <- ggplot(data = rs2_anovas_tech, aes(x = soil_tech2, y = recoveries, color = soil_type)) + 
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
ggsave("figures/Boxplot_TechRepComp_ST_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_ST_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_TechRepComp_MEOH_ST_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_MEOH_ST_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#Recovery Box Plots for MS quant_methods
rs2_anovas_tech_mz <- rs2_anovas_tech %>% 
  #filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  #"LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

boxplot_recoveries_mz <- ggplot(data = rs2_anovas_tech_mz, aes(x = soil_tech2, y = recoveries, color = soil_type)) + 
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
ggsave("figures/Boxplot_MSTechRepComp_ST_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_ST_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_MSTechRepComp_MEOH_ST_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_MEOH_ST_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)


#Recovery Box Plots for nm quant_methods
rs2_anovas_tech_nm <- rs2_anovas_tech %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

boxplot_recoveries_nm <- ggplot(data = rs2_anovas_tech_nm, aes(x = soil_tech2, y = recoveries, color = soil_type)) + 
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
ggsave("figures/Boxplot_NMTechRepComp_ST_MethodComp.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMTechRepComp_ST_MethodComp.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)












###===TREATMENT_TECH BOXPLOT===###
#Set order of variables
rs2_anovas_tech$treatment_tech2 <- factor(rs2_anovas_tech$treatment_tech, levels = c("MB_CF_1", "MB_CF_2", "MB_CF_3",
                                                                                     "kc_CF_1", "kc_CF_2", "kc_CF_3",
                                                                                     "mc_CF_1", "mc_CF_2", "mc_CF_3",
                                                                                     "fo_CF_1", "fo_CF_2", "fo_CF_3",
                                                                                     "gs_CF_1", "gs_CF_2", "gs_CF_3",
                                                                                     "rc_CF_1", "rc_CF_2", "rc_CF_3",
                                                                                     "MB_SO_1", "MB_SO_2", "MB_SO_3",
                                                                                     "kc_SO_1", "kc_SO_2", "kc_SO_3",
                                                                                     "mc_SO_1", "mc_SO_2", "mc_SO_3",
                                                                                     "fo_SO_1", "fo_SO_2", "fo_SO_3",
                                                                                     "gs_SO_1", "gs_SO_2", "gs_SO_3",
                                                                                     "rc_SO_1", "rc_SO_2", "rc_SO_3",
                                                                                     "MB_SH_1", "MB_SH_2", "MB_SH_3",
                                                                                     "kc_SH_1", "kc_SH_2", "kc_SH_3",
                                                                                     "mc_SH_1", "mc_SH_2", "mc_SH_3",
                                                                                     "fo_SH_1", "fo_SH_2", "fo_SH_3",
                                                                                     "gs_SH_1", "gs_SH_2", "gs_SH_3",
                                                                                     "rc_SH_1", "rc_SH_2", "rc_SH_3"))

#Boxplot for all tech replicates for all quant_methods
boxplot_recoveries <- ggplot(data = rs2_anovas_tech, aes(x = treatment_tech2, y = recoveries, color = soil_type)) + 
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
ggsave("figures/Boxplot_TechRepComp_TT_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_TT_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_TechRepComp_MEOH_TT_MethodComp.svg", boxplot_recoveries, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_TechRepComp_MEOH_TT_MethodComp.pdf", boxplot_recoveries, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)



#Recovery Box Plots for MS quant_methods
rs2_anovas_tech_mz <- rs2_anovas_tech %>% 
  #filter(quant_method != "LDOPA_280nm") %>%
  filter(quant_method != "MUBph_315nm") 

method.labs <- c(
  #"LDOPA_198mz" = "LDOPA",
  "MUBph_257mz" = "MUBph")

boxplot_recoveries_mz <- ggplot(data = rs2_anovas_tech_mz, aes(x = treatment_tech2, y = recoveries, color = soil_type)) + 
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
ggsave("figures/Boxplot_MSTechRepComp_TT_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_TT_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

ggsave("figures/Boxplot_MSTechRepComp_MEOH_TT_MethodComp.svg", boxplot_recoveries_mz, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_MSTechRepComp_MEOH_TT_MethodComp.pdf", boxplot_recoveries_mz, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

#Recovery Box Plots for nm quant_methods
rs2_anovas_tech_nm <- rs2_anovas_tech %>% 
  filter(quant_method != "LDOPA_198mz") %>%
  filter(quant_method != "MUBph_257mz") 

method.labs <- c(
  "LDOPA_280nm" = "LDOPA",
  "MUBph_315nm" = "MUBph")

boxplot_recoveries_nm <- ggplot(data = rs2_anovas_tech_nm, aes(x = treatment_tech2, y = recoveries, color = soil_type)) + 
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
ggsave("figures/Boxplot_NMTechRepComp_TT_MethodComp.svg", boxplot_recoveries_nm, width = 30, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/Boxplot_NMTechRepComp_TT_MethodComp.pdf", boxplot_recoveries_nm, device=cairo_pdf, width = 30, height = 18, units = "cm",  dpi = 1200)

