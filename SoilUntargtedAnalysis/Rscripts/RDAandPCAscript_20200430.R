
###########################################################################
######################   RDA analysis  ####################
################################################################################
###########################################################################

## Let's start with a black slate - Let's re-start the R session

# Ctrl + Shift +  F10  (PC / Linux)
# Command + Shift + Fn + F10 (Mac OS)

#AUTHOR:Madison Bell
#Date updated: 2019-08-22


#================= Packages needed ===========================================================================

#Data Handling
library(tidyverse)
library(janitor)

#Plotting
library(extrafont)
library(ggfortify)
library(ggvegan)
library(ggrepel)
library(ggpubr)

#Analysis
library(vegan)
library(missForest)
library(Hmisc)
library(caret)

#=============== Import Data ===================================================================================

#Import excel file (sample names in Rows and variables in columns) 
tree <-read_csv("data_raw/SoilMethodTest_H2O_SoilOmics_IQR_MedianPareto_NoBlanksNoClay.csv")


#ID      Variable1   Variable2
#--------------------------------------------
#lake1   intensity   intensity
#lake2   intensity   intensity

desctree <-read_csv("data_raw/SoilMethodTest_H2O_SoilOmics_Descriptions.csv")
desctree <- clean_names(desctree) %>%
  rename(ph = p_h) %>%
  mutate(num = row_number())


#ID      Variable1   Variable2
#--------------------------------------------
#lake1   value       value
#lake2   value       value


wmat <-read_csv("data_raw/SoilMethodTest_H2O_SoilOmics_Weights.csv")
wmat <- clean_names(wmat)


#ID      Initital weight (g)  
#---------------------------
#lake1   value
#lake2   value

#=============== Clean & Reogrganize Data ===================================================================================
#Encode Dummy Variables if necessary

dmy <- desctree %>%
  select(c("num", "method","soil_type"))  ###Select the columns of the variables to be converted to dummy variables + one variable that is an unique identifer for all samples

dmy2 <- dummyVars(" ~ .", fullRank = TRUE, data = dmy)     #Encode the Dummy variables
trsf <- data.frame(predict(dmy2, newdata = dmy))

desctree <- desctree %>%
  left_join(trsf, by=c("num")) ###Join the two data frames

#Filter and Subset the dataframes 
###All Data subset
desctree_all <- desctree %>%
  select(c("id","ph", "specific_sa", "cec", "soil_p_toc", "extract_toc")) %>%
  #select(c("id", "soil_p_clay", "ph", "soil_p_toc", "extract_toc")) %>%
  as.data.frame

tree_all <- tree[-1]   #turn the first column (the ID column) into a vector
row.names(tree_all) <- tree$ID  #make the rownames become the first column and match them to the first column

desctree_all2 <- desctree_all[-1]   #turn the first column (the ID column) into a vector
row.names(desctree_all2) <- desctree_all$id  #make the rownames become the first column and match them to the first column

###Dummy Data subset
desctree$soil_type <- as.factor(desctree$soil_type)

desctree_dummy <- desctree %>%
  select(c("id", "soil_type", "extract_toc", "extract_tn")) %>%
  as.data.frame

tree_dummy <- tree[-1]   #turn the first column (the ID column) into a vector
row.names(tree_dummy) <- tree$ID  #make the rownames become the first column and match them to the first column

a <- desctree_dummy %>%
  select(c("id", "soil_type"))

b <- desctree_dummy %>%
  select(c("id", "extract_toc", "extract_tn")) %>%
  mutate_at(c("extract_toc", "extract_tn"), ~(scale(.) %>% as.vector)) %>%
  right_join(a, by = "id") %>%
  as.data.frame  

desctree_dummy2 <- b[-1]   #turn the first column (the ID column) into a vector
row.names(desctree_dummy2) <- b$id  #make the rownames become the first column and match them to the first column

###CF Data subset
desctree_cf <- desctree %>%
  select(-c("treatment", "method", "soil_type", "soil_p_sand", "soil_p_silt", "soil_p_clay", 
               "soil_p_tc", "num", "methodSH", "methodSO", "soil_typegs", "soil_typerc", "extract_toc", "extract_tn")) %>%
  filter(stringr::str_detect(id, 'CF') ) %>%
  as.data.frame

tree_cf <- tree %>%
  filter(stringr::str_detect(ID, 'CF') ) %>%
  as.data.frame

tree_cf2 <- tree_cf[-1]   #turn the first column (the ID column) into a vector
row.names(tree_cf2) <- tree_cf$ID  #make the rownames become the first column and match them to the first column

desctree_cf2 <- desctree_cf[-1]   #turn the first column (the ID column) into a vector
row.names(desctree_cf2) <- desctree_cf$id  #make the rownames become the first column and match them to the first column

###SH Data subset
desctree_sh <- desctree %>%
  select(-c("treatment", "method", "soil_type", "soil_p_sand", "soil_p_silt", "soil_p_clay", 
            "soil_p_tc", "num", "methodSH", "methodSO", "soil_typegs", "soil_typerc", "extract_toc", "extract_tn")) %>%
  filter(stringr::str_detect(id, 'SH') ) %>%
  as.data.frame

tree_sh <- tree %>%
  filter(stringr::str_detect(ID, 'SH') ) %>%
  as.data.frame

tree_sh2 <- tree_sh[-1]   #turn the first column (the ID column) into a vector
row.names(tree_sh2) <- tree_sh$ID  #make the rownames become the first column and match them to the first column

desctree_sh2 <- desctree_sh[-1]   #turn the first column (the ID column) into a vector
row.names(desctree_sh2) <- desctree_sh$id  #make the rownames become the first column and match them to the first column

###SO Data subset
desctree_so <- desctree %>%
  select(-c("treatment", "method", "soil_type", "soil_p_sand", "soil_p_silt", "soil_p_clay", 
            "soil_p_tc", "num", "methodSH", "methodSO", "soil_typegs", "soil_typerc", "extract_toc", "extract_tn")) %>%
  filter(stringr::str_detect(id, 'SO') ) %>%
  as.data.frame

tree_so <- tree %>%
  filter(stringr::str_detect(ID, 'SO') ) %>%
  as.data.frame

tree_so2 <- tree_so[-1]   #turn the first column (the ID column) into a vector
row.names(tree_so2) <- tree_so$ID  #make the rownames become the first column and match them to the first column

desctree_so2 <- desctree_so[-1]   #turn the first column (the ID column) into a vector
row.names(desctree_so2) <- desctree_so$id  #make the rownames become the first column and match them to the first column


###Adjusted explanatory matrix with weights
wmat <- wmat %>%
  filter(!stringr::str_detect(id, 'MB')) %>%
  filter(!stringr::str_detect(id, 'MeOH')) %>%
  filter(!stringr::str_detect(id, 'kc')) %>%
  filter(!stringr::str_detect(id, 'mc')) %>%
  rename(id2 = id)

desctree_w <- desctree %>%
  select(-c("treatment", "method", "soil_type")) %>%
  separate(id, c("method", "soil_type", "extract", "bio_rep", "tech_rep"), remove = FALSE) %>%
  unite(id2, c("method", "soil_type", "extract", "bio_rep"), sep = "_", remove = TRUE, na.rm = FALSE) %>%
  right_join(wmat) %>%
  mutate(soil_clay = soil_p_clay/100  * initial_soil_weight_g) %>%
  mutate(soil_toc = soil_p_toc/100  * initial_soil_weight_g) %>%
  mutate(soil_tn = soil_p_tn/100  * initial_soil_weight_g) %>%
  mutate(soil_adj_ssa = specific_sa * initial_soil_weight_g) %>%
  mutate(soil_adj_cec = cec * 100 * initial_soil_weight_g) %>%
  mutate(extract_toc = extract_toc / 1000 /1000 * initial_soil_weight_g) %>%
  mutate(extract_tn = extract_tn / 1000 /1000 * initial_soil_weight_g) %>%
  select(c("id", "soil_clay", "soil_toc", "soil_tn", "soil_tn", "soil_adj_ssa",
           "soil_adj_cec", "extract_toc", "extract_tn", "ph")) %>%
  as.data.frame

tree_w <- tree[-1]   #turn the first column (the ID column) into a vector
row.names(tree_w) <- tree$ID  #make the rownames become the first column and match them to the first column

desctree_w2 <- desctree_w[-1]   #turn the first column (the ID column) into a vector
row.names(desctree_w2) <- desctree_w$id  #make the rownames become the first column and match them to the first column


#=============== Scale and Centre Data ===================================================================================
#Automatic model selection
desctree_scale <- desctree_all2 %>%
  scale(center = TRUE, scale = TRUE) %>%
  as.data.frame             #Scale the imputed_data set since the tree3 is already scaled

#Specify dataframes to use
spc = tree_all
exp = desctree_all2

#=============== Simple RDA ===================================================================================
#RDA with all of the variables 
simple.rda <- rda(spc, exp, scale=F) 
coef(simple.rda)
RsquareAdj(simple.rda)
alias(simple.rda)
vif.cca(simple.rda)

summary(simple.rda) #shows how much is explained by your constraining variables
anova(simple.rda)

envfit(simple.rda, exp) # identify those variables whose vectors are significantly correlated with the site loadings 

plot(simple.rda) 

#####Continue to subset models from simple RDA by envfit
exp_sub <- exp %>%
  select("ph", "bulk_density", "specific_sa", "cec",
         "extract_toc", "extract_tn") #Select columns manually

simple.rda.envfit <- rda(spc, exp_sub, scale=F) 
simple.rda.envfit
summary(simple.rda.envfit)
anova_axis(simple.rda.envfit, by = "axis")
anova_terms(simple.rda.envfit, by = "term") 
envfit(simple.rda, exp_sub)

plot(simple.rda.envfit) 

#=============== Redundancy Analysis and RDA ===================================================================================
#Use this to determine if there are redundant variables in your explanatory dataset
V <- redun(~., data = exp, r2 = 0.95, nk=0) 
V$In
V$Out

#Subset the data frame for RDA
exp_sub_V <- exp %>%
  select(V$In) ##Select variables from the redundancy analysis

#RDA
v.rda <- rda(spc, exp_sub_V, scale=F) 
coef(v.rda)
RsquareAdj(v.rda)
alias(v.rda)
vif.cca(v.rda)

summary(v.rda)
anova(v.rda) 
envfit(v.rda, exp_sub_V)

plot(v.rda) 


#=============== Automatic Model Selection and RDA ===================================================================================
m0 <- rda(spc ~ 1, exp)
m1 <- rda(spc ~ ., exp)

######Method one based on AIC criteria (ordination doesn't really have AIC values so operate with grain of salt)
step.res <- step(m0, scope=formula(m1), test="perm")
step.res
summary(step.res)
anova(step.res)

exp_step.res <- exp %>%
  select("soil_p_n", "cec")

envfit(step.res, exp_step.res)

######Method two based on p-value criteria 
m <- ordistep(m0, scope = formula(m1),  Pin = 0.05, Pout = 0.1)
m$anova
coef(m)
RsquareAdj(m)
alias(m)
vif.cca(m)

summary(m)
anova(m)

exp_m <- exp %>%
  select("extract_toc", "cec", "ph")

envfit(m, exp_m)

plot(m)

######Method three based on maximizing R2
m2 <- ordiR2step(m0, m1, perm.max = 200,  Pin = 0.05, Pout = 0.1)
m2
m2$anova
coef(m2)
RsquareAdj(m2)
alias(m2)
vif.cca(m2)

anova(m2)
summary(m2)

exp_m2 <- exp %>%
  select("cec", "extract_toc")

envfit(m2, exp_m2)

plot(m2)



#=========== Replotting in ggplot2 ======================================================
########Reorganizing so categories can be included (may not be necessary)
categories <- desctree %>%
  select('id', 'treatment', "method", "soil_type") %>%
  rename(Label = id) %>%
  as.data.frame


#############Setting up the data for RDA plot

ford <- fortify(v.rda, axes = 1:2) %>%
  merge(categories, by = "Label", all=TRUE)  # fortify the ordination and merge the categories in

take <- c('RDA1', 'RDA2')  # make a list of which columns contain the scores we want

arrows <- subset(ford, Score == 'biplot')  # take only biplot arrow scores

mul <- ggvegan:::arrowMul(arrows[, take],
                          subset(ford, select = take, Score == 'sites')) ## multiplier for arrows to scale them to the plot range

arrows[, take] <- arrows[, take] * mul  # scale biplot arrows

#Set the Color palette and shapes
#cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#7E6148B2", "#0072B2", "#D55E00", "#CC79A7","#999999","#d0dc87")

cbbPalette <- c("#F6E9CA", "#e1b550", "#876D30", "#E3BDB6", "#b95a49", "#6F362B", "#CBDEE8",  "#5391b3", "#294859")

shapePalette <- c(15, 16, 17, 15, 16, 17, 15, 16, 17)
#########Use ggplot2 to make a more customizable plot (split into RDA plot and biplot with arrows)
rda.plot <- ggplot() + 
  geom_point(data = subset(ford, Score == 'sites'),
             mapping = aes(x = RDA1, y = RDA2, color = treatment, shape = treatment), size=5) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  ylim(-4.5,4.5) +
  xlim(-4.5,4.5) + 
  labs(x = "RDA 1 (22.82%)", y="RDA 2 (14.03%)", color = "Treatments", shape = "Treatments") +
  coord_fixed(ratio = 1) +
  theme_pubr() +
  scale_color_manual(values = cbbPalette) +
  scale_shape_manual(values = shapePalette) +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent"), 
        text = element_text(size=15,  family="Arial"), 
        legend.position = "right")


rda.biplot <- rda.plot +
  geom_segment(data = arrows,
               mapping = aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.01, "npc")), size = 1, alpha=0.8) +
  geom_text_repel(data = arrows,
            mapping = aes(label = Label, x = RDA1 * 1.1, y = RDA2 * 1.1), 
            force = 50,
            segment.alpha = 0.5, 
            size = 4,
            direction = "both",
            fontface = 'bold', 
            segment.color = 'grey50')

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/RDA_MethodComp_FinalVIn.svg", rda.biplot, width = 20, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/RDA_MethodComp_FinalVIn.pdf", rda.biplot, device = cairo_pdf, width = 20, height = 18, units = "cm",  dpi = 1200)

























###########################################################################
######################   Recreating Metaboanalyst PCA/PLSDA  ####################
################################################################################
###########################################################################

## Let's start with a black slate - Let's re-start the R session

# Ctrl + Shift +  F10  (PC / Linux)
# Command + Shift + Fn + F10 (Mac OS)

#AUTHOR:Madison Bell
#Date updated: 2019-08-22


#================= Packages needed ===========================================================================

library(tidyverse)
library(pls)
library(factoextra)
library(ggrepel)
library(ggpubr)
library(extrafont)

#=============== Import Data ===================================================================================
desctree_pca <- desctree %>%
  select("id", "treatment") %>%
  rename(Label = treatment, 
         ID = id)

norm <- desctree_pca %>%
  right_join(tree, by = "ID")

#=============== Clean & Reogrganize Data ===================================================================================

###Collect the categories from the original dataframe

category <- norm[,c("ID", "Label")]

###Reorganize the data frame for pca
row.names(norm) <- norm$ID              #Make your sample names into rownames

norm.pca <-norm %>%
  select(-c(ID, Label)) %>%             #Remove samples names and Labels
  as.data.frame()

str(norm.pca)                           #Check matric to make sure class assignments are correct


#=============== PCA ===================================================================================

#Run PCA using prcomp (same package MetaboanalystR uses)
pca <- prcomp(norm.pca, center = TRUE, scale = FALSE)
summary(pca)

#Set the Color palette and shapes
cbbPalette <- c("#F6E9CA", "#e1b550", "#876D30", "#E3BDB6", "#b95a49", "#6F362B", "#CBDEE8",  "#5391b3", "#294859")

shapePalette <- c(15, 16, 17, 15, 16, 17, 15, 16, 17)

#Plot PCA (with labels)
pca_labels <- fviz_pca_ind(pca, axes = c(1, 2), habillage=norm$Label, addEllipses = TRUE, 
                           ellipse.level=0.95, ellipse.alpha = 0.1, 
                           repel = T, pointsize = 3, invisible="quali") +
  theme_pubr() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent"), 
        text = element_text(size=15,  family="Arial"),
        legend.position = "right") +
  coord_fixed(ratio = 1) + 
  labs(color = "Treatments", shape = "Treatments", fill = "Treatments") +
  scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  scale_shape_manual(values = shapePalette)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/PCA_MethodComp_wLabels.svg", pca_labels, width = 20, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/PCA_MethodComp_wLabels.pdf", pca_labels, device = cairo_pdf, width = 20, height = 18, units = "cm",  dpi = 1200)


#Plot PCA (without labels)
pca <- fviz_pca_ind(pca, axes = c(1, 2), habillage=norm$Label, addEllipses = TRUE, 
                    ellipse.level=0.95, ellipse.alpha = 0.1, 
                    pointsize = 4, invisible="quali", label="none") +
  theme_pubr() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent"), 
        text = element_text(size=15,  family="Arial"),
        legend.position = "right") +
  coord_fixed(ratio = 1) + 
  labs(color = "Treatments", shape = "Treatments", fill = "Treatments") +
  scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  scale_shape_manual(values = shapePalette)

#Save the files as .svg and/or .pdf. DONT FORGET TO CHANGE THE FILE NAMES AS REQUIRED 
ggsave("figures/PCA_MethodComp.svg", pca, width = 20, height = 18, units = "cm",  dpi = 1200)
ggsave("figures/PCA_MethodComp.pdf", pca, device = cairo_pdf, width = 20, height = 18, units = "cm",  dpi = 1200)




#=============== PLSDA ===================================================================================
#Run PLSDA using pls (same package as MetaboanalystR uses)

datmat <- as.matrix(norm.pca)

comp.num <- dim(norm.pca)[1]-1;
if(comp.num > 8) {
  comp.num <- 8;
}

C <- norm$Label
C2 <- as.factor(C)

if(TRUE){                                                     #Make the labels into categorical binary operators (i.e. 0 or 1)
  C2 <- scale(as.numeric(C2))[,1];
}else{
  C2 <- model.matrix(~C2-1);
}

plsr.analysis <- pls::plsr(C2~datmat, method='oscorespls', ncomp=comp.num)

#Plot PLSDA (with labels)
#############Extract information from plsr object
plsr2<-plsr.analysis$scores
comp1a<-plsr2[,1]
comp2a<-plsr2[,2]
plsr2<-as.data.frame(cbind(comp1a, comp2a, category))

var_comp1 <- as.data.frame(plsr.analysis$Xvar)                  #Determine the variation on the principle components
round(100*var_comp1[1:1, "plsr.analysis$Xvar"]/plsr.analysis$Xtotvar, 1)   #PC1 variance percent
round(100*var_comp1[2:2, "plsr.analysis$Xvar"]/plsr.analysis$Xtotvar, 1)   #PC2 variance percent

ggplot(data=plsr2, aes(comp1a,comp2a, color = Label, shape = Label))+
  geom_point(size=3) +
  geom_text_repel(aes(label=ID)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  ylab("Comp 2 (8.1%)") + 
  xlab("Comp 1 (20.1%)") +
  theme_pubr() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent"), 
        text = element_text(size=15,  family="Arial"),
        legend.position = "right") +
  coord_fixed(ratio = 1) +
  stat_ellipse(type = "norm", geom = "polygon", alpha = 0.1, aes(fill = Label)) +
  labs(color = "Treatments", shape = "Treatments", fill = "Treatments") +
  scale_color_manual(values = cbbPalette) +
  scale_fill_manual(values = cbbPalette) +
  scale_shape_manual(values = shapePalette)



tiff("figures/PLSDA_RecreatePCAscript_Treeline_Labels_Coordunfixed .tiff", units="in", width=7, height=7, res=300)


ggplot(data=plsr2, aes(comp1a,comp2a, color = Label, shape = Label))+
  geom_point(size=3) +
  geom_text_repel(aes(label=ID)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  ylab("Comp 2 (5.4%)") + 
  xlab("Comp 1 (49.1%)") +
  theme_pubr() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent"), 
        text = element_text(size=15,  family="Times New Roman")) +
  #coord_fixed(ratio = 1) +
  stat_ellipse(type = "norm", geom = "polygon", alpha = 0.1, aes(fill = Label)) +
  scale_fill_viridis(discrete = TRUE, option="viridis") +
  scale_color_viridis(discrete = TRUE, option="viridis")

