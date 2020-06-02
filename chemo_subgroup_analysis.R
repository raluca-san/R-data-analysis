setwd("C:/develop/R-data-analysis")

library(lsmeans)
library(ggplot2)
library(readxl)
library(lme4)

radiomics <- read_excel("Radiomics_MAVERRIC----003328-20200523_.xlsx")

radiomics <- radiomics[radiomics$Inclusion_Energy_PAV_EAV == TRUE, ]

radiomics$PAV <- radiomics$`Predicted_Ablation_Volume`
radiomics$EAV <- radiomics$`Ablation Volume [ml]`
radiomics$Energy <- radiomics$`Energy [kj]`
radiomics$Ratio <- radiomics$EAV / radiomics$PAV
radiomics$TumorVolume <- radiomics$`Tumour Volume [ml]`
radiomics$Chemotherapy <- radiomics$chemo_before_ablation
radiomics$Subcapsular <- as.logical(radiomics$Proximity_to_surface)
radiomics$Time <- radiomics$Time_Duration_Applied


# Chemotherapy summaries-----------------------------------------------------------
mav.Chemotherapy <- subset(radiomics,
                          subset = radiomics$Chemotherapy == 'Yes')

sum.Chemotherapy = nrow(mav.Chemotherapy)
print(sum.Chemotherapy)
chemo.lm <- lm(EAV ~ PAV, data = mav.Chemotherapy)
summary(chemo.lm)
r_squared <- summary(chemo.lm)$r.squared
print(r_squared)

mav.nochemo <- subset(radiomics,
                             subset = radiomics$Chemotherapy == 'No')
nochemo.lm <- lm(EAV ~ PAV, data = mav.nochemo)
summary(nochemo.lm)
sum.nochemo = nrow(mav.noChemo)
print('no of nonchemotherapy:')
print(sum.nochemo)
r_squared <- summary(nochemor.lm)$r.squared
print(r_squared)

# Subgroup interaction --------------------------------------------------------------------

m.interaction <- lm(EAV ~ PAV*Chemotherapy, data = radiomics)
library(interactions)
interact_plot(m.interaction, pred = PAV, modx = Chemotherapy)
interact_plot(m.interaction, pred = PAV, modx = Chemotherapy, interval = TRUE)
anova(m.interaction)
summary(m.interaction)

# Obtain slopes ------------------------------------------------------------------
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "Chemotherapy", var="PAV")
print(m.lst)
# Compare slopes ------------------------------------------------------------------
pairs(m.lst)

# test the difference between residual variances ---------------------------------------------
library(psych)
library(data.table)
radiomics <- as.data.table(radiomics)

m.correlations <- radiomics[, cor(EAV, PAV), by=Chemotherapy]
m.correlations
# Compare R values with Fisher's R to Z 
radiomics <- as.data.table(radiomics)
paired.r(m.correlations[Chemotherapy=='Yes', V1], m.correlations[Chemotherapy=='No', V1], 
         n = radiomics[Chemotherapy %in% c('Yes', 'No'), .N])


