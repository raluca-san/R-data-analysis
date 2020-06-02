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
radiomics$Chemotherapy <- as.logical(radiomics$chemo_before_ablation)
radiomics$Subcapsular <- as.logical(radiomics$Proximity_to_surface)
radiomics$Time <- radiomics$Time_Duration_Applied

# linear regression model eav vs pav ----------------------------------------------
eav_pav_linearModel <- lm(EAV ~ PAV, data=radiomics)
summary(eav_pav_linearModel)
summary(eav_pav_linearModel)$r.squared

res <- cor.test(radiomics$PAV, radiomics$EAV, method = "pearson", exact=FALSE)
res$estimate

slopes_eav_pav <- lstrends(eav_pav_linearModel, var="PAV")
print(slopes_eav_pav)
# subcapsular summaries-----------------------------------------------------------
mav.subcapsular <- subset(radiomics,
                             subset = radiomics$Subcapsular == TRUE)

sum.subcapsular = nrow(mav.subcapsular)
print('no of subcapsular:')
print(sum.subcapsular)
subcapsular.lm <- lm(EAV ~ PAV, data = mav.subcapsular)
summary(subcapsular.lm)
modelCoeffs <- subcapsular.lm$coefficients  # model coefficients
#beta.estimate <- modelCoeffs["PAV", "Estimate"]  # get beta estimate 
#std.error <- modelCoeffs["PAV", "Std. Error"]  # get std.error
r_squared <- summary(subcapsular.lm)$r.squared
print(r_squared)

mav.notsubcapsular <- subset(radiomics,
                             subset = radiomics$Subcapsular == FALSE)
notsubcapsular.lm <- lm(EAV ~ PAV, data = mav.notsubcapsular)
summary(notsubcapsular.lm)
sum.notsubcapsular = nrow(mav.notsubcapsular)
print('no of non-subcapsular:')
print(sum.notsubcapsular)
r_squared <- summary(notsubcapsular.lm)$r.squared
print(r_squared)
# Subgroup interaction --------------------------------------------------------------------

m.interaction <- lm(EAV ~ PAV*Subcapsular, data = radiomics)
library(interactions)
interact_plot(m.interaction, pred = PAV, modx = Subcapsular)
interact_plot(m.interaction, pred = PAV, modx = Subcapsular, interval = TRUE)
anova(m.interaction)
summary(m.interaction)

# Obtain slopes ------------------------------------------------------------------
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "Subcapsular", var="PAV")
print(m.lst)
# Compare slopes ------------------------------------------------------------------
pairs(m.lst)

# test the difference between residual variances ---------------------------------------------
library(psych)
library(data.table)
radiomics <- as.data.table(radiomics)

m.correlations <- radiomics[, cor(EAV, PAV), by=Subcapsular]
m.correlations
# Compare R values with Fisher's R to Z 
radiomics <- as.data.table(radiomics)
paired.r(m.correlations[Subcapsular==TRUE, V1], m.correlations[Subcapsular==FALSE, V1], 
         n = radiomics[Subcapsular %in% c(TRUE, FALSE), .N])


