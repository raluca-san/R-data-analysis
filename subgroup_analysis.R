setwd("C:/develop/R-data-analysis")

library(lsmeans)

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


# interaction --------------------------------------------------------------------

m.interaction <- lm(EAV ~ PAV*Subcapsular, data = radiomics)
anova(m.interaction)

# Obtain slopes ------------------------------------------------------------------
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "Subcapsular", var="PAV")

# Compare slopes ------------------------------------------------------------------

pairs(m.lst)

# test the difference between residual variances ---------------------------------------------
library(psych)
library(data.table)

m.correlations <- radiomics[, cor(EAV, PAV), by = Subcapsular]
m.correlations
# Compare R values with Fisher's R to Z 
radiomics <- as.data.table(radiomics)
paired.r(m.correlations[Subcapsular==TRUE, V1], m.correlations[Subcapsular==FALSE, V1], 
         n = radiomics[Subcapsular %in% c(TRUE, FALSE), .N])



