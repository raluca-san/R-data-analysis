setwd("C:/develop/R-data-analysis")

library(readxl)


radiomics <- read_excel("Radiomics_Acculis_MAVERRIC_22012020.xlsx")

radiomics <- radiomics[radiomics$Device_name == 'Angyodinamics (Acculis)', ]

radiomics$PAV <- radiomics$`Ablation Volume [ml] (manufacturers)`
radiomics$EAV <- radiomics$`Ablation Volume [ml]`
radiomics$Energy <- radiomics$`Energy [kj]`
radiomics$Ratio <- radiomics$EAV / radiomics$PAV
radiomics$TumorVolume <- radiomics$`Tumour Volume [ml]`
radiomics$Chemotherapy <- radiomics$no_chemo_cycle > 0
radiomics$Subcapsular <- radiomics$Proximity_to_surface
radiomics$Time <- radiomics$Time_Duration_Applied

hist(radiomics$Ratio)
boxplot(radiomics$Ratio)

wilcox.test(radiomics$PAV, radiomics$EAV, alternative='greater')

summary(radiomics$PAV)
summary(radiomics$EAV)

# is the data normally distributed
# shapiro test for normality
shapiro <- shapiro.test(radiomics$EAV)
shapiro <- shapiro.test(radiomics$PAV)
res <- cor.test(radiomics$PAV, radiomics$EAV, method = "spearman", exact=FALSE)
res$estimate
# spearman correlation test

radiomics.lm <- lm(Ratio ~ TumorVolume+Energy+Chemotherapy+Subcapsular+Proximity_to_vessels, 
                   data = radiomics)

summary(radiomics.lm)
radiomics.prediction <- lm(EAV ~ TumorVolume + Energy+Chemotherapy+Subcapsular+Proximity_to_vessels+PAV, 
                           data = radiomics)
summary(radiomics.prediction)

radiomics.pred2 <- lm(EAV ~ Power+Time+Chemotherapy, data = radiomics)
summary(radiomics.pred2)

radiomics.pred3 <- lm(EAV ~ Energy, data = radiomics)
summary(radiomics.pred3)
