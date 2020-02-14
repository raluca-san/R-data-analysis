library(tableone)
library(dplyr)

setwd("C:/Users/Raluca Sandu/Downloads")

mav <- read.csv("C:/develop/lit/lesions.csv")
View(mav)

mav$Lesion.id <- as.character(mav$Lesion.id)
mav$CT_plan <- as.logical(mav$CT_plan)
mav$CT_validation <- as.logical(mav$CT_validation)
mav$segmentation_tumor <- as.logical(mav$segmentation_tumor)
mav$segmentation_ablation <- as.logical(mav$segmentation_ablation)
mav$location_5mm_liver_surface <- as.logical(mav$location_5mm_liver_surface)
mav$study_institution <- as.factor(mav$study_institution)
mav$patient_number <- as.factor(mav$patient_number)

sum.total = nrow(mav)
nrow(mav[mav$CT_validation == FALSE, ])
nrow(mav[mav$segmentation_tumor == FALSE, ])
nrow(mav[mav$segmentation_ablation == FALSE, ])
nrow(mav[mav$CT_validation == FALSE | mav$segmentation_ablation == FALSE, ])

nrow(mav[mav$location_5mm_liver_surface == TRUE, ])

mav.dataavail <- subset(mav, 
                        subset = mav$CT_validation == TRUE &
                          mav$segmentation_tumor == TRUE & 
                          mav$segmentation_ablation == TRUE)

sum.dataavail = nrow(mav.dataavail)
sum.datamissing = sum.total - sum.dataavail

# exclusion of subcapsular

mav.notsubcapsular <- subset(mav.dataavail,
                             subset = mav.dataavail$location_5mm_liver_surface == FALSE)

sum.subcapsular = nrow(mav.dataavail[mav.dataavail$location_5mm_liver_surface == TRUE,])
sum.notsubcapsular = nrow(mav.notsubcapsular)

# exclusion of patients with other treatment

print(c('Total', sum.total))
print(c('Data not available', sum.datamissing))
print(c('Data vailable', sum.dataavail))
print(c('Subcapsular', sum.subcapsular))
print(c('Not subcapsular', sum.notsubcapsular))

######################################
##### contigencz table
vars = c('CT_plan', 'CT_validation', 'segmentation_tumor', 'segmentation_ablation', 'location_5mm_liver_surface')
CreateTableOne(vars = vars, data = mav)

strata = c('study_institution')

CreateTableOne(vars = vars, data = mav, strata = strata)

