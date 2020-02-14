library(tableone)
library(dplyr)
library(ggpubr)
library(xlsx)

setwd("C:/develop/data-analysis")

data_ellispoid <- read.csv("C:/develop/data-analysis/Ellipsoid_Brochure_Info.csv")
print(data_ellispoid)
data_radiomics <- read.csv("C:/develop/data-analysis/Radiomics_MAVERRIC_111119.csv")
print(data_radiomics)


