## code to prepare `BarroColoradoSubset` dataset goes here

#### Import data ####
BarroColoradoFull <- fread("D:/VSC TmFO/Data/Barro Colorado_data/FullMeasurementBCI/Barro Colorado_fulldata.tsv")

#### Subset ####
BarroColoradoSubset <- BarroColoradoFull

BarroColoradoSubset[, Year := as.numeric(format(ExactDate, "%Y"))]

BCplots <- BarroColoradoSubset[QuadratName == 1 | QuadratName == 2 | QuadratName == 3, ]

BarroColoradoSubset <- BCplots[Year >= 1995] # 3829 obs


#### Save this data in the package ####
usethis::use_data(BarroColoradoSubset, overwrite = TRUE)
