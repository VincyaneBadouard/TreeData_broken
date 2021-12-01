## code to prepare `DATASET` dataset goes here

# usethis::use_data(DATASET, overwrite = TRUE)

# ParacouSubset

# Paracou6 <- EcoFoG::Guyafor2df(WHERE="Forest='Paracou' AND Plot='6'")
# setDT(Paracou6) # as data.table
# Paracou6C1 <- Paracou6[SubPlot == 1]
#
# We select the 5 last censuses 2016-2020:
#
# ParacouSubset <- Paracou6C1[CensusYear >= 2016] # 4904 obs
#
# ind <- sample(unique(ParacouSubset$idTree), 1000)
#
# ParacouSubset <- ParacouSubset[idTree %in% ind] # 4854 obs
#
# 4 sub-sub-squares creation:
#
# MaxX <- max(ParacouSubset$Xutm)
# MinX <- min(ParacouSubset$Xutm)
# MaxY <- max(ParacouSubset$Yutm)
# MinY <- min(ParacouSubset$Yutm)
#
# HalfX <- MinX + (MaxX - MinX)/2
# HalfY <- MinY + (MaxY - MinY)/2
#
# ParacouSubset[, SubSubPlot := NA_real_]
# ParacouSubset[Xutm < HalfX & Yutm > HalfY, SubSubPlot := "1"]
# ParacouSubset[Xutm > HalfX & Yutm > HalfY, SubSubPlot := "2"]
# ParacouSubset[Xutm < HalfX & Yutm < HalfY, SubSubPlot := "3"]
# ParacouSubset[Xutm > HalfX & Yutm < HalfY, SubSubPlot := "4"]
#
# ParacouSubset[, Project := NULL]


# ParacouSubsetWide

# data(ParacouSubset)
# ParacouSubsetWide <- dcast(ParacouSubset, idTree ~ CensusYear, value.var = "Circ")
# OtherCols <- copy(ParacouSubset)
# OtherCols[, c("CensusYear", "CensusDate", "CensusDateCertainty",
#               "Circ", "CircCorr", "CorrCode",
#               "CodeAlive", "MeasCode") := NULL]
# ParacouSubsetWide <- unique(merge(ParacouSubsetWide, OtherCols, by = "idTree")) #1000 ind -> 1000 rows


# BarroColoradoSubset

# BarroColoradoSubset <- BarroColoradoFull
# BarroColoradoSubset[, Year := as.numeric(format(ExactDate, "%Y"))]
# BCplots <- BarroColoradoSubset[QuadratName == 1 | QuadratName == 2 | QuadratName == 3, ]
# BarroColoradoSubset <- BCplots[Year >= 1995] # 3829 obs
