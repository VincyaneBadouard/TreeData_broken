## code to prepare `StandardData` dataset goes here

#### Packages libraries ####
library(TreeData)
library(data.table)


#### Import data ####

data("ParacouSubset") # import data
Data = ParacouSubset
## data.frame to data.table
setDT(Data) # with "set" "<-" is not necessary

#### Delete some columns ####

# Data[, CircCorr := NULL] # je les garde pour l'instant pour comparer avec mes corrections
# Data[, CorrCode := NULL]


#### Columns renaming ####
# setnames(Data, "old", "new")
Data <- setnames(Data, "Forest", "Site")
Data[, Plot := NULL]
Data <- setnames(Data, "SubPlot", "Plot")
Data <- setnames(Data, "SubSubPlot", "SubPlot")
Data <- setnames(Data, "CodeAlive", "LifeStatus")
Data <- setnames(Data, "idTree", "IdTree")

#### Circ to DBH ####
if (!"DBH" %in% names(Data) & "Circ" %in% names(Data)) {
  Data[, DBH := Circ/pi]
}

#### Necessary columns creation from the existing ####
# ScientificName (if Genus & Species exist)
if(!"ScientificName" %in% names(Data) & all(c("Genus", "Species") %in% names(Data))){ # or ScientificName == "none"
  Data[, ScientificName := paste(Genus, Species, sep = "_")]
}



#### Class changing ####
### as.character
CharacVar <- c("Plot", "SubPlot", "TreeFieldNum", "IdTree", "ScientificName", "VernName", "Family", "Genus", "Species") # character variables

Data[, (CharacVar) := lapply(.SD, as.character), .SDcols = CharacVar] # (CharacVar) to say that these are existing columns and not new ones to create

### as.numeric
NumVar <- c("CensusYear", "DBH", "PlotArea", "Xutm", "Yutm") # numeric variables

Data[, (NumVar) := lapply(.SD, as.character), .SDcols = NumVar] # first as c haracter when the variable is in factor, to preserve writed information
Data[, (NumVar) := lapply(.SD, as.numeric), .SDcols = NumVar] # () to say that these are existing columns and not new ones to create

### as.logical
LogicVar <- c("LifeStatus", "CommercialSp") # logical variables

Data[, (LogicVar) := lapply(.SD, as.logical), .SDcols = LogicVar] # () to say that these are existing columns and not new ones to create


#### Save this test data in the package ####
StandardData <- Data

usethis::use_data(StandardData, overwrite = TRUE)
