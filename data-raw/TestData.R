## code to prepare `TestData` dataset goes here

#### Packages libraries ####
library(TreeData)
library(data.table)


#### Import data ####

data("StandardData") # import data
Data = StandardData
## data.frame to data.table
setDT(Data) # with "set" "<-" is not necessary

nError <- 2 # number of error to add for each case


#### Missing tree size ####

# Size = NA
modif <- Data[, .I[sample(.N, nError)]] # .I = seq_len(nrow(Data)), .N = nrows in the group -> sample 2 rows number in Data
Data[modif, DBH := NA]
# Data[modif] # to check

# Size = 0
modif <- Data[, .I[sample(.N, nError)]] # .I = seq_len(nrow(Data)), .N = nrows in the group -> sample 2 rows number in Data
Data[modif, DBH := 0]
# Data[modif] # to check


#### Size with bad precision (pas .0 ou .5) ####

wrong <- c(0.2, 0.3, 0.569, 0.8)
modif <- Data[, .I[sample(.N, nError)]]
Data[modif, DBH := DBH + sample(wrong,1)]
# Data[modif] # to check


#### Resurrected tree ####

Last_census <- Data[CensusYear == 2020]
Previous_census <- Data[CensusYear == 2019]

# See if a resurrected tree already exists
MortPrev <- Previous_census[LifeStatus == 0 & IdTree %in% Last_census[LifeStatus == 1, IdTree], IdTree]
# dead in 2019, alive in 2020

Previous_census[IdTree == 101410] # dead
Last_census[IdTree == 101410] # alive


#### Missing life status ####

modif <- Data[, .I[sample(.N, nError)]] # .I = seq_len(nrow(Data)), .N = nrows in the group -> sample 2 rows number in Data
Data[modif, LifeStatus := NA]
Data[modif] # to check


#### Duplicated TreeFieldNum in plot-subplot association ####

modif <- Data[, .I[sample(.N, 1)]] # 1 row to change
duplicatedFieldNum <- Data[!(row.names(Data)) %in% modif & # rows != modif
                             Plot == Data[modif, Plot] & # same plot as modif
                             SubPlot == Data[modif, SubPlot], # same subplot as modif
                           sample(TreeFieldNum,1)] # 1 TreeFieldNum to duplicate

Data[modif, TreeFieldNum := duplicatedFieldNum] # on the row to modif, we duplicate the TreeFieldNum
# Data[TreeFieldNum == duplicatedFieldNum] # to check


#### Unseen tree but alive tree after ####

#### Duplicated IdTree in a census ####

idModif <- Last_census[, sample(IdTree, nError)] # ne chercher que dans ceux qui ne sont pas à verifier pour eviter de prendre certains avec un duplicatedID # selectionner 2 IdTree à modifier

duplicatedID <- Last_census[!(IdTree %in% idModif), sample(IdTree, 1)] # IdTree != modif

Data[IdTree %in% idModif, IdTree := duplicatedID] # we duplicate the IdTree on the previous selected IdTree

Data[CensusYear == 2020 & IdTree == duplicatedID] # to check



#### Abnomal growth ####
#### Abnomal recruit ####

#### Save this test data in the package ####
TestData <- Data

usethis::use_data(TestData, overwrite = TRUE)

