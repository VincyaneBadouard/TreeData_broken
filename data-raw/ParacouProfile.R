## code to prepare `ParacouProfile` dataset goes here

## 1st option to create (non-interactive) - may need to be updated if interactive_items.csv changes

if(!interactive()) {
  ParacouProfile <- list(
    Site = "Forest",
    Plot = "Plot",
    SubPlot = "SubPlot",
    CensusYear = "CensusYear",
    CensusDate = "none",
    TreeFieldNum = "TreeFieldNum",
    IdTree = "idTree",
    IdStem = "none",
    IdMeasure = "none",
    LifeStatus = "CodeAlive",
    DBH = "none",
    DBHUnit = "none",
    Circ = "Circ",
    CircUnit = "none",
    POM = "none",
    POMUnit = "none",
    Lat = "Lat",
    Lon = "Lon",
    Xutm = "Xutm",
    Yutm = "Yutm",
    Xfield = "Xfield",
    Yfield = "Yfield",
    ScientificName = "none",
    VernName = "VernName",
    Family = "Family",
    Genus = "Genus",
    Species = "Species",
    CommercialSp = "CommercialSp",
    TreeHeight = "none",
    TreeHeightUnit = "none",
    LifeForm = "none",
    Treatment = "none",
    SoilType = "none",
    Topography = "none",
    CircUnitMan = "cm"
  )

}
## 2nd option (interactive) - may need to be updated if interactive_items.csv changes

if(interactive()) {
  data("ParacouSubset")

  ParacouProfile <- RequiredFormat_interactive(ParacouSubset)

"ok" #
2
3
5
24
1
6
7
1
1
27
1
1
29
1
1
1
14
15
11
12
9
10
1
22
16
17
18
23
1
1
1
1
1
1
3

}

#### Save this data in the package ####
usethis::use_data(ParacouProfile, overwrite = TRUE)


## For ParacouProfile.Rmd  run next line of code and paste in the item section of R/ParacouProfile.R
write.csv(
  paste0("#'   \\item{", names(ParacouProfile), "}{The column name in datda set @ParacouSubset (", ParacouProfile, ") corresponding to ", x$Label[match(names(ParacouProfile), x$ItemID)], "}"), "clipboard",
  quote = F, row.names = F)

