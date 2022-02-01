## code to prepare `ParacouProfile` dataset goes here

## 1st option to create (non-interactive) - may need to be updated if interactive_items.csv changes

if(!interactive()) {
ParacouProfile <- list(
  Site = "Forest",
  Plot = "Plot",
  Subplot = "SubPlot",
  CensusYear = "CensusYear",
  TreeFieldNum = "TreeFieldNum",
  IdTree = "idTree",
  IdStem = "none",
  IdMeasure = "none",
  LifeStatus = "CodeAlive",
  DBH = "none",
  DBHUnitCol = "none",
  Circ = "Circ",
  CircUnitCol = "none",
  POM = "none",
  POMUnitCol = "none",
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
  TreeHeightUnitCol = "none",
  LifeForm = "none",
  Treatment = "none",
  SoilType = "none",
  Topography = "none",
  CircUnit = "cm"
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
7

}

#### Save this data in the package ####
usethis::use_data(ParacouProfile, overwrite = TRUE)
