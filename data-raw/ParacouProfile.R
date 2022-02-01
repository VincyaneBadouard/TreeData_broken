## code to prepare `ParacouProfile` dataset goes here

## 1st option to create (non-interactive) - may need to be upadted if interactive_items.csv changes

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
## 2nd option (interactive) - may need to be upadted if interactive_items.csv changes

if(interactive()) {
  data("ParacouSubset")

  ParacouProfile <- RequiredFormat_interactive(ParacouSubset)
2 # Site
3 # Plot
5 # Subplot
24 # CensusYear
6 # TreeFieldNum
7 # IdTree
1 # IdStem
1 # IdMeasure
27 # LifeStatus
1 # DBH
1 # DBHUnitCol
29 # Circ
1 # CurcUnitCol
1 # POM
1 # POMUnitCol
14 # Lat
15 # Lon
11 # Xutm
12 # Yutm
9 # Xfield
10 # Yfield
1 # ScientificName
22 # VernName
16 # Family
17 # Genus
18 # Species
23 # CommercialSp
1 # TreeHeight
1 # TreeHeightUnitCol
1 # LifeForm
1 # Treatment
1 # SoilType
1 # Topography
7 # CircUnit
}

#### Save this data in the package ####
usethis::use_data(ParacouProfile, overwrite = TRUE)
