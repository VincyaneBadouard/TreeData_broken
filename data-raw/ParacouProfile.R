## code to prepare `ParacouProfile` dataset goes here

## 1st option to create (non-interactive) - may need to be updated if interactive_items.csv changes

if (!interactive()) {
  ParacouProfile <-
    list(
      Authority = "none",
      BCirc = "none",
      BCircUnitMan = "none",
      BD = "none",
      BDUnitMan = "none",
      Circ = "Circ",
      CircUnitMan = "cm",
      ClearValueName = structure(0L, class = c("integer",
                                               "shinyActionButtonValue")),
      CommercialSp = "CommercialSp",
      Date = "CensusDate",
      Month = "none",
      Day = "none",
      DateFormatMan = "yyyy-mm-dd",
      Diameter = "none",
      DiameterUnitMan = "none",
      Family = "Family",
      Genus = "Genus",
      HOM = "none",
      HOMUnitMan = "none",
      IdCensus = "none",
      IdLevel = "none",
      IdMeasure = "none",
      IdStem = "none",
      IdTree = "idTree",
      IsCommercialMan = "TRUE",
      IsLive = "TRUE",
      TreeLat = "Lat",
      LifeForm = "none",
      LifeStatus = "CodeAlive",
      TreeLon = "Lon",
      MinDBH = "none",
      MinDBHMan = -999,
      MinDBHUnitMan = "none",
      Plot = "Plot",
      PlotArea = "PlotArea",
      PlotAreaMan = -999,
      PlotAreaUnitMan = "ha",
      PlotMan = "",
      TreePlotUnitMan = "m",
      POM = "MeasCode",
      ScientificName = "none",
      ScientificNameSepMan = "",
      Site = "Forest",
      SiteMan = "",
      Species = "Species",
      SpeciesDensity = "none",
      SpeciesDensityunitMan = "none",
      SpeciesBasalAreaDensity = "none",
      SpeciesBasalAreaDensityUnitMan = "none",
      SpeciesTotalBasalArea = "none",
      SpeciesTotalBasalAreaUnitMan = "none",
      SpeciesVolume = "none",
      SpeciesVolumeUnitMan = "none",
      Subplot = "SubPlot",
      SubplotArea = "none",
      SubplotAreaMan = 1,
      SubplotAreaUnitMan = "none",
      SubplotMan = "",
      TreeSubplotUnitMan = "none",
      TickedMelt1 = FALSE,
      TickedMelt2 = FALSE,
      TickedMelt3 = FALSE,
      Tidy = structure(0L, class = c("integer",
                                     "shinyActionButtonValue")),
      TreeCodes = "none",
      TreeFieldNum = "TreeFieldNum",
      TreeHeight = "none",
      TreeHeightUnitMan = "none",
      TreeUTMUnitMan = "m",
      ValueName1 = "field",
      ValueName2 = "utm",
      ValueName3 = "L",
      Variablecolumns1 = c("Xfield", "Yfield"),
      Variablecolumns2 = c("Xutm",
                           "Yutm"),
      Variablecolumns3 = c("Lat", "Lon"),
      VernName = "VernName",
      XTreePlot = "Xfield",
      XTreeSubplot = "none",
      XTreeUTM = "Xutm",
      Year = "CensusYear",
      YearMan = -999,
      YTreePlot = "Yfield",
      YTreeSubplot = "none",
      YTreeUTM = "Yutm"
    )

}

# dput(ParacouProfile)

#### Save this data in the package ####
usethis::use_data(ParacouProfile, overwrite = TRUE)


## For ParacouProfile.Rmd  run next line of code and paste in the item section of R/ParacouProfile.R
x <- read.csv("inst/app/data/interactive_items.csv")

write.csv(
  paste0(
    "#'   \\item{",
    names(ParacouProfile),
    "}",  ifelse(is.na(x$Label[match(names(ParacouProfile), x$ItemID)]), "{Some value entered via interaction with the Shiny app", paste0("{Value or column name in data set @ParacouSubset (", ParacouProfile, ") corresponding to ",
    x$Label[match(names(ParacouProfile), x$ItemID)])),
    "}"),
  "clipboard",
  quote = F,
  row.names = F
)
