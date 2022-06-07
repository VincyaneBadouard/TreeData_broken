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
      DateFormat = "yyyy-mm-dd",
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
      IsCommercrial = "TRUE",
      IsLive = "TRUE",
      Lat = "Lat",
      LifeForm = "none",
      LifeStatus = "CodeAlive",
      Lon = "Lon",
      Plot = "Plot",
      PlotArea = "PlotArea",
      PlotAreaMan = NA,
      PlotAreaUnitMan = "ha",
      PlotMan = "",
      plotUnitMan = "m",
      POM = "MeasCode",
      ScientificName = "none",
      ScientificNameSep = "",
      Site = "Forest",
      SiteMan = "",
      Species = "Species",
      SubPlot = "SubPlot",
      SubPlotArea = "none",
      SubPlotAreaMan = NA,
      SubPlotAreaUnitMan = "none",
      SubPlotMan = "",
      subplotUnitMan = "none",
      TickedMelt1 = FALSE,
      TickedMelt2 = FALSE,
      TickedMelt3 = FALSE,
      Tidy = structure(0L, class = c("integer",
                                     "shinyActionButtonValue")),
      TreeFieldNum = "TreeFieldNum",
      TreeHeight = "none",
      TreeHeightUnitMan = "none",
      utmUnitMan = "m",
      ValueName1 = "field",
      ValueName2 = "utm",
      ValueName3 = "L",
      Variablecolumns1 = c("Xfield", "Yfield"),
      Variablecolumns2 = c("Xutm",
                           "Yutm"),
      Variablecolumns3 = c("Lat", "Lon"),
      VernName = "VernName",
      Xplot = "Xfield",
      Xsubplot = "none",
      Xutm = "Xutm",
      Xutm = "Xutm",
      Year = "CensusYear",
      YearMan = NA,
      Yfield = "Yfield",
      Yplot = "Yfield",
      Ysubplot = "none",
      Yutm = "Yutm"
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
