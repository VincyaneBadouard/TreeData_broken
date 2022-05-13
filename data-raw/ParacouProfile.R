## code to prepare `ParacouProfile` dataset goes here

## 1st option to create (non-interactive) - may need to be updated if interactive_items.csv changes

if (!interactive()) {
  ParacouProfile <-
    list(
      TreeHeightUnitMan = "none",
      Family = "Family",
      TreeHeight = "none",
      Date = "CensusDate",
      POM = "none",
      PlotAreaUnitMan = "ha",
      Xutm = "Xutm",
      IsCommercrial = "TRUE",
      IdStem = "none",
      PlotMan = "",
      Species = "Species",
      ScientificNameSep = "",
      Yfield = "Yfield",
      CircUnitMan = "cm",
      IdMeasure = "none",
      IdLevel = "none",
      TreeFieldNum = "TreeFieldNum",
      ScientificName = "none",
      DateFormat = "yyyy-mm-dd",
      IdCensus = "none",
      PlotAreaMan = NA,
      Year = "CensusYear",
      LifeStatus = "CodeAlive",
      PlotArea = "PlotArea",
      CommercialSp = "CommercialSp",
      SubPlotAreaUnitMan = "none",
      Yutm = "Yutm",
      YearMan = NA,
      Circ = "Circ",
      SubPlotAreaMan = NA,
      SubPlotMan = "",
      Site = "Forest",
      Lon = "Lon",
      Plot = "Plot",
      IdTree = "idTree",
      LifeForm = "none",
      VernName = "VernName",
      SubPlotArea = "none",
      SiteMan = "",
      IsLive = "TRUE",
      Authority = "none",
      Lat = "Lat",
      Genus = "Genus",
      SubPlot = "SubPlot",
      DBHUnitMan = "none",
      Xfield = "Xfield",
      DBH = "none",
      POMUnitMan = "none",
      Tidy = structure(0L, class = c("integer",
                                     "shinyActionButtonValue")),
      ClearValueName = structure(0L, class = c("integer",
                                               "shinyActionButtonValue")),
      TickedMelt3 = FALSE,
      Variablecolumns1 = c("Xfield",
                           "Yfield"),
      TickedMelt1 = FALSE,
      Variablecolumns3 = c("Lat",
                           "Lon"),
      ValueName3 = "L",
      TickedMelt2 = FALSE,
      Variablecolumns2 = c("Xutm",
                           "Yutm"),
      ValueName2 = "utm",
      ValueName1 = "field"
    )

}
# ## 2nd option (interactive) - may need to be updated if interactive_items.csv changes
#
# if (interactive()) {
#   data("ParacouSubset")
#
#   ParacouProfile <- RequiredFormat_interactive(ParacouSubset)
#
#   "ok" #
#   2
#   3
#   5
#   4
#   24
#   25
#   6
#   7
#   1
#   1
#   27
#   1
#   1
#   29
#   1
#   1
#   1
#   14
#   15
#   11
#   12
#   9
#   10
#   1
#   22
#   16
#   17
#   18
#   23
#   1
#   1
#   1
#   1
#   1
#   1
#   2
#   2
#   yyyy - mm - dd
#   2
#   2
# }
#
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
