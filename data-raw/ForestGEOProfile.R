## code to prepare `ForestGeoProfile` dataset goes here

ForestGeoProfile <-
  list(
    TreeHeightUnitMan = "none",
    Family = "Family",
    TreeHeight = "none",
    Date = "ExactDate",
    POM = "hom",
    PlotAreaUnitMan = "none",
    Xutm = "none",
    IdStem = "stemID",
    PlotMan = "ForestGEO",
    Species = "Species",
    ScientificNameSep = "",
    Yfield = "gy",
    CircUnitMan = "none",
    IdMeasure = "MeasureID",
    IdLevel = "IDLevel",
    TreeFieldNum = "tag",
    ScientificName = "Latin",
    DateFormat = "yyyy-mm-dd",
    IdCensus = "CensusID",
    PlotAreaMan = 25.6,
    Year = "none",
    LifeStatus = "DFstatus",
    PlotArea = "none",
    CommercialSp = "none",
    SubPlotAreaUnitMan = "none",
    Yutm = "none",
    YearMan = NA,
    Circ = "none",
    SubPlotAreaMan = 0.04,
    SubPlotMan = "",
    Site = "none",
    Lon = "none",
    Plot = "none",
    IdTree = "treeID",
    LifeForm = "none",
    VernName = "none",
    SubPlotArea = "none",
    SiteMan = "SCBI",
    IsLive = c("alive", "broken below"),
    Authority = "SpeciesID",
    Lat = "none",
    Genus = "Genus",
    SubPlot = "quadrat",
    DBHUnitMan = "mm",
    Xfield = "gx",
    DBH = "dbh",
    POMUnitMan = "m",
    Tidy = structure(0L, class = c("integer",
                                   "shinyActionButtonValue")),
    ClearValueName = structure(0L, class = c("integer",
                                             "shinyActionButtonValue")),
    TickedMelt3 = FALSE,
    Variablecolumns1 = c("sp",
                         "gx", "gy"),
    ValueName4 = "Specis",
    TickedMelt1 = FALSE,
    TickedMelt4 = FALSE,
    Variablecolumns3 = c("DFstatus", "status"),
    ValueName3 = "stau",
    TickedMelt2 = FALSE,
    Variablecolumns2 = c("pom",
                         "hom"),
    ValueName2 = "om",
    Variablecolumns4 = c("Species",
                         "SpeciesID"),
    ValueName1 = ""
  )
usethis::use_data(ForestGeoProfile, overwrite = TRUE)


## For ForestGeoProfile.Rmd  run next line of code and paste in the item section of R/ForestGeoProfile.R
x <- read.csv("inst/app/data/interactive_items.csv")


write.csv(
  paste0(
    "#'   \\item{",
    names(ForestGeoProfile),
    "}",  ifelse(is.na(x$Label[match(names(ForestGeoProfile), x$ItemID)]), "{Some value entered via interaction with the Shiny app", paste0("{Value or column name in data set @ForestGeoSubset (", ForestGeoProfile, ") corresponding to ",
                                                                                                                                          x$Label[match(names(ForestGeoProfile), x$ItemID)])),
    "}"),
  "clipboard",
  quote = F,
  row.names = F
)

