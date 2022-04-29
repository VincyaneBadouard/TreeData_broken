## code to prepare `ForestGEOProfile` dataset goes here

ForestGEOProfile <-
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
    DateFormat = "mm/dd/yyyy",
    IdCensus = "CensusID",
    PlotAreaMan = 25.6,
    Year = "none",
    LifeStatus = "DFstatus",
    PlotArea = "none",
    CommercialSp = "none",
    SubPlotAreaUnitMan = "none",
    YearMan = NA,
    Yutm = "none",
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
    POMUnitMan = "m"
  )

usethis::use_data(ForestGEOProfile, overwrite = TRUE)


## For ForestGEOProfile.Rmd  run next line of code and paste in the item section of R/ForestGEOProfile.R
x <- read.csv("inst/app/data/interactive_items.csv")

write.csv(
  paste0("#'   \\item{", names(ForestGEOProfile), "}{Value or column name in data set @ForestGEOProfile (", ForestGEOProfile, ") corresponding to ", x$Label[match(names(ForestGEOProfile), x$ItemID)], "}"), "clipboard",
  quote = F, row.names = F)
