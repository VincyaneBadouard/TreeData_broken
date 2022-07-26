#' TreeData-package
#'
#' Forest Inventories Harmonization & Correction
#'
#' @name TreeData
#' @docType package
#'
#' @section TreeData functions:
#' RequiredFormat
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
### quiets concerns of R CMD check "no visible binding for global variables"
utils::globalVariables(c("Site", "Plot", "SubPlot", "PlotArea", "SubPlotArea",
                         "Xutm", "Yutm", "Xplot", "Yplot", "Xsubplot", "Ysubplot", "PlotSubNum",

                         "CensusYear", "Date", "CensusDate", "Month", "Day", "IdCensus",
                         "Year", "Time", "DateOriginal", "CensusDateOriginal", "SitYearID",

                         "IdTree", "IdStem", "StemFieldNum", "TreeFieldNum", "NewIdTree",

                         "Circ", "Diameter", "MinDBH", "TreeHeight",
                         "POM", "HOM",


                         "Family", "VernName",  "Genus", "Species", "ScientificName",


                         "LifeStatus", "CommercialSp", "LifeStatusOriginal", "CommercialSpOriginal",



                         "BD", "BCirc", "BPOM", "BHOM",

                         ".", ".N", ".SD", "..ColumnsToReturn",

                         "LifeStatusCor", "Comment",
                         "CorrectedRecruit", "DBHCor", "DiameterCorrectionMeth", "Cresc",
                         "HOMCor",
                         "GenusCor", "SpeciesCor", "ScientificNameCor", "FamilyCor", "VernNameCor",
                         "New.Genus", "New.Species", "Taxonomic.status", "Typo", "family",
                         "Taxon", "GenspFamily", "taxonomicStatus", "spec.name", "scientificName",
                         "BotanicalCorrectionSource", "FamilyCorSource"
))

## usethis namespace: end
NULL
