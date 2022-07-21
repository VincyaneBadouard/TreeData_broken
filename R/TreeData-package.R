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
utils::globalVariables(c("Circ", "Diameter", "MinDBH",
                         "Site", "IdTree", "IdStem", "StemFieldNum",
                         "POM", "HOM", "Plot",
                         "SubPlot", "TreeFieldNum", "TreeHeight",
                         "CensusYear", "Date", "CensusDate", "Month", "Day", "IdCensus",
                         "LifeStatus", "PlotArea", "Year", "Time",
                         "SubPlotArea", "CommercialSp",
                         "Family", "VernName",  "Genus", "Species", "ScientificName",

                         "Xutm", "Yutm", "Xplot", "Yplot", "Xsubplot", "Ysubplot",

                         "DateOriginal", "CensusDateOriginal",
                         "LifeStatusOriginal", "CommercialSpOriginal",

                         "NewIdTree",

                         "BD", "BCirc", "BPOM", "BHOM",

                         ".", ".N", ".SD", "..ColumnsToReturn",

                         "LifeStatusCor", "PlotSubNum", "SitYearID", "Comment",
                         "CorrectedRecruit", "DBHCor", "DiameterCorrectionMeth", "Cresc",
                         "HOMCor",
                         "GenusCor", "SpeciesCor", "ScientificNameCor", "FamilyCor", "VernNameCor",
                         "New.Genus", "New.Species", "Taxonomic.status", "Typo",
                         "Taxon", "GenspFamily",
                         "BotanicalCorrectionSource", "FamilyCorSource"
))

## usethis namespace: end
NULL
