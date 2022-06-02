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
utils::globalVariables(c("CensusYear", "Circ", "Diameter", "Genus", "Site", "IdTree",
                         "POM", "HOM", "Plot", "Species", "ScientificName",
                         "SubPlot", "TreeFieldNum", "TreeHeight", "Date",
                         "CensusDate", "LifeStatus", "PlotArea", "Year", "Time",
                         "SubPlotArea", "CommercialSp", "Xutm", "Yutm", "Family", "VernName",

                         "Xplot", "Yplot", "Xsubplot", "Ysubplot",

                         "DateOriginal", "CensusDateOriginal",
                         "LifeStatusOriginal", "CommercialSpOriginal",

                         "NewIdTree",

                         "BD", "BCirc", "BPOM", "BHOM",

                         ".", ".N", ".SD", "..ColumnsToReturn",

                         "LifeStatusCor", "PlotSubNum", "SitYearID", "Comment",
                         "CorrectedRecruit", "DBHCor", "DiameterCorrectionMeth", "Cresc"
))

## usethis namespace: end
NULL
