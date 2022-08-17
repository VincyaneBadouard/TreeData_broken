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
utils::globalVariables(c("Site", "Plot", "Subplot", "PlotArea", "SubplotArea", "PlotSubNum",
                         "XTreeUTM", "YTreeUTM", "XTreePlot", "YTreePlot", "XTreeSubplot", "YTreeSubplot",

                         "CensusYear", "Date", "CensusDate", "Month", "Day", "IdCensus",
                         "Year", "Time", "DateOriginal", "CensusDateOriginal", "SitYearID",

                         "IdTree", "IdStem", "StemFieldNum", "TreeFieldNum", "NewIdTree",

                         "Circ", "Diameter", "MinDBH", "TreeHeight",
                         "POM", "HOM",


                         "Family", "VernName", "Genus", "Species", "Subspecies", "ScientificName",


                         "LifeStatus", "CommercialSp", "LifeStatusOriginal", "CommercialSpOriginal",



                         "BD", "BCirc", "BPOM", "BHOM",

                         ".", ".N", ".SD", ".NATURAL", "..ColumnsToReturn",

                         "IDYear", "V1", "MaxHOM", "MaxDate",

                         "LifeStatusCor", "Comment",
                         "TaperCorDBH",
                         "CorrectedRecruit", "DBHCor", "DiameterCorrectionMeth", "Cresc",
                         "HOMCor", "POMcor",
                         "GenusCor", "SpeciesCor", "ScientificNameCor", "FamilyCor", "VernNameCor",
                         "New.Genus", "New.Species", "Taxonomic.status", "Typo", "family",
                         "Taxon", "GenspFamily", "taxonomicStatus", "spec.name", "scientificName",
                         "Old.status", "spec.name.ORIG", "BotaCorSource",
                         "BotanicalCorrectionSource", "FamilyCorSource"
))

## usethis namespace: end
NULL
