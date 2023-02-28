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



                         "BD", "BCirc", "BPOM", "BHOM", "site",

                         ".", ".N", ".SD", ".NATURAL", "..ColumnsToReturn", "..ByCols",
                         "rn", "value", "LifeForm",

                         "IDYear", "V1", "V2", "MaxHOM", "MaxDBH", "MaxDate", "POMChange",

                         "LifeStatusCor", 'LifeStatus_TreeDataCor', "Comment_TreeData",
                         "CorrectedRecruit",
                         "DBHCor", "Diameter_TreeDataCor", "DiameterCorrectionMeth_TreeData", "Cresc",
                         "HOM_TreeDataCor", "POM_TreeDataCor",
                         "GenusCor", "SpeciesCor", "ScientificNameCor", "FamilyCor", "VernNameCor",
                         "New.Genus", "New.Species", "Taxonomic.status", "Typo", "family",
                         "Taxon", "GenspFamily", "taxonomicStatus", "spec.name", "scientificName",
                         "Old.status", "spec.name.ORIG", "BotaCorSource",
                         "BotanicalCorrectionSource", "FamilyCorSource",
                         "raised", "InvariantColumns"
))

## usethis namespace: end
NULL
