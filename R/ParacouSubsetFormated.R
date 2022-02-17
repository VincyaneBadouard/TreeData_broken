#' ParacouSubsetFormated
#'
#' This is the standardized, formated version of ParacouSubset (see help(ParacouSubset))
#'
#' @format A tibble
#' \describe{
#'   x
#'   \item{Site}{Site level name}
#'   \item{Plot}{Plot level name}
#'   \item{SubPlot}{SubPlot level name}
#'   \item{TreeFieldNum}{Tree unique identifiers, matching the tag number in the field if that column was provided in the input, automatically generated if not.}
#'   \item{IdTree}{Tree unique identifiers. This was automatically generated if not provided in the input data}
#'   \item{Xfield}{Tree X euclidean position in plot in m}
#'   \item{Yfield}{Tree Y euclidean position in plot in m}
#'   \item{Xutm}{X UTM coordinates in m}
#'   \item{Yutm}{Y UTM coordinats in m}
#'   \item{Lat}{Latitude in degrees}
#'   \item{Lon}{Longitude in degrees}
#'   \item{Family}{Family name}
#'   \item{Genus}{Genus name}
#'   \item{Species}{Species name}
#'   \item{VernName}{Vernacular name}
#'   \item{CommercialSp}{Logical: TRUE if species is considered commercial, FALSE if not}
#'   \item{CensusYear}{Census Year}
#'   \item{LifeStatus}{Logical: TRUE is tree was alive, FALSE if it was dead}
#'   \item{Circ}{circumference in cm}
#'   \item{IdStem}{Stem unique identifier}
#'   \item{IdMeasure}{measure unique identifier if you had more than one measure per stem per year}
#'   \item{DBH}{Diameter at breast height in cm}
#'   \item{POM}{Height of measurement in m}
#'   \item{ScientificName}{Scientific name}
#'   \item{TreeHeight}{Tree Height in m}
#'   \item{LifeForm}{Life form}
#'   ...
#' }

"ParacouSubsetFormated"