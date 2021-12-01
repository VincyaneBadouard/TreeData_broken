#' BarroColoradoSubset
#'
#' Subset of the tree inventory of Barro Colorado 50-ha plot.
#' - 3 quadrats (1, 2, 3)
#' - 1995-2015
#' - 5 last censuses
#' Dataset extracted from the Barro Colorado 50-ha plot database.
#'
#' @format A tibble with 3829 rows and 24 variables:
#' \describe{
#'   \item{Family}{Following (mostly) the angiosperm classification at
#'   http://www.mobot.org/MOBOT/research/APweb/ (character)}
#'   \item{Genus}{Taxonomic genus (character)}
#'   \item{SpeciesName}{Taxonomic species (character)}
#'   \item{Mnemonic}{The species mnemonic (character)}
#'   \item{Subspecies}{Infraspecific name (blank in most cases because no
#'   subspecies name is used) (character)}
#'   \item{SpeciesID}{The unique species identifier in the database.
#'   Guarantees a species match (numeric)}
#'   \item{SubspeciesID}{The unique subspecies identifier in the database.
#'   Guarantees a subspecies match (numeric)}
#'   \item{QuadratName}{Quadrat designation, as a 2-digit row number then
#'   2-digit column number on a 20x20 m grid (numeric)}
#'   \item{QuadratID}{Quadrat unique identifier (numeric)}
#'   \item{PX}{The x coordinate within the plot, meters from the west border of
#'   the plot, always in [0,1000) (numeric)}
#'   \item{PY}{The y coordinate, meters from the south border,
#'   always in [0,500) (numeric)}
#'   \item{TreeID}{The unique tree identifier in the database.
#'   Guarantees a tree match (numeric)}
#'   \item{Tag}{Tag number on the tree (occasionally negative where a tag was
#'   duplicated by mistake) (numeric)}
#'   \item{StemID}{The unique stem identifier in the database.
#'   Guarantees a stem match (numeric)}
#'   \item{StemTag}{Tag number on the individual stem, if present (character)}
#'   \item{PlotCensusNumber}{Census number 1-8 (numeric)}
#'   \item{DBH}{Diameter (mm) of one stem on the tree,
#'   the stem whose stemID is given (numeric)}
#'   \item{HOM}{The height-of-measure, meters above the ground,
#'   where the dbh was measured (numeric)}
#'   \item{ExactDate}{The date on which a tree was measured (date)}
#'   \item{Date}{Integer date for easy calculation of time interval between
#'   censuses (the number of days since 1 Jan 1960) (numeric)}
#'   \item{Year}{The year on which a tree was measured (numeric)}
#'   \item{ListOfTSM}{The codes describing the measurement as recorded in the
#'   field. See Condit (1998) for a description of field codes. For analyses,
#'   status codes should be used, not field codes (character)}
#'   \item{HighHOM}{ A flag, with 1 indicating this hom is at the highest
#'   position on the stem, otherwise 0 (numeric)}
#'   \item{Status}{A status code for the tree or stem. (character)
#'   A: Alive, applied to either the tree or stem.
#'   D: Dead, meaning the entire tree is dead, ie with no living stems or sprouts.
#'   P: Prior, applied to a tree (or stem) in censuses prior to its appearance
#'   in the plot.
#'   M: Missing, indicating that the tree was skipped by mistake during the
#'   census, so it could have been alive or dead.
#'   AD: A seldom-used code, applied when a tree was noted as dead in one census
#'   but was found alive in a later census. For most purposes, this code should
#'   be interpreted the same as code A for alive.
#'   AR: A similar unusual code, applied when a stem was noted as broken in one
#'   census but intact in a later census. For most purposes, this code should be
#'   interpreted the same as code A for alive.
#'   G: Gone, for stems only, meaning the stem broke or otherwise disappeared,
#'   but other stems on the tree were alive and measured.
#'   V: Vanished, for stems only. This is needed in the BCI plot because stem
#'   tags were not used before the 2010 census. It is often the case where a
#'   tree had 2 or more stems that were, after 5 years, impossible to match.
#'   When this was true, new stems with new stemIDs and thus new rows in the
#'   stem table were created. Earlier stems were given status=V.
#'   In most cases, V means the stems persisted but could not be tracked.}
#'   ...
#' }
#' @source Condit, Richard et al. (2019), Complete data from the Barro Colorado
#' 50-ha plot: 423617 trees, 35 years, Dryad, Dataset,
#' https://doi.org/10.15146/5xcp-0d46 ;
#' \url{https://datadryad.org/stash/dataset/doi:10.15146/5xcp-0d46}
#'
"BarroColoradoSubset"
