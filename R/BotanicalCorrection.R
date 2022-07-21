#' Botanical Correction
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdTree` (character)
#'   - `Family` (character)
#'   - `Genus` (character)
#'   - `Species` (character)
#'   - `VernName` (character)
#'   - `ScientificName` (character)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (Default: FALSE) (logical)
#'
#' @return Fill the *Comment* column with error type informations. If
#'   *DetectOnly* = FALSE, add columns:
#'   - `FamilyCor` (character): corrected Family name
#'   - `FamilyCorSource` (character): source of the Family correction
#'   - `GenusCor` (character): corrected Genus name
#'   - `SpeciesCor` (character): corrected Species name
#'   - `BotanicalCorrectionSource` (character): source of the Genus and Species
#'       correction
#'   - `ScientificNameCor` (character): corrected Scientific name
#'   - `VernNameCor` (character): completed if information available at `IdTree`
#'       level.
#'
#'@importFrom Taxonstand TPL
#'@importFrom BIOMASS getTaxonomy
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#'
#' Rslt <- BotanicalCorrection(TestData)
#'
#' ScfcCor <- unique(Rslt[ScientificNameCor != ScientificName,
#'                 list(ScientificName, ScientificNameCor,
#'                 Family, FamilyCor, FamilyCorSource,
#'                 Genus, GenusCor,
#'                 Species, SpeciesCor,
#'                 BotanicalCorrectionSource, Comment)
#'                 ])
#'
#' FamCor <- unique(Rslt[FamilyCor != Family,
#'                 list(ScientificName, ScientificNameCor,
#'                 Family, FamilyCor, FamilyCorSource,
#'                 Genus, GenusCor,
#'                 Species, SpeciesCor,
#'                 BotanicalCorrectionSource, Comment)
#'                 ])
#'
BotanicalCorrection <- function(
  Data,
  DetectOnly = FALSE
){

  #### Arguments check ####
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # DetectOnly (logical)
  if(!inherits(DetectOnly, "logical"))
    stop("The 'DetectOnly' argument must be a logical")


  #### Function ####

  setDT(Data) # data.frame to data.table


  # Corrected columns initialisation --------------------------------------------------------------------------------------
  Data[, GenusCor := Genus]
  Data[, SpeciesCor := Species]

  # Missing value ---------------------------------------------------------------------------------------------------------
  # Family, ScientificName/Genus, species, VernName

  Vars <- c("Family", "ScientificName", "Genus", "Species", "VernName")

  for (v in 1:length(Vars)) {

    if(Vars[v] %in% names(Data)){ # If the column exists

      Data <- GenerateComment(Data,
                              condition = is.na(Data[,get(Vars[v])]),
                              comment = paste0("Missing value in ", Vars[v]))
    }
  }

  # Data[Comment != ""] # to check

  # Special characters: remove : !"#$%&’()*+,-./:;<=>?@[]^_`{|}~ ----------------------------------------------------------

  Data[, GenusCor := gsub("[[:punct:]]", "", Data$GenusCor)]
  Data[, SpeciesCor := gsub("[[:punct:]]", "", Data$SpeciesCor)]

  ## Comment :

  Data <- GenerateComment(Data,
                          condition = grepl('[[:punct:]]', Data$Genus), # TRUE if there are any special character
                          comment = "Special characters in the 'Genus'")

  Data <- GenerateComment(Data,
                          condition = grepl('[[:punct:]]', Data$Species), # TRUE if there are any special character
                          comment = "Special characters in the 'Species'")

  # No family name in the genus and species columns -----------------------------------------------------------------------
  # (detection of the suffix "aceae" in the genus and species columns (it is specific to the family name)
  Data[, GenspFamily := NA_character_]

  Data[grep("aceae", Data$Genus), `:=`(GenspFamily = ifelse(is.na(Family), Genus, GenspFamily), GenusCor = NA_character_)]
  Data[grep("aceae", Data$Species), `:=`(GenspFamily = ifelse(is.na(Family), Species, GenspFamily), SpeciesCor = NA_character_)]

  ## Comment :
  Data <- GenerateComment(Data,
                          condition = grepl("aceae", Data$Genus) | grepl("aceae", Data$Species),
                          comment = "Names ending in 'aceae' cannot be genus or species names")


  # Orthographical error ------------------------------------------------------------------------------------------------
  Data[, ScientificNameCor := paste(GenusCor, SpeciesCor)]

  ## with Scientific name (25 var)
  TPLCor <- Taxonstand::TPL(unique(Data$ScientificNameCor), corr = TRUE, diffchar = 20, max.distance = 1) # diffchar: maximum difference of characters nbr between input and output
  # with Genus and species marche pas bien pcq décale genre et sp quand on unique())

  setDT(TPLCor) # df to dt

  # Take only corrected names. Columns: New.Genus, New.Species, Typo. Not Family because it is outdated.
  TPLCor <- TPLCor[New.Genus != Genus | New.Species != Species,]
  TPLCor <- TPLCor[, list(Taxonomic.status, Typo, Taxon, New.Genus, New.Species)]
  TPLCor[, BotanicalCorrectionSource := "The Plant List"] # create the Source


  # Join the corrected Genus and Species, by original 'ScientificNameCor'
  Data <- merge(Data, TPLCor, by.x = "ScientificNameCor", by.y = "Taxon", all.x = TRUE) #  by.x = "ScientificNameCor", by.y = "Taxon", sort = FALSE

  # Update correction columns
  Data[, GenusCor := ifelse(!is.na(New.Genus), New.Genus, GenusCor)]
  Data[, SpeciesCor := ifelse(!is.na(New.Species), New.Species, SpeciesCor)]


  # Comment:
  ## if "Synonym" :
  Data <- GenerateComment(Data,
                          condition = Data$Taxonomic.status == "Synonym",
                          comment = "'ScientificName' is a synonym of the accepted botanical name")
  ## if Typo == TRUE :
  Data <- GenerateComment(Data,
                          condition = Data$Typo == TRUE,
                          comment = "Spelling error in the 'ScientificName'")

  # Remove columns that have become useless
  Data[, c("Taxonomic.status", "Typo", "New.Genus", "New.Species") := NULL]


  # Family & Scientific names match -------------------------------------------------------------------------------------
  # Recovering the Family name by Genus
  # (*BIOMASS::getTaxonomy*) with APG III family

  FamilyData <-
    setDT( # as data.table
      BIOMASS::getTaxonomy(unique(Data$GenusCor), findOrder = FALSE)
    )

  FamilyData <- setnames(FamilyData, "family", "FamilyCor") # rename columns


  Data <- merge(Data, FamilyData, by.x = "GenusCor", by.y = "inputGenus",  all.x = TRUE, sort = FALSE)

  # Generate a comment if the family name is incorrect
  Data <- GenerateComment(Data,
                          condition = Data[,Family] != Data[,FamilyCor],
                          comment = "The 'Family' name is incorrect")

  Data[Family != FamilyCor | (is.na(Family) & !is.na(FamilyCor)), FamilyCorSource := "APG III family"] # create the Source

  # If no Family corr with APG because no genus, previously with -aceae, take this name put in GenspFamily
  Data[is.na(FamilyCor) & !is.na(GenspFamily), `:=`(FamilyCor = GenspFamily,
                                                    FamilyCorSource = "Found in the 'Genus' or 'Species' column")]

  Data[, GenspFamily := NULL]

  # Per IdTree, the same Family, Genus, Species, Vernacular name --------------------------------------------------------

  Data[, VernNameCor := VernName]

  BotaCols <- c("FamilyCor", "GenusCor", "SpeciesCor", "VernNameCor")

  # Give the unique value (if it is unique) of the IdTree
  for(j in BotaCols){
    Data[,  (j) := ifelse(is.na(get(j)) & length(na.omit(unique(get(j)))) == 1, na.omit(unique(get(j))), get(j)), keyby = IdTree]
  }




  # Check invariant botanical informations per IdTree -------------------------------------------------------------------
  # "FamilyCor", "GenusCor", "SpeciesCor", "VernNameCor"

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {

    BotaIDCombination <- na.omit(unique(
      Data[Data$Site == s, .(IdTree, FamilyCor, GenusCor, SpeciesCor, VernNameCor)]
    ))

    CorresIDs <- BotaIDCombination[, IdTree] # .(IdTree) all the Idtree's having a unique X-Yutm) combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data <- GenerateComment(Data,
                              condition =
                                Data[,Site] == s
                              & Data[,IdTree] %in% duplicated_ID,
                              comment = "Different botanical informations (Family, ScientificName or VernName) for a same IdTree")
    }
  } # end site loop

  unique(Data[IdTree %in% duplicated_ID,
              .(IdTree = sort(IdTree), FamilyCor, GenusCor, SpeciesCor, VernNameCor)]) # to check


  # Reformer ScientificNameCor ------------------------------------------------------------------------------------------
  # If "NA NA" -> NA_character_

  Data[, ScientificNameCor := paste(GenusCor, SpeciesCor)]

  Data[, ScientificNameCor := ifelse(ScientificNameCor == "NA NA", NA_character_, ScientificNameCor)]


  return(Data)

}
