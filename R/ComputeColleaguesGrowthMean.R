#' Compute Colleagues Growth Mean
#'
#' @description Internal function of *PhylogeneticHierarchicalCorrection()*
#'
#' @param Colleagues 'Colleagues' are the individuals other than the one whose
#'   diameter is to be corrected. Their diameter is included in the 'DBHRange'
#'   around the diameter to be corrected. They are either of the same species,
#'   the same genus, the same family or the same stand, depending on what the
#'   inventory proposes and the minimum selection criteria of the user
#'   ('*MinIndividualNbr*' argument of *PhylogeneticHierarchicalCorrection()*).
#'   (data.table)
#'   The dataset must contain the column: 'IdStem' or 'IdTree' (character)
#'
#' @param Data (data.table)
#'   The dataset must contain the columns:
#'   - 'IdStem' or 'IdTree' (character)
#'   - 'Diameter' (numeric)
#'   - 'Year' (numeric)
#'
#' @param PrevValue The last diameter value before the one to correct (cm)
#'
#' @param PositiveGrowthThreshold in cm/year : a tree
#'   widening by more than x cm/year is considered abnormal (numeric, 1 value)
#'
#' @param NegativeGrowthThreshold in cm/census : The possible
#'   positive measurement error (+n) cannot be corrected until the growth
#'   appears abnormal, but a negative measurement error can be allowed until -n
#'   (a tree does not decrease). Thus the positive measurement error (+n) is
#'   "compensated". (numeric, 1 value)
#'
#' @param DBHRange DBH range in cm to take into account to select other trees in
#'   the dataset to apply "phylogenetic hierarchical" correction (Default: 10
#'   cm) (numeric, 1 value)
#'
#' @return The average growth of 'colleagues' in the DBH range (numeric)
#'
#' @export
#'
#' @examples
#' data(TestData)
#'
#' PrevValue <- 17 # Estimated DBH (cm)
#' DBHRange = 10 # DBH range (cm)
#'
#' # Find colleagues
#' Colleagues <- TestData[
#' ScientificName == "Licania membranacea" & # same species as the tree to be corrected
#' (Diameter > (PrevValue - DBHRange/2) & Diameter < (PrevValue + DBHRange/2))] # Diameter or DBHCor ?
#'
#' ColleaguesCrescMean <- ComputeColleaguesGrowthMean(Colleagues = Colleagues, Data = TestData,
#'                                                    PrevValue = PrevValue,
#'                                                    PositiveGrowthThreshold = 5,
#'                                                    NegativeGrowthThreshold = -2,
#'                                                    DBHRange = DBHRange)
#'
ComputeColleaguesGrowthMean <- function(
  Colleagues,
  Data,
  PrevValue,
  PositiveGrowthThreshold,
  NegativeGrowthThreshold,
  DBHRange
){

  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(Data) | all(is.na(Data$IdStem))) &
     ("IdTree" %in% names(Data) & any(!is.na(Data$IdTree))) ){ ID <- "IdTree"

  }else{ ID <- "IdStem"}

  if(!any(c("IdStem", "IdTree") %in% names(Data)) | (all(is.na(Data$IdStem)) &  all(is.na(Data$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------


  # POM or HOM?
  if((!"POM" %in% names(Colleagues) | all(is.na(Colleagues$POM))) &
     ("HOM" %in% names(Colleagues) & any(!is.na(Colleagues$HOM))) ){ POMv <- "HOM" # If no POM take HOM

  }else{ POMv <- "POM"}

  # Work start --------------------------------------------------------------------------------------------------------------
  # Find their cresc at these DBHs and compute the average of these crescs

  # Collect their ID ----------------------------------------------------------------------------------------------------
  ColleaguesId <- unique(Colleagues[, get(ID)])

  # DBH seq of the Colleagues -----------------------------------------------------------------------------------------------
  ColleaguesSeq <- Data[get(ID) %in% ColleaguesId]

  # Remove duplicated measurements ----------------------------------------------------------------------------------------
  ColleaguesSeq <- ColleaguesSeq[!duplicated(ColleaguesSeq[, list(get(ID), Year)])]

  # Order IDs and times in ascending order
  ColleaguesSeq <- ColleaguesSeq[order(get(ID), Year)]

  # Compute cresc for each Colleague ----------------------------------------------------------------------------------------
  # i = "614506"
  for(i in ColleaguesId){ # i : each Colleagues ind

    # i = "100747"
    ColleaguesCresc <- ComputeIncrementation( # matrix (Rows: ind, Col: cresc)
      Var = ColleaguesSeq[get(ID) %in% i, Diameter], # Diameter or DBHCor ?
      Type = "annual",
      Time = ColleaguesSeq[get(ID) %in% i, Year]
    )

    # Compute cresc for individual i
    if(nrow(ColleaguesSeq[get(ID) == i]) > (length(ColleaguesCresc)+1) ) # 1 is for the fist value where cresc=NA
      ColleaguesCresc [ (length(ColleaguesCresc)+1) :( nrow(ColleaguesSeq[get(ID) == i])-1) ] <- NA # put NA errased at the vector end

    ColleaguesSeq[get(ID) == i, Cresc := c(NA, ColleaguesCresc)] # crescs in the Colleagues table
    # no cresc if no diameter


    # No cresc during POM change
    # POM change detection
    POMchange <- NA  # 1st val = NA because it's the default POM
    for( n in (2:(length(ColleaguesSeq[get(ID) %in% i, get(POMv)]))) ){
      POMchange <- c(POMchange, ColleaguesSeq[get(ID) %in% i, get(POMv)][n-1] != ColleaguesSeq[get(ID) %in% i, get(POMv)][n]) # (TRUE = POM change)
    }

    ColleaguesSeq[get(ID) == i, POMChange := POMchange]
  }

  # No cresc during POM change
  Colleagues <- ColleaguesSeq[POMChange %in% FALSE]

  # Keep only rows with DBH in DBHRange -------------------------------------------------------------------------------------
  Colleagues <- Colleagues[Diameter > (PrevValue - DBHRange/2) & Diameter < (PrevValue + DBHRange/2)] # Diameter or DBHCor ?

  # Compute mean(Colleaguescresc) -------------------------------------------------------------------------------------------
  ColleaguesCrescMean <- mean(
    Colleagues[Cresc < PositiveGrowthThreshold | Cresc > NegativeGrowthThreshold, Cresc] # filter abnormal growth
  )

  # length(ColleaguesCrescMean) == 0 # cas où il ne reste plus de colleagues (il faudrait faire tourner cette fct pour tous les niveaux (sp, gen, fam)..très long.

  return(ColleaguesCrescMean)

}
