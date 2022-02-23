# StatusCorrection <- function(
#   Data,
#   InvariantColumns = c("Site",
#                        "Genus",
#                        "Species",
#                        "Family",
#                        "ScientificName"),
#   DeathConfirmation = 2, # (numeric)
#   usesize = FALSE, # (logical)
#   DetectOnly = FALSE, # (logical)
#
#   RemoveRBeforeAlive = FALSE, # "Do you want to delete the rows about the tree before it was seen alive for the 1st time?" (logical)
#   RemoveRAfterDeath = FALSE # "After correction do you want to delete the rows concerning the tree after its death?" (logical)
# ){
#
# }

#' StatusCorrectionByTree
#'
#' @description Detect errors or detect errors and correct the tree life status
#'   evolution over the censuses.
#'   Inspired by the code of Nino Page package (ForestData::correct_alive() and
#'   .correct_alive_tree())
#'
#' @param DataTree A dataset corresponding to a single tree's (1 IdTree)
#'   measurements (data.frame or data.table)
#'   The *LifeStatus* column must be coded as:
#'     - TRUE = alive,
#'     - FALSE = dead,
#'     - NA = unseen
#'
#' @param Censuses Census years for the plot in which the tree is (numeric)
#'
#' @param InvariantColumns Vector with the names of the columns that are
#'   supposed to have always the same value for each measurement of the same
#'   tree (character)
#'
#' @param DeathConfirmation Number of times (censuses) needed for an unseen tree
#'   to be considered dead (numeric)
#'
#' @param UseSize Use the size presence as a witness of the living status of the tree (logical)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (logical)
#'
#' @param RemoveRBeforeAlive Do you want to delete the rows about the tree
#'   before it was seen alive for the 1st time? (logical)
#'
#' @param RemoveRAfterDeath After correction do you want to delete the rows
#'   concerning the tree after its death? (logical)
#'
#' @details
#' - if UseSize : if DBH != NA -> Alive
#' - *Dead* > Alive -> NA
#' - add rows for the forgotten censuses between 2 'Alive'
#' - Alive > *Dead*/*NA* > Alive -> Alive
#' - Alive > *NA* > Dead -> NA
#' - Alive > *Dead* > NA -> Dead
#'
#' - Alive > *NA* > *NA*:
#'   if DeathConfirmation > unseens -> NA
#'   if DeathConfirmation =< unseens -> Dead
#'
#' @return Fill the *Comment* column with error type informations. If
#'   *DetectOnly* = FALSE, add a *LifeStatusCor* column with the corrected tree
#'   life status.
#'
#' @importFrom data.table order
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#' Data <- TestData
#'
#'# Wanted seq: Dead > Alive > NA > Dead > Alive > NA > NA > Dead > NA
#'
#' DataTree <- Data[IdTree == "100658"]
#'
#' AddR <- DataTree[2:5] # the rows to copy
#' AddR[, CensusYear := c(2012:2015)] # the rows to add
#'
#' DataTree <- rbindlist(list(DataTree, AddR)) # add rows
#'
#' DataTree <- DataTree[order(CensusYear)] # arrange years in ascending order
#'
#' # Write the sequence
#' DataTree[, LifeStatus := c(FALSE, TRUE, NA, FALSE, TRUE, NA, NA, FALSE, NA)]
#'
#' Rslt <- StatusCorrectionByTree(DataTree, Censuses = c(2011:2021))
#'
StatusCorrectionByTree <- function(
  DataTree,
  Censuses,
  InvariantColumns = c("Site",
                       "Genus",
                       "Species",
                       "Family",
                       "ScientificName"),
  DeathConfirmation = 2,
  UseSize = FALSE,
  DetectOnly = FALSE,

  RemoveRBeforeAlive = FALSE,
  RemoveRAfterDeath = FALSE
){

  #### Arguments check ####
  # DataTree
  if (!inherits(DataTree, c("data.table", "data.frame")))
    stop("DataTree must be a data.frame or data.table")

  # Censuses
  if (!inherits(Censuses, "numeric"))
    stop("'Censuses' argument must be numeric")

  # InvariantColumns
  if (!inherits(InvariantColumns, "character"))
    stop("'InvariantColumns' argument must be of character class")

  # DeathConfirmation
  if (!inherits(DeathConfirmation, "numeric"))
    stop("'DeathConfirmation' argument must be numeric")

  # UseSize/DetectOnly/RemoveRBeforeAlive/RemoveRAfterDeath
  if(!all(unlist(lapply(list(UseSize, DetectOnly, RemoveRBeforeAlive, RemoveRAfterDeath),
                        inherits, "logical"))))
    stop("The 'UseSize', 'DetectOnly', 'RemoveRBeforeAlive' and 'RemoveRAfterDeath' arguments
         of the 'SatusCorrection' function must be logicals")

  # if there are several IdTrees
  if(length(unique(DataTree$IdTree)) != 1){
    stop("DataTree must correspond to only 1 same tree so 1 same IdTree
    (the IdTrees: " ,paste0(unique(DataTree$IdTree), collapse = "/"),")")
  }

  # if there are several plots for the same IdTree
  if(length(unique(DataTree$Plot)) != 1){
    stop(paste0("Tree ",unique(DataTree$IdTree)," has multiple plots: " ,paste0(unique(DataTree$Plot), collapse = "/")))
  }

  # Check if the InvariantColumns name exists in DataTree
  for(c in InvariantColumns){
    if(!c %in% names(DataTree)){
      stop(paste("InvariantColumns argument must contain one or several column names (see help)."
                 ,c,"is apparently not a dataset's column"))
    }
  }

  if(UseSize %in% TRUE){ # if it is desired (TRUE) to use the presence of measurement to consider the tree alive
    if(!DBH %in% names(DataTree)){
      stop("If you wish to use the size presence (UseSize=TRUE) as a witness of the living status of the tree,
           the DBH column must be present in the dataset")
    }
  }


  #### Function ####

  print(unique(DataTree[, IdTree]))

  # data.frame to data.table
  setDT(DataTree)

  # Arrange year in ascending order
  DataTree <- DataTree[order(CensusYear)] # order de dt

  #### Use the size presence as a witness of the living status of the tree ####
  if(UseSize){

    DataTree <- GenerateComment(DataTree,
                                condition = !is.na(DataTree[, DBH]) &
                                  !DataTree[,LifeStatus] %in% TRUE,
                                comment = "A measured tree is a living tree")


    if(DetectOnly %in% FALSE){
      DataTree[!is.na(DBH), LifeStatusCor := TRUE] # c'est tout ? une taille = vivant ?
    }

  }else{

    if(DetectOnly %in% FALSE){
      DataTree[, LifeStatusCor := LifeStatus] # we will work on a new col and keep the original col intact
    }
  }

  #### Sequence analyse ####
  # if tree has ever been recorded alive
  if(any(DataTree$LifeStatus %in% TRUE)){
  # The first Alive record year
  FirstAliveYear <- min(DataTree[LifeStatus %in% TRUE, CensusYear])

  # First/last alive positions (rows id)
  FirstAlive <- which(DataTree$LifeStatus %in% TRUE)[1] # the 1st seen alive
  LastAlive <-  max(which(DataTree$LifeStatus %in% TRUE)) # the last seen alive
  }

  #### Absents (logical vector of the Censuses length) #### En DetectOnly, je renvoie qqchose ? quoi ? ####

  Censuses <- sort(Censuses) # increasing order

  # if tree has ever been recorded dead
  if(any(DataTree$LifeStatus %in% FALSE)){
    # The last time where the tree has been recorded dead (in case there are several)
    LastDeathRecord <- max(DataTree[LifeStatus %in% FALSE, CensusYear])

    After <- which(DataTree$CensusYear > LastDeathRecord) # After the last death record

    # If there is any "Alive" report after last reported death
    if(any(DataTree$LifeStatus[After] %in% TRUE)) {
      # Absents are the absent record years among the plot censuses from the 1st alive record
      Absents <- (Censuses > FirstAliveYear & !Censuses %in% DataTree$CensusYear)

    }else{
      # Absents are the absent record years between first alive record and the last death record
      Absents <- (Censuses > FirstAliveYear &
                    Censuses < LastDeathRecord & # death = the end
                    !Censuses %in% DataTree$CensusYear)
    }

  }else{ # if tree has not been reported dead yet

    # Absents are the absent record years among the plot censuses from the 1st alive record
    Absents <- (Censuses > FirstAliveYear & !Censuses %in% DataTree$CensusYear)

  }

  #### Creating rows for absents ####
  if(DetectOnly %in% FALSE){

    Nabs <- sum(Absents) # absent is a logical vector giving the census times for which trees were not seen.

    if(Nabs > 0){ # if there are absents
      # if(DataTree$Plot[1] == 1) print(DataTree$Plot[1])
      NewRow <- data.table(IdTree = unique(DataTree$IdTree),     # the idtree
                           CensusYear = Censuses[Absents],       # absent censuses list
                           LifeStatus = NA,                    # not seen
                           LifeStatusCor = NA,               # no corrected status for now
                           Plot = unique(DataTree$Plot),  # the unique plot in DataTree
                           SubPlot = unique(DataTree$SubPlot),  # the unique subplot in DataTree
                           stringsAsFactors =  FALSE)      # do not convert characters into factors
      NewNames <- names(NewRow)

      if(length(InvariantColumns) > 0){ # if there are invariant columns

        NewRow[,(InvariantColumns) := NA] # empty the invariant columns for the added rows

        # Fill in the invariant columns in the added rows
        NewRow <- FillinInvariantColumns(NewRow = NewRow,
                                         InvariantColumns = InvariantColumns,
                                         DataTree = DataTree,
                                         IdTree = unique(DataTree$IdTree))
      }

      # Multiply this new row the number of times as well as the number of absents
      NewRows <- do.call("rbind", replicate(n = Nabs, NewRow, simplify = FALSE))

      # Add these rows in the dataset
      DataTree <- rbindlist(list(DataTree, NewRows), use.names=TRUE, fill=TRUE)

      DataTree <- DataTree[order(CensusYear)] # order by time

    } # end: Nabsents > 0
  }

  #### Alive > *Alive* > Alive ####
  DataTree <- GenerateComment(DataTree,
                              condition = seq.int(nrow(DataTree)) %in% (FirstAlive:LastAlive) &
                                !DataTree[, LifeStatus] %in% TRUE,
                              comment = "Between 2 alive occurrences of the tree, it cannot be dead")

  if(DetectOnly %in% FALSE){
    DataTree[FirstAlive:LastAlive, LifeStatusCor := TRUE] # so all between is alive
  }

  #### Enough/not enough occurrences of death to validate it ####
  # If there are things after the last occurrence of life
  if(LastAlive != nrow(DataTree)){ # if the last seen alive is not the last row of the database

    #### if the one after the last one seen alive is Dead ####
    if(DataTree[LastAlive +1, LifeStatus] %in% FALSE){

      # Remove rows after the death (after correction) (User choice)
      if(RemoveRAfterDeath %in% TRUE)
        DataTree <- DataTree[-(LastAlive +2:nrow(DataTree)),]

    }
    #### if the one after the last one seen alive is Unseen ####
    else if(DataTree[LastAlive +1, LifeStatus] %in% NA){

      ##### If there is still a "death" occurrence #####
      if(any(DataTree$LifeStatus %in% FALSE)){

        LastDeath <- max(which(DataTree$LifeStatus %in% FALSE))

        ###### If the death is not the last record ######
        if(LastDeath < nrow(DataTree)){
          unseen <- sum(DataTree[(LastAlive +1):(LastDeath-1), LifeStatus] %in% NA) # NA until the death (logicals vector)

        }else{
          unseen <- sum(DataTree[(LastAlive +1):nrow(DataTree), LifeStatus] %in% NA) # NA until the dataset end (logicals vector)
        }
        ##### No death record #####
      }else{
        unseen <- sum(DataTree[(LastAlive +1):nrow(DataTree), LifeStatus] %in% NA) # NA until the dataset end (logicals vector)
      }

      if(DeathConfirmation <= unseen){

        # The comment
        DataTree <- GenerateComment(DataTree,
                                    condition = seq.int(nrow(DataTree)) %in% ((LastAlive +1):(LastAlive +unseen)) &
                                      DataTree[, LifeStatus] %in% NA,
                                    comment = "When the tree is unseen a number of times >= DeathConfirmation, it is considered dead")

        if(DetectOnly %in% FALSE){

          # The correction
          DataTree[(LastAlive +1):(LastAlive +unseen), LifeStatusCor := FALSE] # Death validated

          # Remove rows after the death (after correction) (User choice)
          if(RemoveRAfterDeath %in% TRUE)
            DataTree <- DataTree[-(LastAlive +2:nrow(DataTree)),]

        } # correction end

      } # else if(DeathConfirmation > unseen) NAs remain NAs

    }
  } # If there nothing after the last occurrence of life

  #### Before the first alive ####
  if(FirstAlive > 1){ # if the first seen alive is not the first row -> NA/dead

    # The comment
    DataTree <- GenerateComment(DataTree,
                                condition = seq.int(nrow(DataTree)) %in% (1:(FirstAlive -1)) &
                                  DataTree[, LifeStatus] %in% NA, # -> unseen
                                comment = "Isn't the tree alive?")

    DataTree <- GenerateComment(DataTree,
                                condition = seq.int(nrow(DataTree)) %in% (1:(FirstAlive -1)) &
                                  DataTree[, LifeStatus] %in% FALSE, # -> dead
                                comment = "Tree cannot be dead before being alive")


    if(DetectOnly %in% FALSE){

      # The correction (mettre un ifelse)
      DataTree[1:(FirstAlive -1), LifeStatusCor := ifelse(LifeStatus %in% FALSE, NA, LifeStatusCor)] # Tree cannot be dead before being alive -> NA

      # Remove rows after the death (after correction) (User choice)
      if(RemoveRBeforeAlive %in% TRUE)
        DataTree <- DataTree[-(1:(FirstAlive -1)),] # remove from the first to the last row before the first seen alive

    } # correction end
  }

  #### After the death ####
  # If all rows count
  if(RemoveRAfterDeath %in% FALSE){

    # If there is still a "death" occurrence
    if(any(DataTree$LifeStatus %in% FALSE)){

      LastDeath <- max(which(DataTree$LifeStatus %in% FALSE))

      # If the death is not the last record
      if(LastDeath < nrow(DataTree)){

        # The comment
        DataTree <- GenerateComment(DataTree,
                                    condition = seq.int(nrow(DataTree)) %in% ((LastDeath +1):nrow(DataTree)) &
                                      DataTree[, LifeStatus] %in% NA,
                                    comment = "After its death the tree is still dead")

        if(DetectOnly %in% FALSE){

          # The correction
          DataTree[(LastDeath +1):nrow(DataTree), LifeStatusCor := FALSE] # After death there is only death

        } # correction end
      } # the death isn't the last record
    } # there is still a death
  } # we want all the deaths!



  return(DataTree)

}


#' FillinInvariantColumns
#'
#' @description Fill the invariant columns in NewRow with their (unique) value
#'
#' @param NewRow The dataset to fill (data.table)
#'
#' @param InvariantColumns Vector with the names of the columns that are
#'   supposed to have always the same value for each measurement of the same
#'   tree (character)
#'
#' @param DataTree A dataset corresponding to a single tree's (1 IdTree)
#'   measurements, with the invariant columns and their value (data.table)
#'
#' @param IdTree (character)
#'
#' @return The *NewRow* dataset with the invariant columns filled with their
#'   (unique) value
#' @export
#'
FillinInvariantColumns <- function(NewRow, InvariantColumns, DataTree, IdTree){

  for(j in InvariantColumns){

    if(any(is.na(NewRow[,get(j)]))){ # if the column is empty in the new rows (the "absent" trees)

      uni <- unique(DataTree[, get(j)])

      if(length(uni) > 1){ # if the "invariant column is not invariant
        stop("The variable ",
             j,
             " that you defined as a non-varying column -i.e. supposed to have always the same value for each measurement of the same tree- has multiple values for tree ",
             IdTree,
             " and takes the values ",
             uni)
      }
      else if(length(uni) == 0){ # no value in the invariant column
        stop("The variable ",j," has no value for individual ",IdTree)
      }
      else{
        NewRow[is.na(get(j)), (j) := uni] # fill the invariant column in NewRow with their (unique) value
      }
    }
  }

  return(NewRow)
}

