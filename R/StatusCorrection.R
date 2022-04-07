#' StatusCorrection
#'
#' @description Detect errors or detect errors and correct the tree life status
#'   evolution over the censuses.
#'   Inspired by the code of Nino Page package (ForestData::correct_alive() and
#'   .correct_alive_tree())
#'
#' @param Data (data.frame or data.table)
#'   The *LifeStatus* column must be coded as:
#'     - TRUE = alive,
#'     - FALSE = dead,
#'     - NA = unseen
#'  The *Plot* column is needed to add rows to the census where the plot was
#'  inventoried, where the tree was alive, but not recorded.
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
#'   *DetectOnly* = FALSE, add a *LifeStatusCor* column with the corrected trees
#'   life status.
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#'
#' selection <- c("101433","101435","101436","101437","101438","101439","101440",
#' "101441","101442","101443","101444","101446","101410","101067")
#'
#' Rslt <- StatusCorrection(TestData[IdTree %in% selection])
#'
#' library(ggplot2)
#' ggplot(Rslt) +
#' aes(x = CensusYear, y = LifeStatusCor) +
#'   geom_point(shape = "circle", size = 3.9, colour = "#46337E") +
#'   geom_smooth(span = 0.75) +
#'   theme_minimal() +
#'   facet_wrap(vars(IdTree), scales = "free")
#'
StatusCorrection <- function(
  Data,
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
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # Plot column exists
  if (!"Plot" %in% names(Data)){
    stop("The column 'Plot' must be present in the dataset
    to add rows to the census where the plot was inventoried, where the tree was alive, but not recorded")
  }

  # InvariantColumns
  if (!inherits(InvariantColumns, "character"))
    stop("'InvariantColumns' argument must be of character class")

  # DeathConfirmation
  if (!inherits(DeathConfirmation, "numeric"))
    stop("'DeathConfirmation' argument must be numeric")

  # UseSize/DetectOnly/RemoveRBeforeAlive/RemoveRAfterDeath
  if (!all(unlist(lapply(list(UseSize, DetectOnly, RemoveRBeforeAlive, RemoveRAfterDeath),
                         inherits, "logical"))))
    stop("The 'UseSize', 'DetectOnly', 'RemoveRBeforeAlive' and 'RemoveRAfterDeath' arguments
         of the 'SatusCorrection' function must be logicals")

  # Check if the InvariantColumns name exists in Data
  for(c in InvariantColumns){
    if (!c %in% names(Data)){
      stop(paste("InvariantColumns argument must contain one or several column names (see help)."
                 ,c,"is apparently not a dataset's column"))
    }
  }

  # UseSize-DBH
  if(UseSize %in% TRUE){ # if it is desired (TRUE) to use the presence of measurement to consider the tree alive
    if (!"DBH" %in% names(Data)){
      stop("If you wish to use the size presence (UseSize=TRUE) as a witness of the living status of the tree,
           the DBH column must be present in the dataset")
    }
  }

  #### Function ####

  # data.frame to data.table
  setDT(Data)


  # Order IdTrees and times in ascending order
  Data <- Data[order(IdTree, CensusYear)]

  # IdTrees vector
  Ids <- as.vector(na.omit(unique(Data$IdTree))) # Tree Ids

  # Dataset with the rows without IdTree
  DataIDNa <-  Data[is.na(IdTree)]

  # Apply for all the trees
  # i = "100635"
  Data <- do.call(rbind, lapply(Ids, function(i) StatusCorrectionByTree(
    Data[IdTree %in% i], # per IdTree, all censuses
    PlotCensuses = as.vector(na.omit( # rm NA
      unique(Data[Plot %in% unique(Data[IdTree %in% i, Plot]),  CensusYear]) # the censuses for the plot in which the tree is
    )),
    InvariantColumns = InvariantColumns,
    DeathConfirmation = DeathConfirmation,
    UseSize = UseSize,
    DetectOnly = DetectOnly,

    RemoveRBeforeAlive = RemoveRBeforeAlive,
    RemoveRAfterDeath = RemoveRAfterDeath
  )
  )) # do.call apply the 'rbind' to the lapply result

  # Re-put the the rows without IdTree
  Data <- rbindlist(list(Data, DataIDNa), use.names=TRUE, fill=TRUE)

  return(Data)

}



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
#' @param PlotCensuses Census years for the plot in which the tree is (integer)
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
#' @importFrom stats na.omit
#' @importFrom data.table data.table rbindlist
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
#' DataTree <- Data[IdTree == "101623"]
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
#' Rslt <- StatusCorrectionByTree(DataTree, PlotCensuses = c(2011:2021))
#'
#' library(ggplot2)
#' ggplot(DataTree) +
#'   aes(x = CensusYear, y = LifeStatus) +
#'   geom_point(shape = "circle", size = 3.9, colour = "#46337E") +
#'   theme_minimal()
#'
#' ggplot(Rslt) +
#'   aes(x = CensusYear, y = LifeStatusCor) +
#'   geom_point(shape = "circle", size = 4L, colour = "#46337E") +
#'   theme_minimal()
#'
StatusCorrectionByTree <- function(
  DataTree,
  PlotCensuses,
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

  # PlotCensuses
  if (!inherits(PlotCensuses, c("numeric", "integer")))
    stop("'PlotCensuses' argument must be numeric or integer")

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
         of the 'StatusCorrectionByTree' function must be logicals")

  # if there are several IdTrees
  if(length(unique(DataTree$IdTree)) != 1){
    stop("DataTree must correspond to only 1 same tree so 1 same IdTree
    (the IdTrees: " ,paste0(unique(DataTree$IdTree), collapse = "/"),")")
  }

  # if there are several plots for the same IdTree
  if(length(as.vector(na.omit(unique(DataTree$Plot)))) != 1){
    stop(paste0("Tree ",unique(DataTree$IdTree)," has multiple plots: " ,paste0(unique(DataTree$Plot), collapse = "/")))
  }

  # Check if the InvariantColumns name exists in DataTree
  for(c in InvariantColumns){
    if(!c %in% names(DataTree)){
      stop(paste("InvariantColumns argument must contain one or several column names (see help)."
                 ,c,"is apparently not a dataset's column"))
    }
  }

  # UseSize-DBH column
  if(UseSize %in% TRUE){ # if it is desired (TRUE) to use the presence of measurement to consider the tree alive
    if(!"DBH" %in% names(DataTree)){
      stop("If you wish to use the size presence (UseSize=TRUE) as a witness of the living status of the tree,
           the DBH column must be present in the dataset")
    }
  }


  #### Function ####

  # print(unique(DataTree[, IdTree])) # to debug

  # data.frame to data.table
  setDT(DataTree)

  # Arrange year in ascending order
  DataTree <- DataTree[order(CensusYear)] # order de dt

  if(DetectOnly %in% FALSE){
    DataTree[, LifeStatusCor := LifeStatus] # we will work on a new col and keep the original col intact
  }

  #### Use the size presence as a witness of the living status of the tree ####
  if(UseSize){

    DataTree <- GenerateComment(DataTree,
                                condition = !is.na(DataTree[, DBH]) &
                                  !DataTree[,LifeStatus] %in% TRUE,
                                comment = "A measured tree is a living tree")


    if(DetectOnly %in% FALSE){
      DataTree[!is.na(DBH), LifeStatusCor := TRUE]
    }

  }

  #### Sequence analyse ####
  # if tree has ever been recorded alive
  if(any(DataTree$LifeStatusCor %in% TRUE)){
    # The first Alive record year
    FirstAliveYear <- min(DataTree[LifeStatusCor %in% TRUE, CensusYear], na.rm = TRUE)

    # First/last alive positions (rows id)
    FirstAlive <- which(DataTree$LifeStatusCor %in% TRUE)[1] # the 1st seen alive
    LastAlive <-  max(which(DataTree$LifeStatusCor %in% TRUE)) # the last seen alive
  }

  #### Absents (logical vector of the PlotCensuses length) #### En DetectOnly, je renvoie qqchose ? quoi ? ####

  PlotCensuses <- sort(PlotCensuses) # increasing order

  if(any(DataTree$LifeStatusCor %in% TRUE)){

    # if tree has ever been recorded dead
    if(any(DataTree$LifeStatusCor %in% FALSE)){
      # The last time where the tree has been recorded dead (in case there are several)
      LastDeathRecord <- max(DataTree[LifeStatusCor %in% FALSE, CensusYear], na.rm = TRUE)

      After <- which(DataTree$CensusYear > LastDeathRecord) # After the last death record

      # If there is any "Alive" report after last reported death
      if(any(DataTree$LifeStatusCor[After] %in% TRUE)) {
        # Absents are the absent record years among the plot censuses from the 1st alive record
        Absents <- (PlotCensuses > FirstAliveYear & !PlotCensuses %in% DataTree$CensusYear)

      }else{
        # Absents are the absent record years between first alive record and the last death record
        Absents <- (PlotCensuses > FirstAliveYear &
                      PlotCensuses < LastDeathRecord & # death = the end
                      !PlotCensuses %in% DataTree$CensusYear)
      }

    }else{ # if tree has not been reported dead yet

      # Absents are the absent record years among the plot censuses from the 1st alive record
      Absents <- (PlotCensuses > FirstAliveYear & !PlotCensuses %in% DataTree$CensusYear)

    }

    # if no one alive
  }else{

    # La j'ai choisi de ne rajouter les lignes absentes qu'entre le census min et max de l'arbre
    # Si tout est FALSE effectivement ça ne sert à rien de rajouter des lignes apres, mais des lignes avant ? il risque d'en avoir beaucoup, et on ne pourra mettre qu'NA
    # Pour tout est NA, ça aurait un intéret de rajouter des lignes avant-après ?
    Absents <- (PlotCensuses > min(DataTree$CensusYear, na.rm = TRUE) & # entre les bornes, pas avnt pas après
                  PlotCensuses < max(DataTree$CensusYear, na.rm = TRUE) &
                  !PlotCensuses %in% DataTree$CensusYear)

  }

  # En DetectOnly, je renvoie qqchose ? quoi ? ########

  #### Creating rows for absents ####
  if(DetectOnly %in% FALSE){

    Nabs <- sum(Absents) # absent is a logical vector giving the census times for which trees were not seen.

    if(Nabs > 0){ # if there are absents
      # if(DataTree$Plot[1] == 1) print(DataTree$Plot[1])
      NewRow <- data.table(IdTree = unique(DataTree$IdTree),     # the idtree
                           LifeStatus = NA,                    # not seen
                           LifeStatusCor = NA,               # no corrected status for now
                           Plot = unique(DataTree$Plot),  # the unique plot in DataTree
                           SubPlot = unique(DataTree$SubPlot),  # the unique subplot in DataTree
                           stringsAsFactors =  FALSE)      # do not convert characters into factors

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
      NewRows[, CensusYear := PlotCensuses[Absents]]

      # Add these rows in the dataset
      DataTree <- rbindlist(list(DataTree, NewRows), use.names=TRUE, fill=TRUE)

      DataTree <- DataTree[order(CensusYear)] # order by time

    } # end: Nabsents > 0
  }


  #### Alive > *Alive* > Alive ####
  if(any(DataTree$LifeStatusCor %in% TRUE)){

    DataTree <- GenerateComment(DataTree,
                                condition = seq.int(nrow(DataTree)) %in% (FirstAlive:LastAlive) &
                                  !DataTree[, LifeStatusCor] %in% TRUE,
                                comment = "Between 2 alive occurrences of the tree, the tree was alive")

    if(DetectOnly %in% FALSE){
      DataTree[FirstAlive:LastAlive, LifeStatusCor := TRUE] # so all between is alive
    }
  }

  #### Enough/not enough occurrences of death to validate it ####
  # If there are things after the last occurrence of life
  if(any(DataTree$LifeStatusCor %in% NA)){
    if(any(DataTree$LifeStatusCor %in% TRUE)){


      # If there are things after the last occurrence of life
      if(LastAlive != nrow(DataTree)){ # if the last seen alive is not the last row of the database

        #### if the one after the last one seen alive is Dead ####
        if(DataTree[LastAlive +1, LifeStatusCor] %in% FALSE){
          if(DetectOnly %in% FALSE){

            # Remove rows after the death (after correction) (User choice)
            if(RemoveRAfterDeath %in% TRUE)
              DataTree <- DataTree[-((LastAlive +2):nrow(DataTree)),]

          }
        }
        #### if the one after the last one seen alive is Unseen ####
        else if(DataTree[LastAlive +1, LifeStatusCor] %in% NA){

          ##### If there is still a "death" occurrence #####
          if(any(DataTree$LifeStatusCor %in% FALSE)){

            LastDeath <- max(which(DataTree$LifeStatusCor %in% FALSE))

            ###### If the death is not the last record ######
            if(LastDeath < nrow(DataTree)){
              unseen <- sum(DataTree[(LastAlive +1):(LastDeath-1), LifeStatusCor] %in% NA) # NA until the death (logicals vector)

            }else{
              unseen <- sum(DataTree[(LastAlive +1):nrow(DataTree), LifeStatusCor] %in% NA) # NA until the dataset end (logicals vector)
            }
            ##### No death record #####
          }else{
            unseen <- sum(DataTree[(LastAlive +1):nrow(DataTree), LifeStatusCor] %in% NA) # NA until the dataset end (logicals vector)
          }

          if(DeathConfirmation <= unseen){

            # The comment
            DataTree <- GenerateComment(DataTree,
                                        condition = seq.int(nrow(DataTree)) %in% ((LastAlive +1):(LastAlive +unseen)) &
                                          DataTree[, LifeStatusCor] %in% NA,
                                        comment = "When the tree is unseen a number of times >= DeathConfirmation, it is considered dead")

            if(DetectOnly %in% FALSE){

              # The correction
              DataTree[(LastAlive +1):(LastAlive +unseen), LifeStatusCor := FALSE] # Death validated

              # Remove rows after the death (after correction) (User choice)
              if(RemoveRAfterDeath %in% TRUE)
                DataTree <- DataTree[-((LastAlive +2):nrow(DataTree)),]

            } # correction end

          } # else if(DeathConfirmation > unseen) NAs remain NAs

        }
      } # If there nothing after the last occurrence of life
    } # if there is any alive
  } # any NA ?


  #### Before the first alive ####
  if(any(DataTree$LifeStatusCor %in% TRUE)){

    if(FirstAlive > 1){ # if the first seen alive is not the first row -> NA/dead

      # The comment
      DataTree <- GenerateComment(DataTree,
                                  condition = seq.int(nrow(DataTree)) %in% (1:(FirstAlive -1)) &
                                    DataTree[, LifeStatusCor] %in% NA, # -> unseen
                                  comment = "Isn't the tree alive?")

      DataTree <- GenerateComment(DataTree,
                                  condition = seq.int(nrow(DataTree)) %in% (1:(FirstAlive -1)) &
                                    DataTree[, LifeStatusCor] %in% FALSE, # -> dead
                                  comment = "Tree cannot be dead before being alive")


      if(DetectOnly %in% FALSE){

        # The correction (mettre un ifelse)
        DataTree[1:(FirstAlive -1), LifeStatusCor := ifelse(LifeStatusCor %in% FALSE, NA, LifeStatusCor)] # Tree cannot be dead before being alive -> NA

        # Remove rows after the death (after correction) (User choice)
        if(RemoveRBeforeAlive %in% TRUE)
          DataTree <- DataTree[-(1:(FirstAlive -1)),] # remove from the first to the last row before the first seen alive

      } # correction end
    }
  }

  #### After the death ####
  # If all rows count
  if(RemoveRAfterDeath %in% FALSE){

    # If there is still a "death" occurrence
    if(any(DataTree$LifeStatusCor %in% FALSE)){

      LastDeath <- max(which(DataTree$LifeStatusCor %in% FALSE))

      # If the death is not the last record
      if(LastDeath < nrow(DataTree)){

        # The comment
        DataTree <- GenerateComment(DataTree,
                                    condition = seq.int(nrow(DataTree)) %in% ((LastDeath +1):nrow(DataTree)) &
                                      DataTree[, LifeStatusCor] %in% NA,
                                    comment = "After its death the tree is still dead")

        if(DetectOnly %in% FALSE){

          # The correction
          DataTree[(LastDeath +1):nrow(DataTree), LifeStatusCor := FALSE] # After death there is only death

        } # correction end
      } # the death isn't the last record
    } # there is still a death
  } # we want all the deaths!


  #### Dead > *Dead* > Dead ####

  if(any(DataTree$LifeStatusCor %in% FALSE)){

    FirstDead <- which(DataTree$LifeStatusCor %in% FALSE)[1] # the 1st seen dead
    LastDead <-  max(which(DataTree$LifeStatusCor %in% FALSE)) # the last seen dead

    DataTree <- GenerateComment(DataTree,
                                condition = seq.int(nrow(DataTree)) %in% (FirstDead:LastDead) &
                                  !DataTree[, LifeStatusCor] %in% FALSE,
                                comment = "Between 2 dead occurrences of the tree, it is dead")

    if(DetectOnly %in% FALSE){
      DataTree[FirstDead:LastDead, LifeStatusCor := FALSE] # so all between is dead
    }
  }

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

  #### Arguments check ####
  # NewRow
  if (!inherits(NewRow, "data.table"))
    stop("'NewRow' argument of the 'FillinInvariantColumns' function must be a data.table")

  # DataTree
  if (!inherits(DataTree, "data.table"))
    stop("DataTree must be a data.table")

  # if there are several IdTrees
  if(length(unique(DataTree$IdTree)) != 1){
    stop("DataTree must correspond to only 1 same tree so 1 same IdTree
    (the IdTrees: " ,paste0(unique(DataTree$IdTree), collapse = "/"),")")
  }

  # Check if the InvariantColumns name exists in DataTree
  for(c in InvariantColumns){
    if(!c %in% names(DataTree)){
      stop(paste("InvariantColumns argument must contain one or several column names (see help)."
                 ,c,"is apparently not a dataset's column"))
    }
  }

  # IdTree
  if (!inherits(IdTree, "character"))
    stop("'IdTree' argument must be of character class")


  #### Function ####

  # j = "ScientificName"
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

