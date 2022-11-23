#' Status Correction
#'
#' @description Detect errors, or detect errors and correct, the tree life
#'   status evolution over the censuses.
#'   Inspired by the code of Nino Page package (ForestData::correct_alive() and
#'   .correct_alive_tree())
#'
#' @param Data Dataset (data.frame or data.table)
#'   The *LifeStatus* column must be coded as:
#'     - TRUE = alive,
#'     - FALSE = dead,
#'     - NA = unseen
#'  The *Plot* column is needed to add rows to the census where the plot was
#'  inventoried, where the tree was alive, but not recorded.
#'
#' @param InvariantColumns Vector with the names of the columns that are
#'   supposed to have always the same value for each measurement of the same
#'   tree (character). It is recommended to use the columns that have already
#'   been **corrected**, such as the columns containing the corrected botanical
#'   information.
#'
#' @param DeathConfirmation Number of times (censuses) needed for an unseen tree
#'   to be considered dead (numeric)  (Default = 2 censuses)
#'
#' @param UseSize Use the size presence as a witness of the living status of the
#'   tree (logical) (Default = FALSE)
#'
#' @param AddRowsForForgottenCensuses TRUE: adds rows for forgotten censuses
#'   between 2 'Alive', FALSE: does not add any rows (logical)
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
#' - if UseSize : if Diameter != NA -> Alive
#' If (the value in bold is modified by the value given after the arrow):
#' (the ">" gives the chronological order of the sequence)
#' - *Dead* > Alive -> NA
#' - add rows for the forgotten censuses between 2 'Alive' if chosen
#' - Alive > *Dead*/*NA* > Alive -> Alive
#' - Alive > *NA* > Dead -> NA
#' - Alive > *Dead* > NA -> Dead
#'
#' - Alive > *NA* > *NA*:
#'   if DeathConfirmation > unseens -> NA
#'   if DeathConfirmation =< unseens -> Dead
#'
#' @return Fill the *Comment* column with error type informations. If
#'   *DetectOnly* = FALSE, add a *LifeStatus_TreeDataCor* column with the
#'   corrected trees life status.
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
#' selection <- c("101184", "101433","101435","101436")
#'
#'# Write the sequence
#' TestData <- TestData[order(Year)] # arrange years in ascending order
#' TestData[IdTree == "101184", LifeStatus := c(TRUE, TRUE, TRUE, TRUE, FALSE)]
#' TestData[IdTree == "101433", LifeStatus := c(FALSE, TRUE, TRUE, TRUE, TRUE)]
#' TestData[IdTree == "101435", LifeStatus := c(TRUE, TRUE, NA, FALSE, TRUE)]
#' TestData[IdTree == "101436", LifeStatus := c(TRUE, NA, NA, FALSE, NA)]
#'
#'
#' Rslt <- StatusCorrection(TestData[IdTree %in% selection],
#'                          InvariantColumns = c("Site",
#'                                               "Genus",
#'                                               "Species",
#'                                               "Family",
#'                                               "ScientificName"))
#'
#'
#' LifeStatusCorrectionPlot(Rslt)
#'
StatusCorrection <- function(
    Data,
    InvariantColumns = c("Site",
                         "Genus_TreeDataCor",
                         "Species_TreeDataCor",
                         "Family_TreeDataCor",
                         "ScientificName_TreeDataCor"),
    DeathConfirmation = 2,
    UseSize = FALSE,
    AddRowsForForgottenCensuses = TRUE,
    DetectOnly = FALSE,

    RemoveRBeforeAlive = FALSE,
    RemoveRAfterDeath = FALSE
){

  #### Arguments check ####
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(Data) | all(is.na(Data$IdStem))) &
     ("IdTree" %in% names(Data) & any(!is.na(Data$IdTree))) ){ ID <- "IdTree"

  }else{ ID <- "IdStem"}

  if(!any(c("IdStem", "IdTree") %in% names(Data)) | (all(is.na(Data$IdStem)) &  all(is.na(Data$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------

  # Plot column exists
  if (!"Plot" %in% names(Data)){
    stop("The column 'Plot' must be present in the dataset
    to add rows to the census where the plot was inventoried, where the tree was alive, but not recorded")
  }

  if(DetectOnly %in% FALSE){
    # InvariantColumns
    if (!inherits(InvariantColumns, "character"))
      stop("'InvariantColumns' argument must be of character class")
  }

  # DeathConfirmation
  if (!inherits(DeathConfirmation, "numeric"))
    stop("'DeathConfirmation' argument must be numeric")

  # UseSize/DetectOnly/RemoveRBeforeAlive/RemoveRAfterDeath
  if (!all(unlist(lapply(list(UseSize, DetectOnly, RemoveRBeforeAlive, RemoveRAfterDeath),
                         inherits, "logical"))))
    stop("The 'UseSize', 'DetectOnly', 'RemoveRBeforeAlive' and 'RemoveRAfterDeath' arguments
         of the 'SatusCorrection' function must be logicals")

  if(DetectOnly %in% FALSE){
    # Check if the InvariantColumns name exists in Data
    for(c in InvariantColumns){
      if (!c %in% names(Data)){ cc <- gsub("_TreeDataCor", "", c) # remove _TreeDataCor

      if (!cc %in% names(Data)){ # Col without - Cor exists?
        stop(paste("InvariantColumns argument must contain one or several column names (see help)."
                   ,cc,"is apparently not a dataset's column"))

      }else{ InvariantColumns[InvariantColumns == c] <- cc # If yes replace by the col name without cor
      warning("",c," column does't exist. ",cc," column is therefore considered as InvariantColumns instead of ",c,"")

      }
      } # if c doest exist
    } # end c loop
  }

  # UseSize-Diameter
  if(UseSize %in% TRUE){ # if it is desired (TRUE) to use the presence of measurement to consider the tree alive
    if (!"Diameter" %in% names(Data)){
      stop("If you wish to use the size presence (UseSize=TRUE) as a witness of the living status of the tree,
           the 'Diameter' column must be present in the dataset")
    }
  }

  #### Function ####

  # data.frame to data.table
  setDT(Data)

  Data[, (ID) := as.character(get(ID))]


  # Order IDs and times in ascending order
  Data <- Data[order(get(ID), Year)]

  # IDs vector
  Ids <- as.vector(na.omit(unique(Data[, get(ID)]))) # Tree Ids

  # Dataset with the rows without ID
  DataIDNa <-  Data[is.na(get(ID))]

  # censuses per plot
  Data[, all_plot_censuses := paste(unique(na.omit(Year)), collapse = ", "), .(Plot)]

  ## apply status correction per ID (tree or stem)
  Data <- Data[, StatusCorrectionByTree(
    DataTree = Data[get(ID) == KeyCol],
    PlotCensuses = as.numeric(strsplit(unique(all_plot_censuses), ", ")[[1]]),
    InvariantColumns = InvariantColumns,
    DeathConfirmation = DeathConfirmation,
    UseSize = UseSize,
    AddRowsForForgottenCensuses = AddRowsForForgottenCensuses,
    DetectOnly = DetectOnly,
    RemoveRBeforeAlive = RemoveRBeforeAlive,
    RemoveRAfterDeath = RemoveRAfterDeath
  ),
  .(KeyCol = get(ID))]

  # remove unnecessary column
  Data$KeyCol <- NULL

  # Re-put the the rows without ID
  Data <- rbindlist(list(Data, DataIDNa), use.names=TRUE, fill=TRUE)

  if(DetectOnly %in% FALSE){
    # Rename correction columns
    setnames(Data, "LifeStatusCor", gsub("Cor", "_TreeDataCor", "LifeStatusCor"))
  }


  return(Data)

}



#' StatusCorrectionByTree
#'
#' @description Detect errors or detect errors and correct the tree life status
#'   evolution over the censuses.
#'   Inspired by the code of Nino Page package (ForestData::correct_alive() and
#'   .correct_alive_tree())
#'
#' @param DataTree A dataset corresponding to a single tree's (1 IdTree/IdStem)
#'   measurements (data.frame or data.table)
#'   The *LifeStatus* column must be coded as:
#'     - TRUE = alive,
#'     - FALSE = dead,
#'     - NA = unseen
#'
#' @param PlotCensuses Census years for the plot in which the tree is (numeric
#'   or integer)
#'
#' @param InvariantColumns Vector with the names of the columns that are
#'   supposed to have always the same value for each measurement of the same
#'   tree (character). It is recommended to use the columns that have already
#'   been **corrected**, such as the columns containing the corrected botanical
#'   information.
#'
#' @param DeathConfirmation Number of times (censuses) needed for an unseen tree
#'   to be considered dead (numeric)
#'
#' @param UseSize Use the size presence as a witness of the living status of the
#'   tree (logical)
#'
#' @param AddRowsForForgottenCensuses TRUE: adds rows for forgotten censuses
#'   between 2 'Alive', FALSE: does not add any rows (logical)
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
#' - if UseSize : if Diameter != NA -> Alive
#' - *Dead* > Alive -> NA
#' - add rows for the forgotten censuses between 2 'Alive' if chosen
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
#' AddR[, Year := c(2012:2015)] # the rows to add
#'
#' DataTree <- rbindlist(list(DataTree, AddR)) # add rows
#'
#' DataTree <- DataTree[order(Year)] # arrange years in ascending order
#'
#' # Write the sequence
#' DataTree[, LifeStatus := c(FALSE, TRUE, NA, FALSE, TRUE, NA, NA, FALSE, NA)]
#'
#' Rslt <- StatusCorrectionByTree(DataTree, PlotCensuses = c(2011:2021),
#'                                InvariantColumns = c("Site",
#'                                                     "Genus",
#'                                                     "Species",
#'                                                     "Family",
#'                                                     "ScientificName"))
#' setnames(Rslt, "LifeStatusCor", "LifeStatus_TreeDataCor")
#' LifeStatusCorrectionPlot(Rslt)
#'
StatusCorrectionByTree <- function(
    DataTree,
    PlotCensuses,
    InvariantColumns = c("Site",
                         "GenusCor",
                         "SpeciesCor",
                         "FamilyCor",
                         "ScientificNameCor"),
    DeathConfirmation = 2,
    UseSize = FALSE,
    AddRowsForForgottenCensuses = TRUE,
    DetectOnly = FALSE,

    RemoveRBeforeAlive = FALSE,
    RemoveRAfterDeath = FALSE
){

  #### Arguments check ####
  # DataTree
  if (!inherits(DataTree, c("data.table", "data.frame")))
    stop("DataTree must be a data.frame or data.table")

  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(DataTree) | all(is.na(DataTree$IdStem))) &
     ("IdTree" %in% names(DataTree) & any(!is.na(DataTree$IdTree))) ){ ID <- "IdTree"

  }else{ ID <- "IdStem"}

  if(!any(c("IdStem", "IdTree") %in% names(DataTree)) | (all(is.na(DataTree$IdStem)) &  all(is.na(DataTree$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------


  # PlotCensuses
  if (!inherits(PlotCensuses, c("numeric", "integer")))
    stop("'PlotCensuses' argument must be numeric or integer")

  if(DetectOnly %in% FALSE){
    # InvariantColumns
    if (!inherits(InvariantColumns, "character"))
      stop("'InvariantColumns' argument must be of character class")
  }

  # DeathConfirmation
  if (!inherits(DeathConfirmation, "numeric"))
    stop("'DeathConfirmation' argument must be numeric")

  # UseSize/DetectOnly/RemoveRBeforeAlive/RemoveRAfterDeath
  if(!all(unlist(lapply(list(UseSize, DetectOnly, RemoveRBeforeAlive, RemoveRAfterDeath),
                        inherits, "logical"))))
    stop("The 'UseSize', 'DetectOnly', 'RemoveRBeforeAlive' and 'RemoveRAfterDeath' arguments
         of the 'StatusCorrectionByTree' function must be logicals")

  # if there are several IDs
  if(length(unique(DataTree[,get(ID)])) != 1){
    stop("DataTree must correspond to only 1 same tree so 1 same ",ID,"
    (the ",ID,": " ,paste0(unique(DataTree[,get(ID)]), collapse = "/"),")")
  }

  # if there are several plots for the same ID
  if(length(as.vector(na.omit(unique(DataTree$Plot)))) != 1){
    stop(paste0("Tree ",unique(DataTree[,get(ID)])," has multiple plots: " ,paste0(unique(DataTree$Plot), collapse = "/")))
  }

  if(DetectOnly %in% FALSE){
    # Check if the InvariantColumns name exists in DataTree
    for(c in InvariantColumns){
      if(!c %in% names(DataTree)){
        stop(paste("InvariantColumns argument must contain one or several column names (see help)."
                   ,c,"is apparently not a dataset's column"))
      }
    }
  }

  # UseSize-Diameter column
  if(UseSize %in% TRUE){ # if it is desired (TRUE) to use the presence of measurement to consider the tree alive
    if(!"Diameter" %in% names(DataTree)){
      stop("If you wish to use the size presence (UseSize=TRUE) as a witness of the living status of the tree,
           the 'Diameter' column must be present in the dataset")
    }
  }


  #### Function ####

  # print(unique(DataTree[, get(ID)])) # to debug

  # data.frame to data.table
  setDT(DataTree)

  # Arrange year in ascending order
  DataTree <- DataTree[order(Year)] # order de dt

  if(DetectOnly %in% FALSE){
    DataTree[, LifeStatusCor := LifeStatus] # we will work on a new col and keep the original col intact
  }

  #### Use the size presence as a witness of the living status of the tree ####
  if(UseSize){

    DataTree <- GenerateComment(DataTree,
                                condition = !is.na(DataTree[, Diameter]) &
                                  !DataTree[,LifeStatus] %in% TRUE,
                                comment = "A measured tree is a living tree")


    if(DetectOnly %in% FALSE){
      DataTree[!is.na(Diameter), LifeStatusCor := TRUE]
    }

  }

  #### Sequence analyse ####
  # if tree has ever been recorded alive
  if(any(DataTree$LifeStatusCor %in% TRUE)){
    # The first Alive record year
    FirstAliveYear <- min(DataTree[LifeStatusCor %in% TRUE, Year], na.rm = TRUE)
  }

  if(AddRowsForForgottenCensuses == TRUE){

    #### Absents (logical vector of the PlotCensuses length) #### En DetectOnly, je renvoie qqchose ? quoi ? ####

    PlotCensuses <- sort(PlotCensuses) # increasing order

    if(any(DataTree$LifeStatusCor %in% TRUE)){

      # if tree has ever been recorded dead
      if(any(DataTree$LifeStatusCor %in% FALSE)){
        # The last time where the tree has been recorded dead (in case there are several)
        LastDeathRecord <- max(DataTree[LifeStatusCor %in% FALSE, Year], na.rm = TRUE)

        After <- which(DataTree$Year > LastDeathRecord) # After the last death record

        # If there is any "Alive" report after last reported death
        if(any(DataTree$LifeStatusCor[After] %in% TRUE)) {
          # Absents are the absent record years among the plot censuses from the 1st alive record
          Absents <- (PlotCensuses > FirstAliveYear & !PlotCensuses %in% DataTree$Year)

        }else{
          # Absents are the absent record years between first alive record and the last death record
          Absents <- (PlotCensuses > FirstAliveYear &
                        PlotCensuses < LastDeathRecord & # death = the end
                        !PlotCensuses %in% DataTree$Year)
        }

      }else{ # if tree has not been reported dead yet

        # Absents are the absent record years among the plot censuses from the 1st alive record
        Absents <- (PlotCensuses > FirstAliveYear & !PlotCensuses %in% DataTree$Year)

      }

      # if no one alive
    }else{

      # La j'ai choisi de ne rajouter les lignes absentes qu'entre le census min et max de l'arbre
      # Si tout est FALSE effectivement ça ne sert à rien de rajouter des lignes apres, mais des lignes avant ? il risque d'en avoir beaucoup, et on ne pourra mettre qu'NA
      # Pour tout est NA, ça aurait un intéret de rajouter des lignes avant-après ?
      Absents <- (PlotCensuses > min(DataTree$Year, na.rm = TRUE) & # entre les bornes, pas avnt pas après
                    PlotCensuses < max(DataTree$Year, na.rm = TRUE) &
                    !PlotCensuses %in% DataTree$Year)

    }

    # En DetectOnly, je renvoie qqchose ? quoi ? ########

    #### Creating rows for absents ####
    if(DetectOnly %in% FALSE){

      Nabs <- sum(Absents) # absent is a logical vector giving the census times for which trees were not seen.

      if(Nabs > 0){ # if there are absents
        # if(DataTree$Plot[1] == 1) print(DataTree$Plot[1])
        NewRow <- data.table(ID = unique(DataTree[,get(ID)]),     # the ID
                             LifeStatus = NA,                    # not seen
                             LifeStatusCor = NA,               # no corrected status for now
                             Plot = unique(DataTree$Plot),  # the unique plot in DataTree
                             Subplot = unique(DataTree$Subplot),  # the unique subplot in DataTree
                             stringsAsFactors =  FALSE)      # do not convert characters into factors

        setnames(NewRow, "ID", ID)

        if(length(InvariantColumns) > 0){ # if there are invariant columns

          NewRow[,(InvariantColumns) := NA] # empty the invariant columns for the added rows

          # Fill in the invariant columns in the added rows
          NewRow <- FillinInvariantColumns(NewRow = NewRow,
                                           InvariantColumns = InvariantColumns,
                                           DataTree = DataTree,
                                           IdTree = unique(DataTree[,get(ID)]))
        }

        # Multiply this new row the number of times as well as the number of absents
        NewRows <- do.call("rbind", replicate(n = Nabs, NewRow, simplify = FALSE))
        NewRows[, Year := PlotCensuses[Absents]]

        # Add these rows in the dataset
        DataTree <- rbindlist(list(DataTree, NewRows), use.names=TRUE, fill=TRUE)

        DataTree <- DataTree[order(Year)] # order by time

      } # end: Nabsents > 0
    }

  } # end AddRowsForForgottenCensuses

  #### Alive > *Alive* > Alive ####
  if(any(DataTree$LifeStatusCor %in% TRUE)){

    # First/last alive positions (rows id)
    FirstAlive <- which(DataTree$LifeStatusCor %in% TRUE)[1] # the 1st seen alive
    LastAlive <-  max(which(DataTree$LifeStatusCor %in% TRUE)) # the last seen alive


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
  # if(any(DataTree$LifeStatusCor %in% NA)){
  if(any(DataTree$LifeStatusCor %in% TRUE)){


    # If there are things after the last occurrence of life
    if(LastAlive != nrow(DataTree)){ # if the last seen alive is not the last row of the database

      #### if the one after the last one seen alive is Dead and it's not the last row ####
      if((DataTree[LastAlive +1, LifeStatusCor] %in% FALSE) & (LastAlive +1 != nrow(DataTree))){
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
          DataTree <- GenerateComment(
            DataTree,
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
  # } # any NA ?


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
#' @param DataTree A dataset corresponding to a single tree/stem (1
#'   IdTree/IdStem) measurements, with the invariant columns and their value
#'   (data.table)
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

  # j = "ScientificNameCor"
  for(j in InvariantColumns){

    if(any(is.na(NewRow[,get(j)]))){ # if the column is empty in the new rows (the "absent" trees)

      uni <- unique(DataTree[, get(j)])

      if(length(uni) > 1){ # if the "invariant column is not invariant
        stop("The variable ",
             j,
             " that you defined as a non-varying column -i.e. supposed to have always the same value for each measurement of the same tree- has multiple values for tree/stem '",
             IdTree,
             "' and takes the values ",
             uni)
        # }
        # else if(is.na(uni) | length(uni) == 0){ # no value in the invariant column
        #   stop("The variable ",j," has no value for individual '",IdTree,"'")
      }
      else if(!is.na(uni) | length(uni) != 0){
        NewRow[is.na(get(j)), (j) := uni] # fill the invariant column in NewRow with their (unique) value
      }
    }
  }

  return(NewRow)
}

