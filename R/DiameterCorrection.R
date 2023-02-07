#' Diameter correction
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdTree` or `IdStem` (character)
#'   - `ScientificName_TreeDataCor` (character)
#'   - `Diameter` (numeric)
#'   - `Year` (numeric)
#'   - **`POM` (Point Of Measurement) (factor)** or
#'     **`HOM` (Height Of Measurement) (numeric)** if you want to correct from
#'      the **"POM change"**
#'   If you want to apply the **"phylogenetic hierarchical"** correction, the
#'   dataset must also contain the columns:
#'   - `Genus_TreeDataCor` (character)
#'   - `Family_TreeDataCor` (character)
#'
#' @param UseTaperCorrection (logical) TRUE: transform the tree diameter measured at a given height
#' into the diameter corresponding to the default measurement height (`DefaultHOM`), using an allometry.
#'   FALSE: do not apply a taper correction
#'
#' @param DefaultHOM Default Height Of Measurement in meter (Default: 1.3 m)
#'   (numeric, 1 value)
#'
#' @param TaperParameter Taper parameter (unitless) formula (function)
#' Default: *TaperParameter = 0.156 - 0.023 log(DAB) - 0.021 log(HOM)*
#' of Cushman et al.2021.
#' With:
#'   - *DAB*: Diameter Above Buttress (in cm)
#'   - *HOM*: Height Of Measurement (in m)
#'
#' @param TaperFormula Taper formula (function)
#' Default: *DAB / (e^(- TaperParameter (HOM - DefaultHOM)))*
#' of Cushman et al.2021.
#' With:
#'   - *DAB*: Diameter Above Buttress (in cm)
#'   - *HOM*: Height Of Measurement (in m)
#'   - *DefaultHOM*:  Default Height Of Measurement (in m)
#'   - *TaperParameter*: Taper parameter (unitless)
#'
#' @param KeepMeas In case of **multiple diameter measurements** in the same
#'   census year:
#' Possible values: "MaxHOM", "MaxDate" (character).
#'   - "MaxHOM": apply the correction to the measurement taken at the
#'               **highest POM**
#'   - "MaxDate": apply the correction to the **most recent measurement** (same
#'                year but more recent date)
#'
 # param MaxDBH Maximum possible DBH (Diameter at the default HOM) of your
 # stand in cm (numeric, 1 value)
#'
#' @param PositiveGrowthThreshold in cm/year: a tree
#'   widening by more than this value is considered abnormal (numeric, 1 value)
#'
#' @param NegativeGrowthThreshold in cm/census: the possible
#'   positive measurement error (+n) cannot be corrected until the growth
#'   appears abnormal, but a negative measurement error can be allowed until -n
#'   (a tree does not decrease). Thus the positive measurement error (+n) is
#'   "compensated". (numeric, 1 value)
#'
#' @param Pioneers Scientific names of the pioneer species of the site, as in
#'   the `ScientificName_TreeDataCor` column (characters vector)
#'
#' @param PioneersGrowthThreshold in cm/year: a tree of a pioneer species that
#'   widens by more than this value is considered abnormal (numeric, 1 value)
#'
#' @param WhatToCorrect Possible values: "POM change", "punctual", "shift"
#'   (character)
#'   - "POM change": detect POM change in the column `POM` and correct the
#'                   Diameter values from it. (Ignored if taper correction is applied)
#'   - "punctual": detect if the error is punctual and correct it by
#'                 interpolation.
#'   - "shift": detect if there is a shift of several Diameter values and
#'              links them to the 1st measurements set.
#'
#' @param CorrectionType Possible values: "individual", "phylogenetic
#'   hierarchical" (character, 1 value).
#'   - "individual": replace abnormal growth by interpolation from the
#'                   individual values.
#'   - "phylogenetic hierarchical": replace abnormal growth with the average
#'          growth of other trees in the dataset, at the specific, genus, family
#'          or stand level, within a DBH range of x cm (*DBHRange* argument).
#'          If the number of these trees < n (*MinIndividualNbr* argument)
#'          at the specific level, we switch to the genus level etc.
#'
#' @param DBHRange DBH range in cm to take into account to select other trees in
#'   the dataset to apply "phylogenetic hierarchical" correction (Default: 10
#'   cm) (numeric, 1 value)
#' @param MinIndividualNbr Minimum number of individuals to take into account in
#'   "phylogenetic hierarchical" correction (Default: 5) (numeric, 1 value)
#'
#' @param OtherCrit Other criteria to select the individuals used for the
#'   calculation of the mean growth in the "phylogenetic hierarchical"
#'   correction. Give the name of the column(s) for which the individuals must
#'   have the same value as the tree to correct (e.g. c("Plot", "Subplot"))
#'   (character)
#'
#' @param Digits Number of decimal places to be used in the `DBHCor` column
#'   (Default: 1L) (integer)
#'
#' @param DBHCorForDeadTrees (logical) TRUE: return DBHCor also for dead trees.
#'   FALSE: do not return DBHCor for dead trees. In this case it is advisable to
#'   have corrected the tree life status with the *StatusCorrection()* function.
#'
#' @param coef description... (numeric)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (Default: FALSE) (logical)
#'
#' @return Fill the *Comment* column with error type informations. If
#'   *DetectOnly* = FALSE, add columns:
#'   - *Diameter_TreeDataCor*: corrected trees diameter at default HOM
#'   - *DiameterCorrectionMeth* = "local linear regression","weighted
#'       mean"/phylogenetic hierarchical("species"/"genus"/"family"/"stand")/
#'       "shift realignment"/"Same value".
#'   - *POM_TreeDataCor* (factor): POM value at which the corrected diameters are proposed.
#'       Corresponds to the 1st POM value at which the stem was measured.
#'   - *HOM_TreeDataCor* (numeric): HOM value at which the corrected diameters
#'       are proposed. Corresponds to the 1st HOM value at which the stem was
#'       measured.
#'
#' @details When there is only 1 `Diameter` value for a tree/stem,
#'   `Diameter_TreeDataCor` takes the original `Diameter` value. If this value
#'   is 0 or > MaxDBH, `Diameter_TreeDataCor` takes NA. Diameters not linked to
#'   an IdTree/IdStem or to a Census Year are not processed.
#'   Punctual error correction only with linear regression and not quadratic,
#'   because punctual errors are corrected from a local regression with the 2
#'   framing values.
#'
#' @importFrom utils capture.output
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' # library(data.table)
#' data(TestData)
#'
#' TestData$HOM <- 1.3
#' TestData$HOM[1:3] <- c(0.5,1.5,NA)
#'
#' Rslt <- DiameterCorrection(
#'  TestData,
#'   WhatToCorrect = c("POM change", "punctual", "shift"),
#'     CorrectionType = c("individual"),
#'     MinIndividualNbr = 1, Digits = 2L)
#'
#' DiameterCorrectionPlot(Rslt, OnlyCorrected = TRUE)
#'
DiameterCorrection <- function(
    Data,

    UseTaperCorrection = TRUE,
    DefaultHOM = 1.3,
    TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
    TaperFormula = function(DAB, HOM, TaperParameter, DefaultHOM) DAB / (exp(- TaperParameter*(HOM - DefaultHOM))),


    KeepMeas = c("MaxHOM", "MaxDate"),

    # MaxDBH = 500,
    PositiveGrowthThreshold = 5,
    NegativeGrowthThreshold = -2,

    Pioneers = NULL,
    PioneersGrowthThreshold = 7.5,

    WhatToCorrect = c("POM change", "punctual", "shift"),
    CorrectionType = c("individual", "phylogenetic hierarchical"),

    DBHRange = 10,
    MinIndividualNbr = 5,
    OtherCrit = NULL,
    Digits = 1L,

    DBHCorForDeadTrees = TRUE,

    coef = 0.9,

    DetectOnly = FALSE
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

  # Diameter column exists
  if(!"Diameter" %in% names(Data))
    stop("The 'Diameter' column does't exist in the dataset")

  # DefaultHOM/Min-MaxDBH/Positive-Negative-PioneersGrowthThreshold/DBHRange/MinIndividualNbr (numeric, 1 value)
  if(!all(unlist(lapply(list(DefaultHOM,
                             PositiveGrowthThreshold, NegativeGrowthThreshold, PioneersGrowthThreshold,
                             DBHRange, MinIndividualNbr),
                        length)) %in% 1) |
     !all(unlist(lapply(list(PositiveGrowthThreshold, NegativeGrowthThreshold, DefaultHOM, PioneersGrowthThreshold),
                        inherits, c("numeric", "integer")))))
    stop("The 'PositiveGrowthThreshold', 'NegativeGrowthThreshold', 'PioneersGrowthThreshold' and 'DefaultHOM' arguments
         of the 'DiameterCorrection' function must be 1 numeric value each")

  # Pioneers (characters vector)
  if(!inherits(Pioneers, "character") & !is.null(Pioneers))
    stop("'Pioneers' argument must be a characters vector, or NULL")

  # WhatToCorrect
  WhatToCorrect <- match.arg(WhatToCorrect, several.ok = TRUE)

  # CorrectionType
  CorrectionType <- match.arg(CorrectionType)

  # Digits
  if(!inherits(Digits, "integer") & Digits != as.integer(Digits))  {
    warning(paste0("The 'Digits' argument must be an integer. Value entered (", Digits, ")  coerced to ", as.integer(Digits), "."))
    Digits <- as.integer(Digits)
  }

  # DetectOnly (logical)
  if(!inherits(DetectOnly, "logical"))
    stop("The 'DetectOnly' argument must be a logical")

  # Taper before if 'HOM' in the dataset and 'UseTaperCorrection' = F
  if(any(!is.na(Data$HOM)) %in% names(Data) & !UseTaperCorrection) # HOM exist and UseTaperCorrection FALSE
    if(length(unique(na.omit((Data$HOM)))) > 1) message("You have the 'HOM' information in your dataset.
            We advise you to correct your diameters also with UseTaperCorrection = TRUE") # only show if there are varying HOM

  # If 'POM' 'POM change' correction is advised
  if((all(is.na(Data$HOM)) | !"HOM" %in% names(Data)) &
     any(!is.na(Data$POM)) & !"POM change" %in% WhatToCorrect) # POM exists?
    message("You have the 'POM' information in your dataset.
            We advise you to correct your diameters also from the 'POM change' ('WhatToCorrect' argument)")

  # 'POM change' correction needs 'POM' or 'HOM' values
  if(!any(c("POM", "HOM") %in% names(Data)) | (all(is.na(Data$POM)) &  all(is.na(Data$HOM))) )
    stop("You have chosen to make a 'POM change' correction,
       but you do not have the necessary 'POM' or HOM' column in your dataset or they are empty")

  # if 'Pioneers', need ScientificName

  if(!is.null(Pioneers) & PioneersGrowthThreshold != PositiveGrowthThreshold){

    ## ScientificName_TreeDataCor or ScientificName?
    if("ScientificName_TreeDataCor" %in% names(Data)){
      SfcName <- "ScientificName_TreeDataCor"

    }else if(!"ScientificName_TreeDataCor" %in% names(Data) & "ScientificName" %in% names(Data)){
      SfcName <- "ScientificName"

    }else if(!any(c("ScientificName_TreeDataCor", "ScientificName") %in% names(Data)))

      stop("There are no 'ScientificName_TreeDataCor' nor 'ScientificName' column.
           It is not possible to take into account the pioneer character of species in the diameter correction.
             If you do not want to take into account the pioneer character in the diameter correction,
             leave the argument Pioneers = NULL.")

  } # end Pioneers criteria



  # In data.table
  setDT(Data)
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment


  Data <- unique(Data)   # if there are duplicate rows, delete them

  # Dataset with the dead trees if no correction wanted for them --------------------------------------------
  if("LifeStatus_TreeDataCor" %in% names(Data)){ Status <- "LifeStatus_TreeDataCor"
  }else if ("LifeStatusCor" %in% names(Data)){ Status <- "LifeStatusCor"
  }else if ("LifeStatus" %in% names(Data)){ Status <- "LifeStatus"
  }else{stop("You have chosen DBHCorForDeadTrees = FALSE.
             To apply this choice the dataset must contain the column
             'LifeStatus_TreeDataCor', 'LifeStatusCor' or 'LifeStatus'")}

  if(DBHCorForDeadTrees == FALSE){
    DeadTrees <- Data[get(Status) == FALSE]
    Data <- Data[get(Status) == TRUE | is.na(get(Status))] # AliveTrees
    # nrow(Data) == nrow(AliveTrees) + nrow(DeadTrees) # to check
  }

  # Remove duplicated measurements per Year because different POM or Date -----------------------------------
  CompleteData <- copy(Data)

  Data <- UniqueMeasurement(Data, KeepMeas = KeepMeas, ID = ID)

  DuplicatedRows <- CompleteData[!Data, on = .NATURAL] # rows removed

  # Remove duplicated measurements (randomly)
  # Data <- Data[!duplicated(Data[, list((get(ID), IdCensus)], fromLast = TRUE)] # keep the last measurement


  if(!"Comment" %in% names(Data)) Data[, Comment := ""]
  if(DetectOnly %in% FALSE){
    if(!"DiameterCorrectionMeth" %in% names(Data)) Data[, DiameterCorrectionMeth := ""]
  }

  # If no diameter value, write a comment
  Data[is.na(Diameter), Comment := GenerateComment(Comment, comment = "Missing value in 'Diameter'")]


  #### Function ####

  # Taper correction ------------------------------------------------------------------------------------------------------
  if(UseTaperCorrection) {
    Data <- TaperCorrection(Data,
                            DefaultHOM = DefaultHOM,
                            TaperParameter = TaperParameter, TaperFormula = TaperFormula,
                            DetectOnly = DetectOnly)

    # if there is a POM column, also bring that to the first value so no risk to readjust that again
    if("POM" %in% names(Data))  Data[, POM_TreeDataCor := .SD[1, POM], by = c(ID)]
  }

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(get(ID), IdCensus)]

  # IDs vector --------------------------------------------------------------------------------------------------------
  Ids <- as.vector(na.omit(unique(Data[, get(ID)]))) # Tree Ids

  # Dataset with the rows without IDS ----------------------------------------------------------------------------------
  DataIDNa <- Data[is.na(get(ID))]

  # Dataset with the rows without IdCensus ----------------------------------------------------------------------------------
  DataIdCensusNa <- Data[is.na(IdCensus)]

  # Apply Corrections -----------------------------------------------------------------------------------------------


  if(!"Diameter_TreeDataCor" %in% names(Data)) {
    Data[, Diameter_TreeDataCor := Diameter]
  }
  if(!"HOM_TreeDataCor" %in% names(Data)) {
    Data[, HOM_TreeDataCor := HOM]
  }
  if(!"POM_TreeDataCor" %in% names(Data)){
    Data[, POM_TreeDataCor := POM]
  }
  if(!"LifeStatus_TreeDataCor" %in% names(Data)) {
    Data[, LifeStatus_TreeDataCor := LifeStatus]
  }



  # get a DiameterHistory
  DiameterHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "Diameter_TreeDataCor", drop = FALSE)
  DiameterHistory <- as.matrix(DiameterHistory, 1)


  # get HOMHistory
  HOMHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "HOM_TreeDataCor", drop = FALSE)
  HOMHistory <- as.matrix(HOMHistory, 1)

  # get HOMChange History
  HOMChangeHistory <- cbind(NA, t(apply(HOMHistory, 1, diff)))
  colnames(HOMChangeHistory) <- colnames(HOMHistory)

  # get POMHistory
  POMHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "POM_TreeDataCor", drop = FALSE)
  POMHistory <- as.matrix(POMHistory, 1)

  # get POMChange History
  POMChangeHistory <-  t(apply(POMHistory, 1, function(x) x != shift(x, type = "lag")))

  # get DateHistory (to be able to calculate growth)
  DateHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "Date", drop = FALSE)
  DateHistory <- as.matrix(DateHistory, 1)

  # get DateDiff
  DateDiff <- matrix(NA, ncol = ncol(DateHistory), nrow = nrow(DateHistory), dimnames = dimnames(DiameterHistory))
  DateDiff[] <- t(apply(DateHistory, 1, function(x) (as.Date(x) - shift(as.Date(x)))/365))


  # get a MinDBHHistory (this is useful if not same threshold accross plots)
  MinDBHHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "MinDBH", drop = FALSE)
  MinDBHHistory <- as.matrix(MinDBHHistory, 1)
  MinDBHHistory[] <- apply(MinDBHHistory, 1, function(x) x[is.na(x)] <- as.numeric(names(which.max(table(x))))) # fill NA with most common MinDBH in the row . this could be a problem if MinDBH changes accross censuses but should be rare enough that it does not matter

  # get LifeStatusHistory
  LifeStatusHistory <- dcast(Data, get(ID) ~ IdCensus, value.var = "LifeStatus_TreeDataCor", drop = FALSE)
  LifeStatusHistory <- as.matrix(LifeStatusHistory, 1)

  # create Comment History
  CommentHistory <- matrix("", nrow(DiameterHistory), ncol(DiameterHistory), dimnames = dimnames(DiameterHistory))

  DiameterCorrectionMethHistory <- matrix("", nrow(DiameterHistory), ncol(DiameterHistory), dimnames = dimnames(DiameterHistory))

  # small correction affecting next histories
  # DBH = 0 is impossible
  DiameterHistory[DiameterHistory %in% 0] <- NA
  # DBH > MaxDBH -> DBH = NA
  # DiameterHistory[DiameterHistory > MaxDBH] <- NA



  # fill in the Diameter when the tree was missed

  MissedDiametetFilled <- t(mapply(function(d, t, c, l) {

    t <- as.Date(t)
    m <- lm(d~t)

    if(!is.na(coef(m)["t"])) { # if there is at least 2 diameters and corresponding dates... if not, no regression can be done
      p <- predict(lm(d~t), newdata = t) # if some t are NA, still i twil
      # p <- p[match(names(d), names(p))] # this is to account for when the date is NA
      c[is.na(d) & !is.na(p) & l %in% TRUE] <- GenerateComment(c[is.na(d) & !is.na(p)], "Initial linear interpolation for missed tree")
      d[is.na(d) & l %in% TRUE] <- p[is.na(d) & l %in% TRUE]
    }

    return(list(d, c))

  },
  d = split(DiameterHistory, row(DiameterHistory)),
  t = split(DateHistory, row(DateHistory)),
  c = split(CommentHistory, row(CommentHistory)),
  l = split(LifeStatusHistory, row(LifeStatusHistory))))

  DiameterHistory[] <- do.call(rbind, MissedDiametetFilled[,1])
  CommentHistory[] <-  do.call(rbind, MissedDiametetFilled[,2])

  # create a function that will allow to get diameter difference and growth history (because we will need to recalculate those a few times, as we make corrections)
  CalcGrowthHist <- function(DiameterHistory, DateHistory) {
    x <-  matrix(NA, ncol = ncol(DiameterHistory), nrow = nrow(DiameterHistory), dimnames = dimnames(DiameterHistory))
    x[,-1] <- t(round(apply(DiameterHistory, 1, diff) / apply(DateHistory, 1, function(x) diff(as.Date(x))/365), 2))
    return(x[])
  }

  CalcDiameterDiffHist <- function(DiameterHistory = DiameterHistory) {
    x <-  matrix(NA, ncol = ncol(DiameterHistory), nrow = nrow(DiameterHistory), dimnames = dimnames(DiameterHistory))
    x[,-1] <- t(round(apply(DiameterHistory, 1, diff),2))
    return(x[])
  }

  # get growth History (for annual growth incrementation)
  GrowthHistory <- CalcGrowthHist(DiameterHistory = DiameterHistory, DateHistory = DateHistory)

  # get Growth difference (for absolute growth incrementation)
  DiameterDiffHistory <- CalcDiameterDiffHist(DiameterHistory)

  # get Plot, Family, Genus and Specie in an array
  UniqueInfo <- Data[, .(Plot = unique(Plot),
          Family = unique(Family),
          Genus = unique(Genus),
          Species = unique(ScientificName)), by = .(IdStem =get(ID))]

  UniqueInfo <- UniqueInfo[complete.cases(UniqueInfo),] # removing when for some reason one of these info is not specified (which deals with most duplicated)

  if(any(duplicated(UniqueInfo$get))) stop("Some individuals don't have a unique Plot, Family, Genus or species")

  for(w in c("Plot", "Family", "Genus", "Species")) {
    x <- UniqueInfo[, get(w)]
    names(x) <- UniqueInfo$IdStem
    x <- x[rownames(DiameterHistory)] # make sure same order as other objecys
    assign(w, x)
  } # make one object for each info (will help later)



  # calculate weights to use with "individual correction"
  ## For each Census, compute the absolute time difference and use coefs to calculate weight of the growths by temporal proximity

  Weights <- lapply(1:ncol(DateHistory), function(j) matrix(exp(as.numeric(abs(as.Date(DateHistory) - as.Date(DateHistory[,j])))/365*-coef), nrow = nrow(DiameterHistory), ncol = ncol(DiameterHistory), dimnames = dimnames(DiameterHistory))) # list of length equal to number of censuses

  # Corrections ####
  Idx_enough_DBH <- rowSums(!is.na(DiameterHistory)) > 1
  Idx_one_DBH <- rowSums(!is.na(DiameterHistory)) %in% 1



  # detect and change to NA the growth of cases of abnormal increment

  ## positive

  if(!is.null(Pioneers)) {

    ### pioneers
    idx_sp <- Species %in% Pioneers
    idx = !is.na(GrowthHistory[idx_sp, ]) & GrowthHistory[idx_sp, ] >= PioneersGrowthThreshold

    CommentHistory[idx_sp, ][idx] <- GenerateComment(CommentHistory[idx_sp, ][idx], paste("Growth greated than threshold of", PioneersGrowthThreshold))
    GrowthHistory[idx_sp, ][idx]  <- NA
    DiameterDiffHistory[idx_sp, ][idx] <- NA

    ### non-pioneers
    idx_sp <- !Species %in% Pioneers
    idx = !is.na(GrowthHistory[idx_sp, ]) & GrowthHistory[idx_sp, ] >= PositiveGrowthThreshold

    CommentHistory[idx_sp, ][idx] <- GenerateComment(CommentHistory[idx_sp, ][idx], paste("Growth greated than threshold of", PositiveGrowthThreshold))
    GrowthHistory[idx_sp, ][idx]  <- NA
    DiameterDiffHistory[idx_sp, ][idx] <- NA

  } else {

    idx = !is.na(GrowthHistory) & GrowthHistory >= PositiveGrowthThreshold

    CommentHistory[idx] <- GenerateComment(CommentHistory[idx], paste("Growth greated than threshold of", PositiveGrowthThreshold))
    GrowthHistory[idx]  <- NA
    DiameterDiffHistory[idx] <- NA
  }


  ## negative
  idx = !is.na(GrowthHistory) & GrowthHistory < NegativeGrowthThreshold # Valentine decided to use GrowthHistory instead of DiameterDiffHistory

  CommentHistory[idx] <- GenerateComment(CommentHistory[idx], paste("Growth smaller than threshold of", NegativeGrowthThreshold))
  GrowthHistory[idx]  <- NA
  DiameterDiffHistory[idx] <- NA



  if("POM change" %in% WhatToCorrect){

    if(all(is.na(HOMHistory)) & all(is.na(POMHistory)))  stop("You have chosen to make a 'POM change' correction,
        but 'POM' and HOM' columns are empty for all trees so we can't apply corrections.")
}


    ## POM change detection -----------------------------------------------------------------------------------------------

    # Check the HOM value over the time

    idx = !is.na(HOMChangeHistory) & HOMChangeHistory < 0
    CommentHistory[idx] <- GenerateComment(CommentHistory[idx], "HOM decreased")

    idx = !is.na(HOMChangeHistory) & HOMChangeHistory > 0
    CommentHistory[idx] <- GenerateComment(CommentHistory[idx], "HOM increased")


    # Check the POM value over the time
    CommentHistory[POMChangeHistory %in% TRUE] <- GenerateComment(CommentHistory[idx], "POM changed")

    # detect any change in HOM or POM
    idxPOMChange <- !is.na(HOMChangeHistory) & !HOMChangeHistory %in% 0 | POMChangeHistory %in% TRUE

    # Remove growth between shifts (take growth only intra seq)
    GrowthHistory[idxPOMChange]  <- NA
    DiameterDiffHistory[idxPOMChange] <- NA

    # get indexes to replace
    idxPOMChange <- is.na(GrowthHistory) & idxPOMChange
    idxToReplace <- suppressWarnings(cbind(which(idxPOMChange, arr.ind = T), what = 1)) # 1 is for POM Change, 2 is for abnormal growth
    idxToReplace <- cbind(idxToReplace, sign = sign(CalcGrowthHist(DiameterHistory = DiameterHistory, DateHistory = DateHistory)[idxPOMChange])) # get the sign of the shift



    ## Abnormal growth detection (punctual and shift combined) -----------------------------------------------------------------------------------------------

    # get indexes to replace
    idxAbnormal <- idxPOMChange # this is to help maintaining the structure
    idxAbnormal[] <- grepl(paste("Growth greated than threshold", "Growth smaller than threshold", sep = "|"), CommentHistory) & !grepl(paste("HOM decreased", "HOM increased", "POM changed", sep = "|"), CommentHistory)

    idxToReplace <-  rbind(idxToReplace, suppressWarnings(cbind(which(idxAbnormal, arr.ind = T), what = 2, # 1 is for POM Change, 2 is for shift or punctual
                                                                sign = sign(CalcGrowthHist(DiameterHistory = DiameterHistory, DateHistory = DateHistory)[idxAbnormal])))) # get the sign of the shift



    # ## Leading NA in status (to see if we may be missing recruitments) -----------------------------------------------------------------------------------------------
    #
    # # get indexes to replace
    # idxLeadingNAs <- idxPOMChange # this is to help maintaining the structure
    # idxLeadingNAs[] <- LeadingNAHistory %in% 1
    #
    # idxToReplace <-  rbind(idxToReplace, suppressWarnings(cbind(which(idxLeadingNAs, arr.ind = T), what = 3, # 1 is for POM Change, 2 is for shift or punctual, 3 is for recruitment
    #                                                             sign = sign(CalcGrowthHist(DiameterHistory = DiameterHistory, DateHistory = DateHistory)[idxLeadingNAs])))) # get the sign of the shift
    # CAREFUL IF WE BRING THIS BACK IN THE FUNCTION WE NEED TO CHANGE idxToReplace <- idxToReplace[idxToReplace[, 2]> 1, ] to idxToReplace <- idxToReplace[idxToReplace[, 2]> 1 | idxToReplace[, 2] %in% 3, ] + keep working on the corrections(need to go backward instead of forward)


## Corrections -------------------------------------------------------------


    # remove cases where NA is in first column since that is fake data

    idxToReplace <- idxToReplace[idxToReplace[, 2]> 1, ]

    # remove cases that were not selected
    if(!"POM change" %in% WhatToCorrect) idxToReplace <- idxToReplace[!idxToReplace[,3] %in% 1, ]
    if(!"Abnormal growth" %in% WhatToCorrect) idxToReplace <- idxToReplace[!idxToReplace[,3] %in% 2, ]

    # replace Diameters that need it
    if(nrow(idxToReplace) > 0) {


      for(l in 1:nrow(idxToReplace)) {

        t = rownames(idxToReplace)[l]
        i = idxToReplace[l, 1]
        j = idxToReplace[l, 2]

         if(l > 1) if(idxToReplace[l, 3] %in% 2 & idxToReplace[l-1, 3] %in% 2 & t %in% rownames(idxToReplace)[l-1] & (j-1) %in% idxToReplace[l-1, 2] & idxToReplace[l,4] + idxToReplace[l-1,4] == 0) next # ignore this correction if the previous fix was a punctual error measurement and this abnormal growth is just the return to "normal"

        pd = DiameterHistory[i,j-1] # previous dbh

        SwitchToIndividual = FALSE # this will be turned to TRUE if CorrectionType %in% "phylogenetic hierarchical" but there are not enough colleagues

        if(CorrectionType %in% "phylogenetic hierarchical") {
          # find potential Colleagues


          idxSamePlot <- Plot %in% Plot[t]
          idxSameFamily <- Family %in% Family[t]
          idxSameGenus <- Genus %in% Genus[t]
          idxSameSpecies <- Species %in% Species[t]

          idxDBHWithinRange <- !is.na(DiameterHistory) & (DiameterHistory > (pd - DBHRange/2) & DiameterHistory < (pd + DBHRange/2))

          idxGrowthExists <- !is.na(GrowthHistory)

          # figure out set of colleague (need at least MinIndividualNbr, with actual growth measures)

          idxColleagues <- list(species = idxSameSpecies & idxDBHWithinRange & idxGrowthExists,
                                genus = idxSameGenus & idxDBHWithinRange & idxGrowthExists,
                                family = idxSameFamily & idxDBHWithinRange & idxGrowthExists,
                                plot = idxSamePlot & idxDBHWithinRange & idxGrowthExists)


          Method <- names(which(lapply(idxColleagues, sum, na.rm = T) > MinIndividualNbr))[1] # take the first set that meets the min requirement

          if(length(Method) > 0) { # if we can use phylo correction
            idxColleagues <- idxColleagues[[Method]]

            # compute Colleagues growth mean
            wg <- mean(GrowthHistory[idxColleagues])

            DiameterCorrectionMethHistory[i, j] <- GenerateComment( DiameterCorrectionMethHistory[i, j], paste(Method, "phylogenetic gowth mean"))

            SwitchToIndividual = FALSE
          } else {
          #   stop("Not enough individuals in your dataset to apply the 'phylogenetic hierarchical' correction even at the 'stand' level.
          #              You asked for a minimum of ", MinIndividualNbr," individuals ('MinIndividualNbr' argument).
          #               The 'individual' correction is applied in this case.")
          # }
            SwitchToIndividual = TRUE
          }

        }

        if(CorrectionType %in% "individual" | SwitchToIndividual) {


          w = Weights[[j]][t, ] # weight for a NA in that census, for that tree

          g = GrowthHistory[i,] # growth history of that tree

          wg <- weighted.mean(g, w, na.rm = T) # weighted mean of growth for that tree at that census

          DiameterCorrectionMethHistory[i, j] <- GenerateComment( DiameterCorrectionMethHistory[i, j], "Weighted mean")
        }



        do = DiameterHistory[i,j] # original DBH of that year

        DiameterHistory[i,j] <- pd + wg * DateDiff[i, j]

        dn = DiameterHistory[i,j] # new DBH of that year



        # apply switch to other values if j is not last column and l+1 in idxToReplace is not of same tree with opposite sign (which would indicated a punctual errro measurement)
        shift = FALSE # initialize with no shit
        if(j < ncol(DiameterHistory)) {
          if(l == nrow(idxToReplace))  {
            shift = TRUE # if this is the last abnormal growth, we know we need to shift because we already skipped it if it was a return to normwl
          } else {
            if((idxToReplace[l, 3] %in% 1 | (idxToReplace[l, 3] %in% 2 & idxToReplace[l+1, 3] %in% 2 & t %in% rownames(idxToReplace)[l+1] & (j+1) %in% idxToReplace[l+1, 2] & idxToReplace[l,4] + idxToReplace[l+1,4] != 0))) shit = TRUE
          }
        }

        if(shift) {
          DiameterHistory[i,(j+1):ncol(DiameterHistory)] <- DiameterHistory[i,(j+1):ncol(DiameterHistory)] + dn - do

          DiameterCorrectionMethHistory[i,(j+1):ncol(DiameterHistory)]  <- GenerateComment( DiameterCorrectionMethHistory[i,(j+1):ncol(DiameterHistory)] , paste("Shift realignment after", c("POM change", "Abnormal growth")[idxToReplace[l, 3]]))
        }


      }



    }


# If not enough Diameters or growth to help? --------------------------------------------------


# Check that there are no more abnormal growths -----------------------------------------------------------------------------

    GrowthHistory <- CalcGrowthHist(DiameterHistory = DiameterHistory, DateHistory = DateHistory)

    ## positive

    if(!is.null(Pioneers)) {

      ### pioneers
      idx_sp <- Species %in% Pioneers
      idx = !is.na(GrowthHistory[idx_sp, ]) & GrowthHistory[idx_sp, ] >= PioneersGrowthThreshold

      if(sum(idx)>1)  warning("There are still pioneers with abnormal positive growth (the selected methods are insufficient
                    or the method needs to be improved)")

      ### non-pioneers
      idx_sp <- !Species %in% Pioneers
      idx = !is.na(GrowthHistory[idx_sp, ]) & GrowthHistory[idx_sp, ] >= PositiveGrowthThreshold

      if(sum(idx)>1)  warning("There are still non-pioneers with abnormal positive growth (the selected methods are insufficient
                    or the method needs to be improved)")

    } else {

      idx = !is.na(GrowthHistory) & GrowthHistory >= PositiveGrowthThreshold
      if(sum(idx)>1)  warning("There are still individuals with abnormal positive growth (the selected methods are insufficient
                    or the method needs to be improved)")

    }

    ## negative
    idx = !is.na(GrowthHistory) & GrowthHistory < NegativeGrowthThreshold # Valentine decided to use GrowthHistory instead of DiameterDiffHistory
    if(sum(idx)>1)  warning("There are still individuals with abnormal negative growth (the selected methods are insufficient
                    or the method needs to be improved)" )

# Write changes in Data -------------------------------------------------------------------------------------------
if(!DetectOnly){

}




# Re-put the rows duplicated, or without ID or IdCensus -----------------------------------------------------------------
Data <- rbindlist(list(Data, DuplicatedRows, DataIDNa, DataIdCensusNa), use.names = TRUE, fill = TRUE)

# Re-put the dead trees in the dataset (if there are not corrected by choice)
if(DBHCorForDeadTrees == FALSE){
  Data <- rbindlist(list(Data, DeadTrees), use.names = TRUE, fill = TRUE)
}

# Order IDs and times in ascending order ----------------------------------------------------------------------------
Data <- Data[order(get(ID), IdCensus)]

if(DetectOnly %in% FALSE){
  # Rename correction columns
  setnames(Data, c("DBHCor", "POMCor", "HOMCor"),
           c("Diameter_TreeDataCor", "POM_TreeDataCor", "HOM_TreeDataCor"), skip_absent=TRUE)
} else {
  Data[, Diameter_TreeDataCor := NULL]
  Data[, HOM_TreeDataCor := NULL]
}


return(Data)

}


