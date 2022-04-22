#' DiameterCorrection
#'
#' @param Data (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - 'DBH'
#'   - **'HOM'(Height Of Measurement) (numeric)** if you want to apply the
#'       **"taper" correction**
#'   - **'POM'(Point Of Measurement) (factor)** if you want to correct from the
#'       **"POM change**
#'
#' @param DefaultHOM Default Height Of Measurement in meter (Default: 1.3 m)
#'   (numeric, 1 value)
#'
#' @param MinDBH Minimum census DBH of your protocol in cm (numeric, 1 value)
#' @param MaxDBH Maximum possible DBH of your stand in cm (numeric, 1 value)
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
#' @param Pioneers (characters vector)
#' @param PioneersGrowthThreshold in cm (numeric, 1 value)
#'
#' @param TrustMeasSet Trust measurements set: the "first" or the "last" set
#'   (character, 1 value)
#' @param WhatToCorrect  c("POM change", "punctual", "shift") (character)
#'   - "POM change": detect POM change in the column 'POM' or 'HOM' and correct
#'                   the DBH values from it.
#'   - "punctual": detect if the error is punctual and correct it by
#'                 interpolation.
#'   - "shift": detect if there is a shift of several DBH values and links them
#'              to the trust measurements set (*TrustMeasSet* argument).
#'
#' @param CorrectionType c("taper", "linear", "quadratic", "individual",
#'   "phylogenetic hierarchical") (character).
#'   - "taper": correct for biases associated with nonstandard and changing
#'              measurement heights, from a taper model (*TaperParameter* &
#'              *TaperFormula* arguments).
#'              Correction possible only if the 'HOM' (Height Of Measurement)
#'              column is available.
#'   - "linear": interpolation by linear regression of the individual annual
#'               growth over time.
#'   - "quadratic": interpolation by quadratic regression  of the individual
#'               annual growth over time.
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
#' @param Digits Number of decimal places to be used in the 'DBHCor' column
#'   (Default: 1L) (integer)
#'
#' @param TaperParameter Taper parameter (unitless) formula (function)
#' Default: *TaperParameter = 0.156 - 0.023 log(DAB) - 0.021 log(HOM)*
#' of Cushman et al.2021.
#' With:
#'   - *DAB*: Diameter Above Buttress (in cm)
#'   - *HOM*: Height Of Measurement (in m)
#'
#' @param TaperFormula Taper formula (function)
#' Default: *DAB / (2 e^(- TaperParameter (HOM - DefaultHOM)))*
#' of Cushman et al.2021.
#' With:
#'   - *DAB*: Diameter Above Buttress (in cm)
#'   - *HOM*: Height Of Measurement (in m)
#'   - *DefaultHOM*:  Default Height Of Measurement (in m)
#'   - *TaperParameter*: Taper parameter (unitless)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (Default: FALSE) (logical)
#'
#' @return Fill the *Comment* column with error type informations. If
#'   *DetectOnly* = FALSE, add columns: - *DBHCor*: corrected trees diameter at
#'   default HOM - *DiameterCorrectionMeth* =
#'   "taper"/"linear"/"quadratic"/"individual"/"phylogenetic hierarchical"
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#'
#' # Rslt <- DiameterCorrection(
#' #  TestData,
#' #  CorrectionType = c("quadratic",
#' #                     "individual", "phylogenetic hierarchical"))
#'
DiameterCorrection <- function(
  Data,

  DefaultHOM = 1.3,
  MinDBH = 10,
  MaxDBH = 500,
  PositiveGrowthThreshold = 5,
  NegativeGrowthThreshold = -2,

  Pioneers = c("Cecropia","Pourouma"),
  PioneersGrowthThreshold = 7.5,

  TrustMeasSet = "first",
  WhatToCorrect = c("POM change", "punctual", "shift"),
  CorrectionType = c("taper", "quadratic", "linear", "individual", "phylogenetic hierarchical"),

  DBHRange = 10,
  MinIndividualNbr = 5,
  Digits = 1L,

  TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  TaperFormula = function(DAB, HOM, TaperParameter) DAB / (2 * exp(- TaperParameter*(HOM - DefaultHOM))),


  DetectOnly = FALSE
){

  #### Arguments check ####
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # DBH column exists
  if(!"DBH" %in% names(Data))
    stop("The 'DBH' (Diameter at Breast Height) column does't exist in the dataset")

  # DefaultHOM/Min-MaxDBH/Positive-Negative-PioneersGrowthThreshold/DBHRange/MinIndividualNbr (numeric, 1 value)
  if(!all(unlist(lapply(list(DefaultHOM, MinDBH, MaxDBH,
                             PositiveGrowthThreshold, NegativeGrowthThreshold, PioneersGrowthThreshold,
                             DBHRange, MinIndividualNbr),
                        length)) %in% 1) |
     !all(unlist(lapply(list(PositiveGrowthThreshold, NegativeGrowthThreshold, DefaultHOM, PioneersGrowthThreshold),
                        inherits, c("numeric", "integer")))))
    stop("The 'PositiveGrowthThreshold', 'NegativeGrowthThreshold', 'PioneersGrowthThreshold' and 'DefaultHOM' arguments
         of the 'DiameterCorrection' function must be 1 numeric value each")

  # Pioneers (characters vector)
  if(!inherits(Pioneers, "character"))
    stop("'Pioneers' argument must be a characters vector")

  # TrustMeasSet
  if(length(TrustMeasSet) != 1 |
     !any(TrustMeasSet == "first" || TrustMeasSet == "last"))
    stop("The 'TrustMeasSet' argument value must be equal to 'first' or 'last'")

  # WhatToCorrect
  if(!any(WhatToCorrect == "POM change" || WhatToCorrect == "punctual"|| WhatToCorrect == "shift"))
    stop("The 'WhatToCorrect' argument value must be among 'POM change', 'punctual' and 'shift'")

  # CorrectionType
  if(!any(CorrectionType == "taper" || CorrectionType == "quadratic"|| CorrectionType == "linear"||
          CorrectionType == "individual"|| CorrectionType == "phylogenetic hierarchical"))
    stop("The 'CorrectionType' argument value must be among
         'taper', 'quadratic', 'linear', 'individual' and 'phylogenetic hierarchical'")

  # DefaultHOM
  if(!inherits(DefaultHOM, "numeric"))
    stop("The 'DefaultHOM' argument must be numeric")

  # Digits
  if(!inherits(Digits, "integer"))
    stop("The 'Digits' argument must be an integer (put an 'L' after your number (eg. 1L)) ")

  # DetectOnly (logical)
  if(!inherits(DetectOnly, "logical"))
    stop("The 'DetectOnly' argument must be a logical")

  # In data.table
  setDT(Data)

  #### Function ####

  # Order IdTrees and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(IdTree, Year)]

  # IdTrees vector --------------------------------------------------------------------------------------------------------
  Ids <- as.vector(na.omit(unique(Data$IdTree))) # Tree Ids

  # Dataset with the rows without IdTree ----------------------------------------------------------------------------------
  DataIDNa <-  Data[is.na(IdTree)]

  # Apply for all the trees -----------------------------------------------------------------------------------------------
  # i = "100635"
  Data <- do.call(rbind, lapply(Ids, function(i) DiameterCorrectionByTree(
    DataTree = Data[IdTree %in% i], # per IdTree, all censuses
    Data = Data,

    DefaultHOM = DefaultHOM,
    MinDBH = MinDBH,
    MaxDBH = MaxDBH,
    PositiveGrowthThreshold = PositiveGrowthThreshold,
    NegativeGrowthThreshold = NegativeGrowthThreshold,

    Pioneers = Pioneers,
    PioneersGrowthThreshold = PioneersGrowthThreshold,

    TrustMeasSet = TrustMeasSet,
    WhatToCorrect = WhatToCorrect,
    CorrectionType = CorrectionType,

    DBHRange = DBHRange,
    MinIndividualNbr = MinIndividualNbr,
    Digits = Digits,

    TaperParameter = TaperParameter,
    TaperFormula = TaperFormula,
    DetectOnly = DetectOnly
  )
  )) # do.call apply the 'rbind' to the lapply result

  # Re-put the the rows without IdTree ------------------------------------------------------------------------------------
  Data <- rbindlist(list(Data, DataIDNa), use.names=TRUE, fill=TRUE)


  return(Data)

}

#' DiameterCorrectionByTree
#'
#' @param DataTree A dataset corresponding to a single tree's (1 IdTree)
#'   measurements (data.frame or data.table).
#'
#' @param Data Complete dataset (data.table)
#'
#' @param DefaultHOM Default Height Of Measurement in meter (Default: 1.3 m)
#'   (numeric, 1 value)
#'
#' @param MinDBH Minimum census DBH of your protocol in cm (numeric, 1 value)
#' @param MaxDBH Maximum possible DBH of your stand in cm (numeric, 1 value)
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
#' @param Pioneers (characters vector)
#' @param PioneersGrowthThreshold in cm (numeric, 1 value)
#'
#' @param TrustMeasSet Trust measurements set: the "first" or the "last" set
#'   (character, 1 value)
#' @param WhatToCorrect  c("POM change", "punctual", "shift") (character)
#'   - "POM change": detect POM change in the column 'POM' or 'HOM' and correct
#'                   the DBH values from it.
#'   - "punctual": detect if the error is punctual and correct it by
#'                 interpolation.
#'   - "shift": detect if there is a shift of several DBH values and links them
#'              to the trust measurements set (*TrustMeasSet* argument).
#'
#' @param CorrectionType c("taper", "linear", "quadratic", "individual",
#'   "phylogenetic hierarchical") (character).
#'   - "taper": correct for biases associated with nonstandard and changing
#'              measurement heights, from a taper model (*TaperParameter* &
#'              *TaperFormula* arguments).
#'              Correction possible only if the 'HOM' (Height Of Measurement)
#'              column is available.
#'   - "linear": interpolation by linear regression of the individual annual
#'               growth over time.
#'   - "quadratic": interpolation by quadratic regression  of the individual
#'               annual growth over time.
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
#' @param Digits Number of decimal places to be used in the 'DBHCor' column
#'   (Default: 1L) (integer)
#'
#' @param TaperParameter Taper parameter (unitless) formula (function)
#' Default: *TaperParameter = 0.156 - 0.023 log(DAB) - 0.021 log(HOM)*
#' of Cushman et al.2021.
#' With:
#'   - *DAB*: Diameter Above Buttress (in cm)
#'   - *HOM*: Height Of Measurement (in m)
#'
#' @param TaperFormula Taper formula (function)
#' Default: *DAB / (2 e^(- TaperParameter (HOM - DefaultHOM)))*
#' of Cushman et al.2021.
#' With:
#'   - *DAB*: Diameter Above Buttress (in cm)
#'   - *HOM*: Height Of Measurement (in m)
#'   - *DefaultHOM*:  Default Height Of Measurement (in m)
#'   - *TaperParameter*: Taper parameter (unitless)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (Default: FALSE) (logical)
#'
#' @return Fill the *Comment* column with error type informations. If
#'   *DetectOnly* = FALSE, add columns: - *DBHCor*: corrected trees diameter at
#'   default HOM - *DiameterCorrectionMeth* =
#'   "taper"/"linear"/"quadratic"/"individual"/"phylogenetic hierarchical"
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#' # DataTree = Data[IdTree %in% 101433]
#'
#' DataTree <- data.table(IdTree = "c",
#'       Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 DBH values
#'       DBH = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
#'       POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
#'       HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))
#'
#' Rslt <- DiameterCorrectionByTree(DataTree, TestData)
#'
DiameterCorrectionByTree <- function(
  DataTree,
  Data,

  DefaultHOM = 1.3,
  MinDBH = 10,
  MaxDBH = 500,
  PositiveGrowthThreshold = 5,
  NegativeGrowthThreshold = -2,

  Pioneers = c("Cecropia","Pourouma"),
  PioneersGrowthThreshold = 7.5,

  TrustMeasSet = "first",
  WhatToCorrect = c("POM change", "punctual", "shift"),
  CorrectionType = c("taper", "quadratic", "linear", "individual", "phylogenetic hierarchical"),

  DBHRange = 10,
  MinIndividualNbr = 5,
  Digits = 1L,

  TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  TaperFormula = function(DAB, HOM, TaperParameter) DAB / (2 * exp(- TaperParameter*(HOM - DefaultHOM))),


  DetectOnly = FALSE
){

  #### Arguments check ####
  # DataTree
  if (!inherits(DataTree, c("data.table", "data.frame")))
    stop("DataTree must be a data.frame or data.table")

  # if there are several IdTrees
  if(length(unique(DataTree$IdTree)) != 1){
    stop("DataTree must correspond to only 1 same tree so 1 same IdTree
    (the IdTrees: " ,paste0(unique(DataTree$IdTree), collapse = "/"),")")
  }

  # In data.table
  setDT(DataTree)

  #### Function ####

  # print(unique(DataTree[, IdTree])) # to debug

  # Arrange year in ascending order
  DataTree <- DataTree[order(Year)] # order de dt

  # Taper correction ------------------------------------------------------------------------------------------------------
  if("taper" %in% CorrectionType) {
    DataTree <- TaperCorrection(DataTree,
                                DefaultHOM = DefaultHOM,
                                TaperParameter = TaperParameter, TaperFormula = TaperFormula,
                                Digits = Digits,
                                DetectOnly = DetectOnly)
  }


  # Correction with POM ---------------------------------------------------------------------------------------------------
  if("POM change" %in% WhatToCorrect){
    ## POM change detection -----------------------------------------------------------------------------------------------
    if (any(!is.na(DataTree$POM))) { # POM exists?

      raised = which(diff(c(NA, DataTree$POM)) == 1) # Detection des changements de POM (1ere val = NA car ) (1 = changement de POM)
      if(length(raised) != 0){ # if there are POM changes

        ## 1. DBH[init shift] ---------------------------------------------------------------------------------------------
        if("individual" %in% CorrectionType) {
          # Estcresc <- RegressionInterpolation()
          # DBH[init shift] =  previous value + Estcresc

        }

        if(!"individual"%in% CorrectionType & "phylogenetic hierarchical" %in% CorrectionType){
          # Colleaguescresc <- PhylogeneticHierarchicalCorraction()
          # DBH[init shift] =  previous value + mean(Colleaguescresc)
        }
        ## 2. DBH[shift] --------------------------------------------------------------------------------------------------
        # DBH[shift] = previous value + their cresc
        ## 3. + trunk width reduction factor (if POM change (only?)) ------------------------------------------------------

      }# if there are POM changes
    }# if there are POMs
  }# Correction with POM



  # Punctual error detection  + replace with NA ---------------------------------------------------------------------------
  if("punctual" %in% WhatToCorrect){
    # PunctualErrorDetection()
  }



  # Shift Correction ------------------------------------------------------------------------------------------------------
  if("shift" %in% WhatToCorrect){
    ## Shift detection si PunctualErrorDetection() ne s'en est pas chargé -------------------------------------------------
    ## 1. DBH[init shift] -------------------------------------------------------------------------------------------------
    if("individual" %in% CorrectionType) {
      # Estcresc <- RegressionInterpolation()
      # DBH[init shift] =  previous value + Estcresc

    }

    if(!"individual"%in% CorrectionType & "phylogenetic hierarchical" %in% CorrectionType){
      # Colleaguescresc <- PhylogeneticHierarchicalCorraction()
      # DBH[init shift] =  previous value + mean(Colleaguescresc)
    }
    ## 2. DBH[shift] ------------------------------------------------------------------------------------------------------
    # DBH[shift] = previous value + their cresc
    ## 3. + trunk width reduction factor (if POM change (only?)) ----------------------------------------------------------
  }


  if("punctual" %in% WhatToCorrect){
    # Replace NA by the correction ------------------------------------------------------------------------------------------
    # Estcresc <- RegressionInterpolation()
    # DBH[error] =  previous value + Estcresc
  }



  return(DataTree)
}


