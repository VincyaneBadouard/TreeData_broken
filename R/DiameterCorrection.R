#' Diameter correction
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdTree` (character)
#'   - `IdStem` (character) if *ByStem* argument = TRUE
#'   - `ScientificName` (character)
#'   - `Diameter` (numeric)
#'   - `Year` (numeric)
#'   - **`HOM` (Height Of Measurement) (numeric)** if you want to apply the
#'       **"taper" correction**
#'   - **`POM` (Point Of Measurement) (factor)** if you want to correct from the
#'       **"POM change"**
#'   If you want to apply the **"phylogenetic hierarchical"** correction, the
#'   dataset must also contain the columns:
#'   - `Genus` (character)
#'   - `Family` (character)
#'
#' @param ByStem must be equal to TRUE if your inventory contains the stem
#'   level, equal to FALSE if not, and in this case the correction is done by
#'   tree (logical)
#'
#' @param DefaultHOM Default Height Of Measurement in meter (Default: 1.3 m)
#'   (numeric, 1 value)
#'
#' @param MinDBH Minimum census DBH (Diameter at the default HOM) of your
#'   protocol in cm (numeric, 1 value)
#' @param MaxDBH Maximum possible DBH (Diameter at the default HOM) of your
#'   stand in cm (numeric, 1 value)
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
#'   the `ScientificName` column (characters vector)
#'
#' @param PioneersGrowthThreshold in cm/year: a tree of a pioneer species that
#'   widens by more than this value is considered abnormal (numeric, 1 value)
#'
#' @param TrustMeasSet Trust measurements set: the "first" or the "last" set
#'   (character, 1 value) (not implemented yet)
#'
#' @param WhatToCorrect Possible values: "POM change", "punctual", "shift"
#'   (character)
#'   - "POM change": detect POM change in the column `POM` and correct the
#'                   Diameter values from it.
#'   - "punctual": detect if the error is punctual and correct it by
#'                 interpolation.
#'   - "shift": detect if there is a shift of several Diameter values and
#'              links them to the trust measurements set
#'              (*TrustMeasSet* argument).
#'
#' @param CorrectionType Possible values: "taper", "linear", "quadratic",
#'   "individual", "phylogenetic hierarchical" (character).
#'   - "taper": correct for biases associated with nonstandard and changing
#'              measurement heights, from a taper model (*TaperParameter* &
#'              *TaperFormula* arguments).
#'              Correction possible only if the `HOM` (Height Of Measurement)
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
#' @param Digits Number of decimal places to be used in the `DBHCor` column
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
#' # Remove other errors types (non-unique idTree, missing Year)
#' TestData <- TestData[!IdTree %in% c("100898", "101686")]
#'
#' Rslt <- DiameterCorrection(
#'  TestData,
#'   WhatToCorrect = c("POM change", "punctual", "shift"),
#'     CorrectionType = c("quadratic", "phylogenetic hierarchical"))
#'
DiameterCorrection <- function(
  Data,

  ByStem = FALSE,

  DefaultHOM = 1.3,
  MinDBH = 10,
  MaxDBH = 500,
  PositiveGrowthThreshold = 5,
  NegativeGrowthThreshold = -2,

  Pioneers = NULL,
  PioneersGrowthThreshold = 7.5,

  TrustMeasSet = c("first", "last"),
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

  # Diameter column exists
  if(!"Diameter" %in% names(Data))
    stop("The 'Diameter' (Diameter at Breast Height) column does't exist in the dataset")

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
  if(!inherits(Pioneers, "character") & !is.null(Pioneers))
    stop("'Pioneers' argument must be a characters vector, or NULL")

  # TrustMeasSet
  TrustMeasSet <- match.arg(TrustMeasSet, choices = c("first", "last"))

  # WhatToCorrect
  if(!any(WhatToCorrect %in% "POM change" | WhatToCorrect %in% "punctual"| WhatToCorrect %in% "shift"))
    stop("The 'WhatToCorrect' argument value must be among 'POM change', 'punctual' and 'shift'")

  # CorrectionType
  if(!any(CorrectionType  %in%  "taper" | CorrectionType %in% "quadratic"| CorrectionType %in% "linear"|
          CorrectionType %in% "individual"| CorrectionType %in% "phylogenetic hierarchical"))
    stop("The 'CorrectionType' argument value must be among
         'taper', 'quadratic', 'linear', 'individual' and 'phylogenetic hierarchical'")

  # Digits
  if(!inherits(Digits, "integer"))  {
     warning(paste0("The 'Digits' argument must be an integer. Value entered (", Digits, ")  coerced to ", as.integer(Digits), "."))
    Digits <- as.integer(Digits)
  }

  # DetectOnly (logical)
  if(!inherits(DetectOnly, "logical"))
    stop("The 'DetectOnly' argument must be a logical")

  if(any(!is.na(Data$HOM)) & !any(CorrectionType  %in% "taper")) # HOM exists?
    message("You have the 'HOM' information in your dataset.
            We advise you to correct your diameters also with the 'taper' correction ('CorrectionType' argument)")

  if((all(is.na(Data$HOM)) | !"HOM" %in% names(Data)) &
     any(!is.na(Data$POM)) & !any(WhatToCorrect %in% "POM change")) # POM exists?
    message("You have the 'POM' information in your dataset.
            We advise you to correct your diameters also from the 'POM change' ('WhatToCorrect' argument)")

  # In data.table
  setDT(Data)

  Data <- unique(Data)   # if there are duplicate rows, delete them

  if(!"Comment" %in% names(Data)) Data[, Comment := ""]
  if(!"DiameterCorrectionMeth" %in% names(Data)) Data[, DiameterCorrectionMeth := ""]

  # If no diameter value, write a comment
  Data <- GenerateComment(Data,
                          condition = is.na(Data[, Diameter]),
                          comment = "Missing value in 'Diameter'")

  #### Function ####

  if(ByStem == TRUE){
    ID <- "IdStem"
  }else if (ByStem == FALSE) {
    ID <- "IdTree"
  }

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(get(ID), Year)]

  # IDs vector --------------------------------------------------------------------------------------------------------
  Ids <- as.vector(na.omit(unique(Data[, get(ID)]))) # Tree Ids

  # Dataset with the rows without IDS ----------------------------------------------------------------------------------
  DataIDNa <-  Data[is.na(get(ID))]

  # Apply for all the trees -----------------------------------------------------------------------------------------------
  # i = "100635"
  Data <- do.call(rbind, lapply(Ids, function(i) DiameterCorrectionByTree(
    DataTree = Data[get(ID) %in% i], # per ID, all censuses
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

  # Re-put the the rows without ID ------------------------------------------------------------------------------------
  Data <- rbindlist(list(Data, DataIDNa), use.names = TRUE, fill = TRUE)



  if(DetectOnly %in% FALSE){

    # Data[, POMcor := DefaultPOM] # Corrected diameter is at default POM

    Data[, HOMCor := DefaultHOM] # Corrected diameter is at default HOM

  }



  return(Data)

}

#' DiameterCorrectionByTree
#'
#' @param DataTree A dataset corresponding to a single tree/stem's (1 IdTree/IdStem)
#'   measurements (data.frame or data.table).
#'
#' @param Data Complete dataset (data.table) used if the "phylogenetic
#'   hierarchical" correction (*CorrectionType* argument) is chosen.
#'   The dataset must contain the columns:
#'   - 'IdTree' (character)
#'   - 'IdStem' (character) if *ByStem* argument = TRUE
#'   - 'ScientificName' (character)
#'   - 'Genus' (character)
#'   - 'Family' (character)
#'   - 'Diameter' (numeric)
#'   - 'Year' (numeric)
#'
#' @param DefaultHOM Default Height Of Measurement in meter (Default: 1.3 m)
#'   (numeric, 1 value)
#'
#' @param MinDBH Minimum census DBH of your protocol in cm (numeric, 1 value)
#' @param MaxDBH Maximum possible DBH of your stand in cm (numeric, 1 value)
#'
#' @param PositiveGrowthThreshold in cm/year: a tree
#'   widening by more than x cm/year is considered abnormal (numeric, 1 value)
#'
#' @param NegativeGrowthThreshold in cm/census: the possible
#'   positive measurement error (+n) cannot be corrected until the growth
#'   appears abnormal, but a negative measurement error can be allowed until -n
#'   (a tree does not decrease). Thus the positive measurement error (+n) is
#'   "compensated". (numeric, 1 value)
#'
#' @param Pioneers Scientific names of the pioneer species of the site, as in
#'   the 'ScientificName' column (characters vector)
#'
#' @param PioneersGrowthThreshold in cm/year: a tree of a pioneer species that
#'   widens by more than x cm/year is considered abnormal (numeric, 1 value)
#'
#' @param TrustMeasSet Trust measurements set: the "first" or the "last" set
#'   (character, 1 value)
#' @param WhatToCorrect  c("POM change", "punctual", "shift") (character)
#'   - "POM change": detect POM change in the column 'POM' and correct
#'                   the Diameter values from it.
#'   - "punctual": detect if the error is punctual and correct it by
#'                 interpolation.
#'   - "shift": detect if there is a shift of several 'Diameter' values and
#'              links them to the trust measurements set
#'              (*TrustMeasSet* argument).
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
#'   *DetectOnly* = FALSE, add columns:
#'   - *DBHCor*: corrected trees diameter at default HOM
#'   - *DiameterCorrectionMeth* = "taper"/"linear"/"quadratic"/"individual"
#'   /phylogenetic hierarchical("species"/"genus"/"family"/"stand"/"shift
#'   realignment")
#'
#'@importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#'
#'  DataTree <- data.table(IdTree = "c",
#'       ScientificName = "A",
#'       Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
#'       Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
#'       POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
#'       HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))
#'
#' Rslt <- DiameterCorrectionByTree(
#'   DataTree, TestData,
#'   WhatToCorrect = c("POM change", "punctual", "shift"),
#'   CorrectionType = c("quadratic", "linear", "individual")
#'   )
#'
DiameterCorrectionByTree <- function(
  DataTree,
  Data,

  DefaultHOM = 1.3,
  MinDBH = 10,
  MaxDBH = 500,
  PositiveGrowthThreshold = 5,
  NegativeGrowthThreshold = -2,

  Pioneers = NULL,
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
  if(length(unique(DataTree$IdTree)) != 1 & length(unique(DataTree$IdStem)) != 1){
    stop("DataTree must correspond to only 1 same tree/stem so 1 same IdTree/IdStem
    (the IdTrees: " ,paste0(unique(DataTree$IdTree), collapse = "/"),",
     the IdStems: " ,paste0(unique(DataTree$IdStem), collapse = "/"),")")
  }

  # In data.table
  setDT(DataTree)

  # print(unique(DataTree[, IdTree])) # to debug

  # If not enough Diameter values
  if(sum(!is.na(DataTree$Diameter)) > 1){

    #### Function ####

    # Arrange year in ascending order
    DataTree <- DataTree[order(Year)] # order de dt

    DBHCor <- Diameter <- DataTree[, Diameter]
    Time <- DataTree[, Year]

    # Pioneers species case
    if(any(na.omit(unique(DataTree[, ScientificName]) == Pioneers))){ # if it's a pioneer species

      PositiveGrowthThreshold <- PioneersGrowthThreshold # take the Pioneers growth threshold
    }

    # DBH = 0 is impossible
    DBHCor[DBHCor == 0] <- NA

    # DBH > MaxDBH -> DBH = NA
    DBHCor[DBHCor > MaxDBH] <- NA

    # Taper correction ------------------------------------------------------------------------------------------------------
    if("taper" %in% CorrectionType) {
      DataTree <- TaperCorrection(DataTree,
                                  DefaultHOM = DefaultHOM,
                                  TaperParameter = TaperParameter, TaperFormula = TaperFormula,
                                  DetectOnly = DetectOnly)

      DBHCor <- DataTree[, DBHCor]
    }


    # Correction with POM ---------------------------------------------------------------------------------------------------
    if("POM change" %in% WhatToCorrect){
      ## POM change detection -----------------------------------------------------------------------------------------------
      if(any(!is.na(DataTree$POM))) { # POM exists?

        raised = which(diff(c(NA, DataTree$POM)) == 1) # POM change detection (1ere val = NA because it's the default POM) (1 = changement de POM)
        if(length(raised) != 0){ # if there are POM changes

          DataTree <- GenerateComment(DataTree,
                                      condition = as.numeric(rownames(DataTree)) %in% (raised),
                                      comment = paste0("POM change"))

          if(DetectOnly %in% FALSE){

            # Compute diameter incrementation without the inits shift
            cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
            cresc_abs <- ComputeIncrementation(Var = DBHCor, Type = "absolute", Time = Time)
            # Remove incr between 2 shifts (take growth only intra seq)
            cresc[raised-1]  <- NA # cresc[which(is.na(cresc))+1] <- NA
            cresc_abs[raised-1] <- NA # cresc_abs[which(is.na(cresc_abs))+1] <- NA

            # Put NA if other abnormal incrementation
            AbnormalCrescs <- (cresc >= PositiveGrowthThreshold | cresc_abs < NegativeGrowthThreshold)
            cresc[AbnormalCrescs]  <- NA
            cresc_abs[AbnormalCrescs]  <- NA

            if("individual" %in% CorrectionType) {

              ## 1. DBH[init shift] -------------------------------------------------------------------------------------------

              # Check that only non-abnormal growths are kept
              if(length(which(cresc[!is.na(cresc)] >= PositiveGrowthThreshold |
                              cresc_abs[!is.na(cresc_abs)] < NegativeGrowthThreshold)) == 0){

                # Replace NA by the correction --------------------------------------------------------------------------------
                cresc_Corr <- RegressionInterpolation(Y = cresc, X = Time[-1], CorrectionType = CorrectionType) # Compute the corrected cresc

                for(rs in 1:length(raised)){  # as many rs as POM changes
                  # DBH[init shift] = previous value + Estimated cresc
                  DBHCor[raised[rs]] <- DBHCor[raised[rs]-1] + cresc_Corr[raised[rs]-1]*diff(Time)[raised[rs]-1] # Correct with the corrected cresc, the corrected DBH

                  # Add the column with the correction method  ------------------------------------------------------------------------
                  if("quadratic" %in% CorrectionType & length(which(!is.na(Diameter))) > 3){

                    Meth <- "quadratic"

                  }else{

                    Meth <-  "linear"
                  }

                  DataTree <- GenerateComment(DataTree,
                                              condition = as.numeric(rownames(DataTree)) %in% (raised[rs]),
                                              comment = Meth,
                                              column = "DiameterCorrectionMeth")

                  if(length(DBHCor) > (raised[rs])){ # if the init shift is not the last diameter value

                    ## 2. DBH[shift] --------------------------------------------------------------------------------------------
                    # If NA in cresc_abs replace it by a interpolation value
                    cresc_abs_Corr <- RegressionInterpolation(Y = cresc_abs, X = Time[-1], CorrectionType = CorrectionType) # Compute the corrected cresc

                    for(i in (raised[rs]+1): min(raised[rs+1]-1, length(DBHCor), na.rm = TRUE)){ # i = each value in a shift
                      # DBH[shift] = previous value + their cresc_abs
                      DBHCor[i] <- # then correct the other shift values
                        DBHCor[i-1] + # New position of the previous value
                        cresc_abs_Corr[i-1] #  cresc_abs of the value we are correcting, not recalculated

                      # Add the column with the correction method  ------------------------------------------------------------------------
                      # DataTree[i, DiameterCorrectionMeth := "shift realignment"]
                      DataTree <- GenerateComment(DataTree,
                                                  condition = as.numeric(rownames(DataTree)) %in% (i),
                                                  comment = "shift realignment",
                                                  column = "DiameterCorrectionMeth")

                    } # end i loop

                  } # end : if the init shift is not the last diameter value

                } # end rs loop

              }else{stop("There are still abnormal growths not detected upstream (method to be improved)")}

            }

            if(!"individual"%in% CorrectionType & "phylogenetic hierarchical" %in% CorrectionType){

              DataTree <- PhylogeneticHierarchicalCorrection(
                DataTree = DataTree,
                Data = Data,
                cresc = cresc, cresc_abs = cresc_abs, cresc_abn = raised-1,
                DBHCor = DBHCor, Time = Time,
                PositiveGrowthThreshold = PositiveGrowthThreshold,
                NegativeGrowthThreshold = NegativeGrowthThreshold,
                DBHRange = DBHRange, MinIndividualNbr = MinIndividualNbr)

              DBHCor <- DataTree[,DBHCor]
            }

            ## 3. + trunk width reduction factor (if POM change (only?)) ------------------------------------------------------

          } # End correction "POM change"

        }# if there are POM changes
      }# if there are POMs
    }# Correction with POM



    # Punctual/shift error detection  + replace with NA if punctual ---------------------------------------------------------
    if(any("punctual" %in% WhatToCorrect | "shift" %in% WhatToCorrect)){
      DBHCor <- PunctualErrorDetection(
        DBHCor = DBHCor, Time = Time,
        PositiveGrowthThreshold = PositiveGrowthThreshold, NegativeGrowthThreshold = NegativeGrowthThreshold,
        TrustMeasSet = TrustMeasSet,
        DetectOnly = DetectOnly)
      # ça serait bien de renvoyer qqchose si un shift est detecté pour être plus secure (y refléchir)

      if("DBHCor" %in% names(DataTree)){
        DataTree[, DBHCor := NULL] # remove the DBHCor col to avoid conflict
      }

      DataTree[,DBHCor := DBHCor]

      DataTree <- GenerateComment(DataTree,
                                  condition = (is.na(DataTree[,DBHCor]) & !is.na(DataTree[,Diameter])),
                                  comment = paste0("Abnormal diameter value (punctual error)"))

      if(DetectOnly %in% TRUE) DataTree[,DBHCor := NULL] # remove the DBHCor col if we detect only
    }

    # Shift Correction ------------------------------------------------------------------------------------------------------
    if("shift" %in% WhatToCorrect){
      ## Init shift detection si PunctualErrorDetection() ne s'en est pas chargé --------------------------------------------
      ### Compute diameter incrementation without the inits shift
      cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
      cresc_abs <- ComputeIncrementation(Var = DBHCor, Type = "absolute", Time = Time)

      ### Detect abnormal growth --------------------------------------------------------------------------------------------
      cresc_abn <- which(cresc >= PositiveGrowthThreshold | cresc_abs < NegativeGrowthThreshold) # abnormal values indices
      # le retour à la normale est considéré comme une erreur (perte excessive)

      if(length(cresc_abn) != 0) { # if there are abnormal values

        if("DBHCor" %in% names(DataTree)){
          DataTree[, DBHCor := NULL] # remove the DBHCor col to avoid conflict
        }

        DataTree[,DBHCor := DBHCor]

        DataTree <- GenerateComment(DataTree,
                                    condition = as.numeric(rownames(DataTree)) %in% (cresc_abn+1),
                                    comment = paste0("Abnormal diameter value (shift error)"))

        if(DetectOnly %in% TRUE) DataTree[,DBHCor := NULL] # remove the DBHCor col if we detect only


        if(DetectOnly %in% FALSE){

          # Remove incr between 2 shifts (take growth only intra seq)
          cresc[cresc_abn] <- NA
          cresc_abs[cresc_abn] <- NA

          if("individual" %in% CorrectionType) {

            ## 1. DBH[init shift] -----------------------------------------------------------------------------------------------

            # Check that only non-abnormal growths are kept
            if(length(which(cresc[!is.na(cresc)] >= PositiveGrowthThreshold | cresc_abs[!is.na(cresc_abs)] < NegativeGrowthThreshold))==0){

              # Replace NA by the correction ---------------------------------------------------------------------------------
              cresc_Corr <- RegressionInterpolation(Y = cresc, X = Time[-1], CorrectionType = CorrectionType) # Compute the corrected cresc

              for(rs in 1:length(cresc_abn)){  # as many rs as POM changes
                # DBH[init shift] = previous value + Estimated cresc
                DBHCor[cresc_abn[rs]+1] <- DBHCor[cresc_abn[rs]] + cresc_Corr[cresc_abn[rs]]*diff(Time)[cresc_abn[rs]] # Correct with the corrected cresc, the corrected DBH

                # Add the column with the correction method  ------------------------------------------------------------------------
                if("quadratic" %in% CorrectionType & length(which(!is.na(Diameter))) > 3){
                  Meth <- "quadratic"
                }else{
                  Meth <- "linear"
                }

                DataTree <- GenerateComment(DataTree,
                                            condition = as.numeric(rownames(DataTree)) %in% (cresc_abn[rs]+1),
                                            comment = Meth,
                                            column = "DiameterCorrectionMeth")

                ## 2. DBH[shift] --------------------------------------------------------------------------------------------
                for(i in (cresc_abn[rs]+2): min(cresc_abn[rs+1], length(DBHCor), na.rm = TRUE)){ # i = each value in a shift
                  # DBH[shift] = previous value + their cresc_abs

                  # If NA in cresc_abs replace it by a interpolation value
                  cresc_abs_Corr <- RegressionInterpolation(Y = cresc_abs, X = Time[-1], CorrectionType = CorrectionType) # Compute the corrected cresc

                  DBHCor[i] <- # then correct the other shift values
                    DBHCor[i-1] + # New position of the previous value
                    cresc_abs_Corr[i-1] #  cresc_abs of the value we are correcting, not recalculated

                  # Add the column with the correction method  ------------------------------------------------------------------------
                  # DataTree[i, DiameterCorrectionMeth := "shift realignment"]

                  DataTree <- GenerateComment(DataTree,
                                              condition = as.numeric(rownames(DataTree)) %in% (i),
                                              comment = "shift realignment",
                                              column = "DiameterCorrectionMeth")


                }
              }

            }else{stop("There are still abnormal growths not detected upstream (method to be improved)")}
          }

          if(!"individual"%in% CorrectionType & "phylogenetic hierarchical" %in% CorrectionType){
            DataTree <- PhylogeneticHierarchicalCorrection(
              DataTree = DataTree,
              Data = Data,
              cresc = cresc, cresc_abs = cresc_abs, cresc_abn = cresc_abn,
              DBHCor = DBHCor, Time = Time,
              PositiveGrowthThreshold = PositiveGrowthThreshold,
              NegativeGrowthThreshold = NegativeGrowthThreshold,
              DBHRange = DBHRange, MinIndividualNbr = MinIndividualNbr)

            DBHCor <- DataTree[,DBHCor]
          }

          ## 3. + trunk width reduction factor (if POM change (only?)) ----------------------------------------------------------
        } # End shift correction
      }
    }


    if(DetectOnly %in% FALSE & "punctual" %in% WhatToCorrect & any(is.na(DBHCor))){ # Na to be replaced

      # Compute diameter incrementation without the abnormal values
      cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
      cresc_abs <- ComputeIncrementation(Var = DBHCor, Type = "absolute", Time = Time)

      # Put NA if other abnormal incrementation
      AbnormalCrescs <- (cresc >= PositiveGrowthThreshold | cresc_abs < NegativeGrowthThreshold)
      cresc[AbnormalCrescs]  <- NA
      cresc_abs[AbnormalCrescs]  <- NA

      # Check that only non-abnormal growths are kept
      if(length(which(cresc[!is.na(cresc)] >= PositiveGrowthThreshold | cresc_abs[!is.na(cresc_abs)] < NegativeGrowthThreshold))==0){

        # Replace NA by the correction ------------------------------------------------------------------------------------------
        cresc_Corr <- RegressionInterpolation(Y = cresc, X = Time[-1], CorrectionType = CorrectionType) # Compute the corrected cresc

        for(i in which(is.na(DBHCor))){ # i the abnormal DBH
          # DBH[error] = previous value + Estimated cresc

          if(i == 1){ DBHCor[i] <- DBHCor[i+1] - cresc_Corr[i]*diff(Time)[i] # if we correct the 1st dbh value, take the next one and subtracts the corrected growth

          }else{ DBHCor[i] <- DBHCor[i-1] + cresc_Corr[i-1]*diff(Time)[i-1]} # Correct with the corrected cresc, the corrected DBH

          # Add the column with the correction method  ------------------------------------------------------------------------
          if("quadratic" %in% CorrectionType & length(which(!is.na(Diameter))) > 3){
            Meth <- "quadratic"
          }else{
            Meth <- "linear"
          }

          DataTree <- GenerateComment(DataTree,
                                      condition = as.numeric(rownames(DataTree)) %in% (i),
                                      comment = Meth,
                                      column = "DiameterCorrectionMeth")

        }
      }else{warning("There are still abnormal growths. Either the selected methods are insufficient
                    or the method needs to be improved")}

    }

    if(DetectOnly %in% FALSE){
      # Check that there are no more abnormal growths -----------------------------------------------------------------------------
      cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
      cresc_abs <- ComputeIncrementation(Var = DBHCor, Type = "absolute", Time = Time)

      if(any(na.omit(cresc >= PositiveGrowthThreshold | cresc_abs < NegativeGrowthThreshold))){

        if("IdStem" %in% names(DataTree)){
          ID <- unique(DataTree[, IdStem])
        }else{
          ID <- unique(DataTree[, IdTree])
        }

        warning("There are still abnormal growths for the tree/stem ", ID,". Either the selected methods are insufficient
                    or the method needs to be improved")
      }
    }








    # 'DBHCor' vector in DataTree -------------------------------------------------------------------------------------------
    if(DetectOnly %in% FALSE){

      if("DBHCor" %in% names(DataTree)){
        DataTree[, DBHCor := NULL] # remove the DBHCor col to avoid conflict
      }

      DataTree[, DBHCor := round(DBHCor, digits = Digits)] }

  }else if (sum(!is.na(DataTree$Diameter)) < 2 & DetectOnly %in% FALSE){ # if only 1 dbh value
    DataTree[, DBHCor := NA]
  }

  return(DataTree)
}

