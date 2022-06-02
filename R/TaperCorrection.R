#' TaperCorrection
#'
#' @param DataTree A dataset corresponding to a single tree's (1 IdTree)
#'   measurements (data.frame or data.table).
#'   The dataset must contain the columns:
#'   - 'Diameter'
#'   - **'HOM'(Height Of Measurement) (numeric)**
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
#'      - *DBHCor*: corrected trees diameter at default HOM
#'      - *DiameterCorrectionMeth* = "taper"
#'
#' @export
#'
#' @examples
#' library(data.table)
#'
#' DataTree <- data.table(IdTree = "c",
#'       Year = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020), # 9 Diameter values
#'       Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2), # 0.5 cm/year
#'       POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
#'       HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))
#'
#' Rslt <- TaperCorrection(DataTree)
#'
TaperCorrection <- function(
  DataTree,
  DefaultHOM = 1.3,

  TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  TaperFormula = function(DAB, HOM, TaperParameter) DAB / (2 * exp(- TaperParameter*(HOM - DefaultHOM))),

  DetectOnly = FALSE
){

  #### Arguments check ####
  # # DataTree
  # if (!inherits(DataTree, c("data.table", "data.frame")))
  #   stop("DataTree must be a data.frame or data.table")

  # Check if the HOM column exists
  if(!"HOM" %in% names(DataTree)){
    stop("You have chosen to make a 'taper' correction,
       but you do not have the necessary 'HOM' column in your dataset")
  }

  # if there are several IdTrees
  # if(length(unique(DataTree$IdTree)) != 1){
  #   stop("DataTree must correspond to only 1 same tree so 1 same IdTree
  #   (the IdTrees: " ,paste0(unique(DataTree$IdTree), collapse = "/"),")")
  # }

  # DefaultHOM
  if(!inherits(DefaultHOM, "numeric"))
    stop("The 'DefaultHOM' argument must be numeric")

  # TaperParameter/TaperFormula (function)
  if(!all(unlist(lapply(list(TaperParameter, TaperFormula),
                        inherits, "function"))))
    stop("The 'TaperParameter' and 'TaperFormula' arguments must be functions")

  # DetectOnly (logical)
  # if(!inherits(DetectOnly, "logical"))
  #   stop("The 'DetectOnly' argument must be a logical")

  # In data.table
  setDT(DataTree)

  if(any(DataTree[,HOM] > DefaultHOM)){ # if some measurements of the tree were made above the POM by default

    DataTree <- GenerateComment(DataTree,
                                condition = (DataTree[,HOM] > DefaultHOM),
                                comment = paste0("HOM different from the default HOM"))

    if(DetectOnly %in% FALSE){
      if(!"DBHCor" %in% names(DataTree))
        DataTree[, DBHCor := numeric(.N) ] # start without value (I can't put NA because it's a logical, so it's a 0)

      # Apply taper correction  -------------------------------------------------------------------------------------------
      DataTree[HOM == DefaultHOM, ("DBHCor") := ifelse(is.na(DBHCor) | DBHCor == 0, Diameter, DBHCor)] # At default POM, keep the measured value
      DataTree[HOM > DefaultHOM, ("DBHCor") := TaperFormula(DAB = Diameter,
                                                            HOM = HOM,
                                                            TaperParameter = TaperParameter(DAB = Diameter, HOM = HOM))
      ]


      # Add the column with the correction method  ------------------------------------------------------------------------
      DataTree[HOM > DefaultHOM & !is.na(DBHCor), DiameterCorrectionMeth := "taper"]


    } # end of the correction
  }

  return(DataTree)

}

