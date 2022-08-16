#' TaperCorrection
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `Diameter` (numeric)
#'   - **`HOM` (Height Of Measurement) (numeric)**
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
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (Default: FALSE) (logical)
#'
#' @return Fill the *Comment* column with error type informations. If
#'   *DetectOnly* = FALSE, add columns:
#'      - *TaperCorDBH*: corrected trees diameter at default HOM
#'      - *DiameterCorrectionMeth* = "taper"
#'      - *HOMCor* = HOM corresponding to the *TaperCorDBH* (= *DefaultHOM*)
#'
#' @export
#'
#' @examples
#' library(data.table)
#'
#' Data <- data.table(
#'       Diameter = c(13:16, 16-4, (16-4)+2, (16-4)+3, 15-4, (15-4)+2),
#'       POM = c(0, 0, 0, 0, 1, 1, 1, 2, 2),
#'       HOM = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 2, 2))
#'
#' Rslt <- TaperCorrection(Data)
#'
TaperCorrection <- function(
  Data,
  DefaultHOM = 1.3,

  TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  TaperFormula = function(DAB, HOM, TaperParameter, DefaultHOM) DAB / (exp(- TaperParameter*(HOM - DefaultHOM))),

  DetectOnly = FALSE
){

  #### Arguments check ####
  Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # Check if the HOM column exists
  if(!"HOM" %in% names(Data)){
    stop("You have chosen to make a 'taper' correction,
       but you do not have the necessary 'HOM' column in your dataset")
  }

  # DefaultHOM
  if(!inherits(DefaultHOM, "numeric"))
    stop("The 'DefaultHOM' argument must be numeric")

  # TaperParameter/TaperFormula (function)
  if(!all(unlist(lapply(list(TaperParameter, TaperFormula),
                        inherits, "function"))))
    stop("The 'TaperParameter' and 'TaperFormula' arguments must be functions")

  # DetectOnly (logical)
  if(!inherits(DetectOnly, "logical"))
    stop("The 'DetectOnly' argument must be a logical")

  # In data.table
  setDT(Data)

  if(any(Data[,HOM] > DefaultHOM)){ # if some measurements of the tree were made above the POM by default

    Data <- GenerateComment(Data,
                            condition = (Data[,HOM] > DefaultHOM),
                            comment = paste0("HOM different from the default HOM"))

    if(DetectOnly %in% FALSE){
      if(!"TaperCorDBH" %in% names(Data))
        Data[, TaperCorDBH := numeric(.N) ] # start without value (I can't put NA because it's a logical, so it's a 0)

      # Apply taper correction  -------------------------------------------------------------------------------------------
      Data[HOM == DefaultHOM, ("TaperCorDBH") := ifelse(is.na(TaperCorDBH) | TaperCorDBH == 0, Diameter, TaperCorDBH)] # At default POM, keep the measured value
      Data[HOM > DefaultHOM, ("TaperCorDBH") := TaperFormula(DAB = Diameter,
                                                        HOM = HOM,
                                                        TaperParameter = TaperParameter(DAB = Diameter, HOM = HOM),
                                                        DefaultHOM = DefaultHOM)
      ]


      # Add the column with the correction method  ------------------------------------------------------------------------

      Data <- GenerateComment(Data,
                              condition = ( Data[,HOM] > DefaultHOM & !is.na(Data[, TaperCorDBH]) ),
                              comment = "taper",
                              column = "DiameterCorrectionMeth")

      Data[, HOMCor := DefaultHOM]


    } # end of the correction
  }

  return(Data)

}

