#' TaperCorrection
#'
#' @description Transform the tree diameter measured at a given height into the
#'   diameter corresponding to the default measurement height (HOM), using an
#'   allometry.
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
#'
#' @return Fill the *Comment_TreeData* column with error type informations and add columns:
#'      - *Diameter_TreeDataCor*: corrected trees diameter at default HOM
#'      - *DiameterCorrectionMeth* = "taper"
#'      - *HOM_TreeDataCor* (numeric): HOM corresponding to the
#'           *Diameter_TreeDataCor* (= *DefaultHOM*)
#'
#' @export
#'
#' @examples
#' library(data.table)
#'
#' Data <- data.table(IdStem = "A",
#'       ScientificName = "Tree",
#'       Year = c(1998, 2008, 2016, 2017, 2018, 2019, 2021),
#'       IdCensus = factor(c(1998, 2008, 2016, 2017, 2018, 2019, 2021), ordered = TRUE),
#'       Diameter = c(19, 19, 21.4, 22.6, 23.1, 23.1, 23.6),
#'       HOM = c(1.30, 3.25, 3.25, 3.25, 3.25, 3.25, 3.25))
#'
#' Rslt <- TaperCorrection(Data)
#' DiameterCorrectionPlot(Rslt, CorCol = "Diameter_TreeDataCor")
#'
TaperCorrection <- function(
    Data,
    DefaultHOM = 1.3,

    TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
    TaperFormula = function(DAB, HOM, TaperParameter, DefaultHOM) DAB / (exp(- TaperParameter*(HOM - DefaultHOM)))
){

  #### Arguments check ####
  # Data
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


  # In data.table
  setDT(Data)
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environment


  # if(any(Data[!is.na(HOM),HOM] != DefaultHOM)){ # if some measurements of the tree were made above the POM by default

  if(!"Comment_TreeData" %in% names(Data)) Data[, Comment_TreeData := "" ]

  Data[!HOM %in% DefaultHOM, Comment_TreeData := GenerateComment(Comment_TreeData, "HOM different from the default HOM")]



    if(!"Diameter_TreeDataCor" %in% names(Data)) Data[, Diameter_TreeDataCor := Diameter ]
    if(!"DiameterCorrectionMeth_TreeData" %in% names(Data))  Data[,DiameterCorrectionMeth_TreeData := ""]

    # Apply taper correction  -------------------------------------------------------------------------------------------
    Data[HOM != DefaultHOM, ("Diameter_TreeDataCor") := TaperFormula(DAB = Diameter,
                                                                     HOM = HOM,
                                                                     TaperParameter = TaperParameter(DAB = Diameter, HOM = HOM),
                                                                     DefaultHOM = DefaultHOM)
    ]


    # If no HOM value, we keep the original Diameter value
    Data[!is.na(Diameter) & is.na(HOM), Diameter_TreeDataCor := Diameter] # probably not necessary step

    # If corrected value is 0 (DBH = NA) put NA
    Data[Diameter_TreeDataCor == 0 & is.na(Diameter), Diameter_TreeDataCor := NA_real_]

    # Add the column with the correction method  ------------------------------------------------------------------------

    Data[Diameter != Diameter_TreeDataCor &
           !is.na(Data[, Diameter_TreeDataCor]), DiameterCorrectionMeth_TreeData := GenerateComment(DiameterCorrectionMeth_TreeData, "taper")]

    # Data <- GenerateComment(Data,
    #                         condition = ( Data[,HOM] != DefaultHOM &
    #                                         !is.na(Data[, Diameter_TreeDataCor]) &
    #                                         !is.na(Data[,HOM]) ),
    #                         comment = "taper",
    #                         column = "DiameterCorrectionMeth")

    Data[, HOM_TreeDataCor := DefaultHOM]



  return(Data[])

}

