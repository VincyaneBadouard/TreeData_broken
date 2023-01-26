#' Individual Diameter Shift Correction
#'
#' @description Correct for 'shift' diameter errors, based on the individual's
#'   non-abnormal growths. Greater weight is given to growths recorded at a
#'   diameter close to the one to correct. The weighted mean of the
#'   non-abnormal growths is computed and applied to correct the abnormal
#'   diameter.
#'
#' @param DataTree  A dataset corresponding to a single tree/stem's (1 IdTree/IdStem)
#'   measurements (data.frame or data.table)
#'   To be filled in if you want the column "DiameterCorrectionMeth" (correction
#'   method) to be filled in the corresponding row.
#'   Otherwise leave DataTree = NULL
#'
#'
#' @param DBHCor Diameter vector (numeric)
#' @param Time Time vector (numeric)
#'
#' @param cresc Annual diameter increment with NA instead of abnormal values
#'   (numeric)
#' @param cresc_abs Absolute diameter increment (not divided by time between 2
#'   values) with NA instead of abnormal values (numeric)
#' @param cresc_abn Abnormal diameter increment positions (numeric)
#'
#' @param coef description... (numeric)
#'
#' @details Method:
#' 1. Correct the 1st value of the shift ("weighted mean"):
#'   - Compute the absolute difference between the DBH to correct and the others
#'   - Give respective weights to the growth (cresc) values according to the
#'      distance to the DBH to correct
#'   - Compute the weighted mean of the growths (cresc)
#'   - Apply the weighted mean to correct the abnormal DBH
#' 2. Correct the following values of the shift ("shift realignment"):
#' Each value = the previous value + initial growth
#'
#' @return List of 2 objects:
#' - DBHCor (numeric vector): corrected by the method
#' - DataTree (data.table): with the *DiameterCorrectionMeth* column filled with
#'    correction method ("weighted mean", "shift realignment")
#'
#' @importFrom stats weighted.mean
#'
#' @export
#'
#' @examples
#' DBHCor = c(13, 13.5, 14.3, 15.8, 16-4, (17.1-4)+2, (18.1-4)+3, 15-4, (15.5-4)+2)
#' Time = c(seq(2000,2008, by = 2), 2012, 2014,2016, 2020)
#' plot(Time, DBHCor)
#' cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
#' cresc_abs <- ComputeIncrementation(Var = DBHCor, Type = "absolute", Time = Time)
#' cresc_abn <- which(cresc >= 5 | cresc_abs < -2) # abnormal values indices
#' cresc[cresc_abn] <- NA
#' cresc_abs[cresc_abn] <- NA
#'
#' Rslt <- IndividualDiameterShiftCorrection(DBHCor = DBHCor, Time = Time,
#'                                   cresc = cresc, cresc_abs = cresc_abs,
#'                                   cresc_abn = cresc_abn)
#'
#' library(ggplot2)
#' ggplot() +
#'   aes(x = Time) +
#'   geom_point(aes(y = Rslt$DBHCor),
#'              col = "forestgreen") +
#'   geom_line(aes(y = Rslt$DBHCor),
#'             col = "forestgreen") +
#'   geom_point(aes(y = DBHCor),
#'              col = "red") +
#'   geom_line(aes(y = DBHCor),
#'             col = "red") +
#'   theme_minimal() +
#'   labs(x = "Year", y = "Diameter (cm)")
#'
IndividualDiameterShiftCorrection <- function(
  # DataTree = NULL, # to comment
  DBHCor,
  Time,
  cresc,
  cresc_abs,
  cresc_abn,
  coef = 0.9
){

  # length(cresc)-1 = length(DBHCor)
  if(length(DBHCor) > (length(cresc)+1) ) cresc[(length(cresc)+1):(length(DBHCor)-1)] <- NA
  if(length(DBHCor) > (length(cresc_abs)+1) ) cresc_abs[(length(cresc_abs)+1):(length(DBHCor)-1)] <- NA

  # rs = 1
  for(rs in 1:length(cresc_abn)){  # as many rs as POM changes

    ## 1. DBH[init shift] -----------------------------------------------------------------------------------------

    # Compute the absolute difference between the DBH to correct and the others
    DBHDiffs <- abs(DBHCor[cresc_abn[rs]+1] - DBHCor) # DBH indices

    # Give respective weights to the growth (cresc) values according to the distance to the DBH to correct
    Weights <- exp(DBHDiffs*-coef) # DBH indices

    Weights <- Weights[-1] # remove 1st value to be in cresc indices


    # Compute the weighted mean of the growths (cresc)

    ## Remove abnormal cresc (and their weight) and NAs
    # cresc <- cresc[-cresc_abn] # remove abnormal cresc(s)
    # Weights <- Weights[-cresc_abn] # remove respective weights

    Weights <- Weights[!is.na(cresc)] # remove weight of missing cresc(s), of which abnormals
    cresc_ok <- cresc[!is.na(cresc)] # remove missing cresc(s), of which abnormals (other name to retain the original value)

    crescMean <- weighted.mean(cresc_ok, Weights)

    # Apply the weighted mean to correct the abnormal DBH (keeping track of old and new value to be able to add the difference to next values)

    oldDBH <- DBHCor[cresc_abn[rs]+1]
    DBHCor[cresc_abn[rs]+1] <- DBHCor[cresc_abn[rs]] + crescMean*diff(Time)[cresc_abn[rs]]
    DiameterCorrectionMeth[cresc_abn[rs]+1] <- GenerateComment( DiameterCorrectionMeth[cresc_abn[rs]+1], "weighted mean")
    newDBH <- DBHCor[cresc_abn[rs]+1]

    # apply switch to other values (until next abnormal value if exist, other wise until the end)
    idx <- ifelse(is.na(cresc_abn[rs+1]), length(DBHCor), cresc_abn[rs+1])
    DBHCor[(cresc_abn[rs]+2):idx] <-  DBHCor[(cresc_abn[rs]+2):idx] + newDBH - oldDBH

    # add method
    DiameterCorrectionMeth[(cresc_abn[rs]+2):idx] <- GenerateComment( DiameterCorrectionMeth[(cresc_abn[rs]+2):idx], "shift realignment")



  } # end rs loop


  return(list(DBHCor = DBHCor,
              DataTree = DataTree))

}
