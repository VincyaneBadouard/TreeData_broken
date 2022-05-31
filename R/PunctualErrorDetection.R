#' PunctualErrorDetection
#'
#' @param DBHCor The diameter vector (numeric)
#' @param Time Time variable (numeric)
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
#' @param TrustMeasSet Trust measurements set: the "first" or the "last" set
#'   (character, 1 value)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (Default: FALSE) (logical)
#'
#' @return The diameter vector (numeric) with NA instead of the punctual errors
#'   detected. Return the intact input diameter vector in case of shift errors.
#'
#' @export
#'
#' @examples
#' Time <- c(2000, 2002, 2004, 2006, 2008, 2012, 2014, 2016, 2020)
#'
#' # 1 ind at a time
#' ## Punctual error case
#' DBHCor <- c(13, 14, 15, 16, 30, 19, 15, 21, 23)
#' plot(Time, DBHCor)
#'
#' PunctualErrorDetection(DBHCor = DBHCor, Time = Time)
#'
#' ## 2 shifts error case
#' DBHCor <- c(13, 14, 15, 16, 12, 14, 15, 11, 13)
#' plot(Time, DBHCor)
#'
#' PunctualErrorDetection(DBHCor = DBHCor, Time = Time)
#'
PunctualErrorDetection <- function(
  DBHCor,
  Time,

  PositiveGrowthThreshold = 5,
  NegativeGrowthThreshold = -2,

  TrustMeasSet = "first",

  DetectOnly = FALSE
){

  # Compute diameter incrementation -----------------------------------------------------------------------------------------
  cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
  cresc_abs <- ComputeIncrementation(Var = DBHCor, Type = "absolute", Time = Time)

  # remove the last value if it's a NA (no DBH value so no cresc or cresc_abs)
  if(is.na(cresc[length(cresc)]) & is.na(cresc_abs[length(cresc)])){
    cresc <- cresc[-length(cresc)]
    cresc_abs <- cresc_abs[-length(cresc_abs)]
  }

  # Detect abnormal growth --------------------------------------------------------------------------------------------------
  Ncresc_abn <- sum(cresc >= PositiveGrowthThreshold | cresc_abs < NegativeGrowthThreshold) # nbr of abnormal values
  # le retour à la normale est considéré comme une erreur (perte excessive)

  if(Ncresc_abn > 0) { # if there are abnormal values
    for (i in 1:Ncresc_abn) { # for each abnormal value

      # Begin with the census with the highest DBH change
      cresc_abn <- which(cresc >= PositiveGrowthThreshold | cresc_abs < NegativeGrowthThreshold) # quels sont les concernés
      ab <- cresc_abn[which.max(abs(cresc[cresc_abn]))] # the highest absolute DBH increment (celui qui a initié) (ab in cresc indices)

      # Check if there is a return to normal --------------------------------------------------------------------------------
      if(length(ab) == 1) {
        # With 4 values surrounding ab
        surround <- c(ab - 2, ab - 1, ab + 1, ab + 2) # the 4 values, 2 before & 2 after the error
        # In the DBH seq
        surround <- surround[surround > 0 &
                               surround <= length(cresc)] # de taille maximale [0;longueur de l'incrémentation = length(dbh) -1)]

        # Moment of max and min DBH changes around ab (including ab, that should be one of the 2)
        sourround_ab <- sort(c(surround, ab)) # ab and its 4 values around in the increasing order
        up <- sourround_ab[which.max(cresc[sourround_ab])] # Max growth of this seq
        down <- sourround_ab[which.min(cresc[sourround_ab])] # Min growth of this seq


        if(length(surround) > 0) {  # if there are values around ab
          # 1st case = Punctual: excessive increase/decrease offset by a similar decrease in dbh, + a normal growth
          # is there a value that could compensate the excessive DBH change?
          # check if removing those values would solve the problem (ie cresc < 5 & cresc_abs > -2 )
          if( # if it's punctual --------------------------------------------------------------------------------------------
              # the max positive growth is before the min negative growth (increase then decrease) and ab is before the other value
              isTRUE(up < down & down != ab & cresc[up] * cresc[down] < 0 &
                     # Compute cresc around the error by skipping the error to check if it's normal
                     # (cresc and down are cresc indices, to have the corresponding DBH index add +1)
                     ((DBHCor[down + 1] - DBHCor[up]) / (Time[down + 1] - Time[up])) <= PositiveGrowthThreshold &
                     (DBHCor[down + 1] - DBHCor[up]) >= NegativeGrowthThreshold) | # Nino a mis un else plutot qu'un "ou" (ce qui semble mieux)

              # the max positive growth is after the min negative growth (decrease then increase) and ab is before the other value
              isTRUE(down < up & up != ab & cresc[up] * cresc[down] < 0 &
                     # Compute cresc around the error by skipping the error to check if it's normal
                     ((DBHCor[up + 1] - DBHCor[down]) / (Time[up + 1] - Time[down])) <= PositiveGrowthThreshold &
                     (DBHCor[up + 1] - DBHCor[down]) >= -NegativeGrowthThreshold)) { # different de Nino (tag dirtyhack) (runner ac mon ex)

            # Abnormal DBH <- NA and will be replaced later on (by RegressionInterpolation()) -------------------------------
            first <- min(up, down) + 1 # The punctual error: the 1st value with the greatest increment (positive or negative) (+1 to switch from cresc to DBH indices)
            last <- max(up, down) # The compensation: the last value with the greatest increment (positive or negative) (in cresc indices, -1 to get the DBH index)

            # first and last is the same value if the error is compensated immediately
            DBHCor[first:last] <- NA # put NA from the 1st to the last greatest increment
          } # if it's a punctual error, else it's a shift

        } # if there are values around ab

        ## If only 2 values, with abnormal difference
        if(length(DBHCor[!is.na(DBHCor)]) == 2 & i==1){ # i =  chaque valeur aberrante

          if(TrustMeasSet %in% "first"){
            # trust the 1st one
            DBHCor[!is.na(DBHCor)][2] <- DBHCor[!is.na(DBHCor)][1]
          }else if(TrustMeasSet %in% "last"){
            # trust the 2nd one
            DBHCor[!is.na(DBHCor)][1] <- DBHCor[!is.na(DBHCor)][2]
          }
        }

        # Update diameter incrementation (for the i loop)--------------------------------------------------------------------
        cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
        cresc_abs <- ComputeIncrementation(Var = DBHCor, Type = "absolute", Time = Time)

      } # length(ab) == 1
    } # i loop end
  } # if there are abnormal values

  return(DBHCor)

}

