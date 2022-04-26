#' ComputeIncrementation
#'
#' @param Var Variable (numeric)
#'
#' @param Type Type of incrementation: "absolute" or "annual" (character)
#'    - "absolute": absolute increment (not divided by time between 2 values)
#'    - "annual": annual increment
#'
#' @param Time Time variable (numeric)
#'
#' @return Absolute or annual incrementation of the variable
#'
#' @export
#'
#' @examples
#'
#' DBHCor = c(13, 14, 15, 16, 30, 19, 15, 21, 23)
#' Time = c(2000, 2002, 2004, 2006, 2008, 2012, 2014, 2016, 2020)
#' Rslt <- ComputeIncrementation(Var = DBHCor, Time = Time)
#'
ComputeIncrementation <- function(
  Var,
  Type,
  Time
){

  #### Arguments check ####

  # Var (numeric)
  if(!inherits(Var, "numeric"))
    stop("'Var' argument must be numeric")

  # If not enough values
  if(sum(!is.na(Var)) <= 1)
    stop("You do not have enough values to calculate an increment (</= 1 value in your data)")

  # Type (character)
  if(length(Type) != 1 | !inherits(Type, "character"))
    stop("'Type' argument must be in character")

  # Time (numeric)
  if(Type %in% "annual"){
    if(!inherits(Time, "numeric"))
      stop("'Time' argument must be numeric")
  }

  # Initialisation ----------------------------------------------------------------------------------------------------------
  incr <- rep(NA, length(Var) - 1) # (cresc[1] corresponds to the 2nd DBH)

  if(Type %in% "absolute"){
    # Absolute diameter increment (not divided by time between census) (cresc_abs) ------------------------------------------
    incr[which(!is.na(Var))[-1] - 1] <- diff(Var[!is.na(Var)])
  }

  if(Type %in% "annual"){
    # Annual diameter increment (cresc) -------------------------------------------------------------------------------------
    incr[which(!is.na(Var))[-1] - 1] <- # 8 cresc for 9 dbh values ([-1]), shift all indices by 1 to the left (-1)
      diff(Var[!is.na(Var)]) / diff(Time[!is.na(Var)]) # DBH difference between pairwise censuses / time difference between pairwise censuses
  }

  return(incr)

}
