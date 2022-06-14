#' RegressionInterpolation
#'
#' @param Y Response variable with missing value (NA) (numeric)
#' @param X Explanatory variable (numeric)
#' @param CorrectionType "quadratic" and/or "linear" (character)
#'
#' @details The variables X and Y must be of the same length.
#'
#' @return Y (numeric) with interpolated missing values depending on the form of
#'   model chosen (*CorrectionType*)
#'
#'@importFrom stats lm poly
#' @export
#'
#' @examples
#'
#' DBHCor = c(13, 14, 15, 16, 30, 19, 15, 21, 23)
#' Time = c(2000, 2002, 2004, 2006, 2008, 2012, 2014, 2016, 2020)
#' plot(Time, DBHCor)
#'
#' # Compute all cresc
#' cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
#' cresc_abs <- ComputeIncrementation(Var = DBHCor, Type = "absolute", Time = Time)
#'
#' # Replace abnormal cresc by NA in the DBH
#' PositiveGrowthThreshold = 5
#' NegativeGrowthThreshold = -2
#' ab_cresc <- which(cresc >= PositiveGrowthThreshold | cresc_abs < NegativeGrowthThreshold)
#' DBHCor[ab_cresc +1] <- NA # abnormal DBH <- NA
#'
#' # Re-compute without the abnormal growth
#' cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
#'
#' Y = cresc
#' X = Time[-1]
#' plot(X,Y)
#'
#' # Compute the corrected cresc
#' cresc_Corr <- RegressionInterpolation(Y = Y, X = X,
#'                                       CorrectionType = "quadratic")
#'
#' # Correct with the corrected cresc, the corrected DBH
#' for(i in which(is.na(DBHCor))){
#' DBHCor[i] <- DBHCor[i-1] + cresc_Corr[i-1] * diff(Time)[i-1]
#' }
#' plot(Time, DBHCor)
#'
RegressionInterpolation <- function(
  Y,
  X,
  CorrectionType = "quadratic"
){

  #### Arguments check ####

  # X/Y (numeric)
  if(!all(unlist(lapply(list(X, Y),
                        inherits, c("numeric", "integer"))))){
    stop("The 'X' and 'Y' variables of the 'RegressionInterpolation' function must be numeric")
  }
  # X & Y of the same lenght
  if(length(X) != length(Y))
    stop("The variables X and Y must be of the same length ('RegressionInterpolation()' function)")

  # CorrectionType (character)
  if(!any(any(CorrectionType %in% "quadratic") || any(CorrectionType %in% "linear")))
    stop("The 'CorrectionType' argument value must be 'quadratic' and/or 'linear'")

  #### Function ####

  miss <- which(is.na(Y)) # DBH = NA -> value to replace

  # miss: nb of the missing value(s) (which values) (vector)

  Y[miss] <- sapply(miss, function(i) { # i = each value de miss

    if("quadratic" %in% CorrectionType & length(which(!is.na(Y))) > 2){

      # Degree 2 polynomial regression (= quadratic)
      reg <- lm(Y ~ poly(X, degree = 2, raw = TRUE))$coef # 'degree' must be less than number of unique points
      yi <- reg[1] + reg[2] * X[i] + reg[3] * X[i]^2 # (y = b + ax + cx^2),  DBHi = b + a YEARi + c YEARi^2

    }else{ # "linear"

      # Linear regression: DBH ~ years
      reg <- lm(Y ~ X)$coef # extract the coefs
      yi <- reg[1] + reg[2] * X[i] # (y = b + ax),  DBHi = b + a YEARi
    }

    return(yi) # DBH of i, yi -> Y[miss]

  }) # sapply end (for each i)

  return(as.numeric(unlist(Y))) # corrected DBHs

}
