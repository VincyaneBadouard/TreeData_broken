# RegressionInterpolation

# Y = cresc
# X = X

# args : Y, X, CorrectionType = "linear"
#
# ex  ---------------------------------------------------------------------------------------------------------------------
# DBHCor = c(13, 14, 15, 16, 30, 19, 15, 21, 23)
# Time = c(2000, 2002, 2004, 2006, 2008, 2012, 2014, 2016, 2020)
# plot(Time,DBHCor)
#
# # Compute all cresc
# cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
# cresc_abs <- ComputeIncrementation(Var = DBHCor, Type = "absolute", Time = Time)
# # replace abnormal cresc by NA in the DBH
# PositiveGrowthThreshold = 5
# NegativeGrowthThreshold = -2
# ab_cresc <- which(cresc >= PositiveGrowthThreshold | cresc_abs < NegativeGrowthThreshold)# là il considere le retour à la normale comme une erreur
# DBHCor[ab_cresc +1] <- NA # abnormal DBH <- NA
# # re-compute without the abnormal growth
# cresc <- ComputeIncrementation(Var = DBHCor, Type = "annual", Time = Time)
#
# Y = cresc
# X = Time[-1]
# plot(X,Y)
#
# cress_Corr <- RegressionInterpolation(Y = cresc, X = Time[-1], CorrectionType = "quadratic") # Compute the corrected cresc
#
# for(i in which(is.na(DBHCor))){
# DBHCor[i] <- DBHCor[i-1] + cress_Corr[i-1]*diff(Time)[i-1] # Correct with the corrected cresc, the corrected DBH
# }
# function ----------------------------------------------------------------------------------------------------------------

#' RegressionInterpolation
#'
#' @param Y dfghj
#' @param X sdf
#' @param CorrectionType sdfg
#'
#' @return qsdf
#' @export
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
    stop("The variables X and Y must be of the same length ('RegressionInterpolation()' function) ")

  # CorrectionType (character)
  if(!any(CorrectionType %in% "quadratic"|| CorrectionType %in% "linear"))
    stop("The 'CorrectionType' argument value must be 'quadratic' and/or 'linear'")

  #### Function ####

  miss <- which(is.na(Y)) # DBH = NA -> value to replace

  # miss: nb of the missing value(s) (which values) (vector)

  Y[miss] <- sapply(miss, function(i) { # i = each value de miss

    if("quadratic" %in% CorrectionType & length(which(!is.na(Y))) > 3){

      # Degree 2 polynomial regression (= quadratic)
      reg <- lm(Y ~ poly(X, 2))$coef # 'degree' must be less than number of unique points
      yi <- reg[1] + reg[2] * X[i] + reg[3] * X[i]^2 # (y = b + ax + cx^2),  DBHi = b + a YEARi + c YEARi^2


    }else{ # "linear"

      # Linear regression: DBH ~ years
      reg <- lm(Y ~ X)$coef # extract the coefs
      yi <- reg[1] + reg[2] * X[i] # (y = b + ax),  DBHi = b + a YEARi
    }

    return(yi) # DBH of i, yi -> Y[miss]

  }) # sapply end (for each i)

  return(unlist(Y)) # corrected DBHs

}

# plot(X,Y)
