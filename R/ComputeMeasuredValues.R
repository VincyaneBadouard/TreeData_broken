#' Compute Measured Values
#'
#' @param initial_values Vector of the initial values of the variable (numeric)
#' @param cresc_abs Vector of the absolute incrementations of the variable (numeric)
#'
#' @return A numeric vector of the same size as *initial_values* with, the first
#'   initial value incremented by the first value of cresc_abs, then this value
#'   incremented by the 2nd value of cresc_abs, then this value incremented by
#'   the 3rd value of cresc_abs, etc.
#'
#' @export
#'
#' @examples
#' initial_values <- c(3.7, 2.8, 2.8, 2.8, NA)
#' cresc_abs <- c(-0.9, 0.0,  0.0)
#' ComputeMeasuredValues(initial_values = initial_values,
#'                       cresc_abs = cresc_abs)
#'
ComputeMeasuredValues <- function(
  initial_values,
  cresc_abs
){

  # length(cresc_abs) = length(initial_values)-1
  if((length(initial_values)-1) > length(cresc_abs))
    cresc_abs[(length(cresc_abs)+1):(length(initial_values)-1)] <- NA


  # Val[1] = initial_values[1]
  Val <- c(initial_values[1])

  # Loop to compute values from the 1st value and variable incrementation
  for(n in 1:length(cresc_abs)){
    Val <- c(Val, Val[n] + cresc_abs[n])
  }

  return(Val)

}
