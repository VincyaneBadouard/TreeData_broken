#' RequiredFormat_interactive
#'
#' @param Data Input forest inventory (data.frame)
#'
#' @param input a named list. Usually empty, except in Shiny App
#'
#' @return a names list with all the argument needed for RequiredFormat_calcul function (column name correspondances + extra input)
#'
#' @export
#'
#' @importFrom data.table copy setDT setDF melt tstrsplit :=
#'
#' @examples
#'\dontrun{
#' data(ParacouSubset)
#' Data <- ParacouSubset
#' RequiredFormat_interactive(Data)
#'                }
#'

RequiredFormat_interactive <- function(Data, input = list()) {

  x <- read.csv("inst/app/data/interactive_items.csv")
  for(i in unique(x$UI)) assign(paste0("x", i), x[x$UI %in% i,])

  column_options <- c("none", colnames(Data))
  unit_options <- c("none", "mm", "millimetre", "millimeter", "milimetro", "milimetrica", "cm", "centimetre", "centimeter", "centimetro", "dm", "decimetre", "decimeter", "decimetro", "m", "metre", "meter", "metro")



readline(cat("Please, help us match your columns to ours.\nWe will successively ask you to give us what columns, in your data set, match the following items:\n", paste(paste("-", x1$ItemID, ifelse(x1$helpText!="", paste0(" (", x1$helpText, ")"), "")), collapse = "\n"), "\nFor each if these items, you will have to enter a number. Follow the list provided and enter the number corresponding to the corresponding column, then press [enter].\nIf you don't have a column corresponding, enter '1:none'.\n\n***SCROLL UP TO SEE THE BEGEINING OF THIS MESSAGE***\n\nPress [enter] to start."))

eval(parse(text = paste(paste0('input[["', x1$ItemID, '"]] <- column_options[as.numeric(readline(cat("', x1$Label, ifelse(x1$helpText!="", paste0(" (", x1$helpText, ")"), ""), ":\n", paste0(1:length(column_options), ": ", column_options, collapse = "  "), '")))]')))) # ask about the columns - level 1

return(input)
}







