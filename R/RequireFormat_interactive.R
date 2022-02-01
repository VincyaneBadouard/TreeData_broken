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
#' ParacouProfile <- RequiredFormat_interactive(ParacouSubset)
#'                }
#'

RequiredFormat_interactive <- function(Data, input = list()) {

  x <- read.csv("inst/app/data/interactive_items.csv")
  x <- x[x$Activate,]

  # remove what is already in the input (if it is a profile)
  x <- x[!x$ItemID %in% names(input),]

  # separate x into its subsets
  for(i in unique(x$UI)) assign(paste0("x", i), x[x$UI %in% i,])


  column_options <- c("none", colnames(Data))
  unit_options <- c("none", "mm", "millimetre", "millimeter", "milimetro", "milimetrica", "cm", "centimetre", "centimeter", "centimetro", "dm", "decimetre", "decimeter", "decimetro", "m", "metre", "meter", "metro")
  other_options <- ""



  helper_choice <- function(x) eval(parse(text = paste(paste0("input[[\"", x$ItemID, "\"]] <<-", x$argValue, "[as.numeric(readline(cat(\"", x$Label, ifelse(x$helpText != "", paste0(" (",
 x$helpText, ")"), ""), ":\n", paste0(1:length(get(x$argValue)),
 ": ", get(x$argValue), collapse = "  "), "\")))]"))))

  helper_nonchoice <- function(x) eval(parse(text = paste(paste0("input[[\"", x$ItemID, "\"]] <<- readline(cat(\"", x$Label, ifelse(x$helpText != "", paste0(" (", x$helpText, ")"), ""), ":\n", "\"))"))))





readline(cat("Please, help us match your columns to ours.\nWe will successively ask you to give us what columns, in your data set, match the following items (when not provided in 'input'):\n", paste(paste("-", x$Label, ifelse(x$helpText!="", paste0(" (", x$helpText, ")"), "")), collapse = "\n"), "\nFor each if these items, you will have to enter a number. Follow the list provided and enter the number corresponding to the corresponding column, then press [enter].\nIf you don't have a column corresponding, enter '1:none'.\n\n***SCROLL UP TO SEE THE BEGEINING OF THIS MESSAGE***\n\nPress [enter] to start."))

if(exists("x1")) {

  x1_choice <- x1[x1$ItemType %in% "selectInput", ]
  x1_nonchoice <- x1[!x1$ItemType %in% "selectInput", ]

  if (nrow(x1_choice) > 0) helper_choice(x1_choice)
  if (nrow(x1_nonchoice) > 0) helper_nonchoice(x1_nonchoice)
}


if(exists("x2")) {
  x2_ask <- x2[sapply(input[x2$if_X1_is_none], is.null) | input[x2$if_X1_is_none] %in% "none", ]

  x2_ask_choice <- x2_ask[x2_ask$ItemType %in% "selectInput", ]
  x2_ask_nonchoice <- x2_ask[!x2_ask$ItemType %in% "selectInput", ]

  if (nrow(x2_ask_choice) > 0) helper_choice(x2_ask_choice)
  if (nrow(x2_ask_nonchoice) > 0) helper_nonchoice(x2_ask_nonchoice)

}

if(exists("x3")) {

  x3_ask <- x3[sapply(input[x3$if_X1_is_none], is.null) | (input[x3$if_X1_is_none] %in% "none" & !input[x3$if_X2_isnot_none] %in% "none"), ]

  x3_ask_choice <- x3_ask[x3_ask$ItemType %in% "selectInput", ]
  x3_ask_nonchoice <- x3_ask[!x3_ask$ItemType %in% "selectInput", ]

  if (nrow(x3_ask_choice) > 0) helper_choice(x3_ask_choice)
  if (nrow(x3_ask_nonchoice) > 0) helper_nonchoice(x3_ask_nonchoice)

}

return(input)
}

