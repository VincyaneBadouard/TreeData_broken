#'ReversedRequiredFormat
#'
#'@param Data Standardized data, returned from [RequiredFormat()]
#'
#'@param input A named list, typically the output of function
#'  RequiredFormat_interactive, also called site profile with information on
#'  column names correspondence, size units etc... Chosen to be the output profile the user wants their data turned into
#'
#'@param x For internal use when function used by Shiny app
#'
#'@param ThisIsShinyApp For internal use when function used by Shiny app (logical)
#'
#'@param Untidy (logical). If TRUE and input$tidy exists, the data will be untidy (changed from long to wide format, according to input information)
#'
#'
#'@details This function takes the standardized forest inventory data.table (returned by [RequiredFormat()])
#'  and converts the column names to the names of the profile given as input.
#'
#'@return (data.frame) in the format given the profile selected in input.
#'
#'@export
#'
#'@importFrom data.table copy setDT setDF melt tstrsplit :=
#'@importFrom utils read.csv
#'
#' @examples
#'\dontrun{
#'
#' data(ParacouSubsetFormated)
#' data("ForestGeoProfile")
#' ReversedRequiredFormat(ParacouSubsetFormated, ForestGeoProfile)
#'                }
#'

ReversedRequiredFormat <- function(
  Data,
  input,
  x = NULL,
  ThisIsShinyApp = FALSE,
  Untidy = FALSE
){
  # data(ParacouSubsetFormated)
  # data("ForestGeoProfile")
  # Data <- ParacouSubsetFormated
  # input <- ForestGeoProfile

  # Arguments check
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")


  if(!ThisIsShinyApp & !inherits(input, "list")) {
    stop("input must be a list (typically, the output of funcion RequireFormat_interactive.R,
         or a profile saved viw the Shiny App")
  }

  # Load interactive items to see what we are missing ####

  if(!ThisIsShinyApp) {
    x <- try(expr = read.csv(system.file("/app/data/", "interactive_items.csv", package = "TreeData", mustWork = TRUE)), silent = T)

    if (class(x) %in% "try-error"){
      warning("TreeData package not loaded. Assuming you are in the root of the package instead.")
      x <- read.csv("inst/app/data/interactive_items.csv")
    }
    x <- x[x$Activate,]
  }


  ## set as data.table
  setDT(Data)

  ## format date of measurement like output profile ####
  if(!input$Date %in% "none"){

    DateFormat <- input$DateFormat
    DateFormat <- gsub("(?<=^)\\w|(?<=[[:punct:]])\\w", "%", DateFormat, perl = T, ignore.case = T) # replace first letter of each word by '%'
    DateFormat <- gsub("yyy", "Y", DateFormat, ignore.case = T)# if remains 3 `y`, change to upper case Y


    Data[, Date := as.Date(Date)]
    Data[, Date := format(Date, format = DateFormat)]

  }





  # Units reverting from standard one ####

  ### DBH in cm ####

  if(!input$DBH %in% "none" | !input$Circ %in% "none") {
    SizeUnit <- grep("[^none]", c(input$DBHUnitMan, input$CircUnitMan), value = T)[1] # take DBH in priority, otherwise CircUnit (not a big deal since we only care about DBH and we already converted it from Circ if that was the only size we had)

    if (SizeUnit == "mm") Data[, DBH := DBH*10] # cm -> mm

    if (SizeUnit == "dm") Data[, DBH := DBH/10] # cm -> dm

    if (SizeUnit == "m") Data[, DBH := DBH/100] # cm -> m
  }



  ### POM in m ####

  if(!input$POM %in% "none" & !input$POMUnitMan %in% "none") {

    POMUnit <- input$POMUnitMan

    if (POMUnit == "mm") Data[, POM := POM*1000] # m -> mm

    if (POMUnit == "cm") Data[, POM := POM*100] # m -> cm


    if (POMUnit == "dm") Data[, POM := POM*10] # m -> dm
  }



  ### TreeHeight in m ####

  if(!input$TreeHeight %in% "none" & !input$TreeHeightUnitMan %in% "none") {

    TreeHeightUnit <- input$TreeHeightUnitMan

    if (TreeHeightUnit == "mm") Data[, TreeHeight := TreeHeight*1000] # m -> mm

    if (TreeHeightUnit == "cm") Data[, TreeHeight := TreeHeight*100] # m -> cm

    if (TreeHeightUnit == "dm") Data[, TreeHeight := TreeHeight*10] # m -> dm
  }




  ### PlotArea in ha ####

  if(!input$PlotArea %in% "none" & !input$PlotAreaUnitMan %in% "none") {

    PlotAreaUnit <- input$PlotAreaUnitMan

    if (PlotAreaUnit == "m2") Data[, PlotArea := PlotArea*10000] # ha -> m2

    if (PlotAreaUnit == "km2") Data[, PlotArea := PlotArea/100] # ha -> km2
  }

  ### SubPlotArea in ha ####

  if(!input$SubPlotArea %in% "none" & !input$SubPlotAreaUnitMan %in% "none") {

    SubPlotAreaUnitMan <- input$SubPlotAreaUnitMan

    if (SubPlotAreaUnitMan == "m2") Data[, SubPlotArea := SubPlotArea*10000] # ha -> m2

    if (SubPlotAreaUnitMan == "km2") Data[, SubPlotArea := SubPlotArea/100] # ha -> km2

  }




  # untidy if wanted ####
  if(Untidy & input$Tidy > 0) {

    VariableName <- names(input)[input == input$VariableName & names(input) %in% x$ItemID]

    idx = sort(grep("TickedMelt", names(input), value = T))
    idx = which(unlist(input[idx]))


    ValueNames <- sort(grep("^ValueName", names(input), value = T))[idx]
    ValueNames <- names(input)[input %in% input[ValueNames] & names(input) %in% x$ItemID]

    Variablecolumns <- sort(grep("^Variablecolumns", names(input), value = T))[idx]
    Variablecolumns <-  unlist(input[Variablecolumns])


    # remove Circ or DBH if one or the other is involved in tidy (otherwise that duplicates rows)
    if("DBH" %in% ValueNames & "Circ" %in% names(Data)) Data$Circ <- NULL
    if("Circ" %in% ValueNames & "DBH" %in% names(Data)) Data$DBH <- NULL

    Data <- dcast(Data, formula(bquote(...~.(str2lang(VariableName)))), value.var = ValueNames)

    if(!any(grepl("_", Variablecolumns))) {
      old = grep(paste0(paste0('(', ValueNames, '_)', collapse = "|"), "|", paste0('(_', ValueNames, ')', collapse = "|")) , names(Data), value = TRUE)

      setnames(Data, old, gsub("_", "", old))

      # setnames(Data, old, Variablecolumns)
    }

  }



  # ## get (or recalculate) circumference if needed ####
  # if(!input$Circ %in% "none") Data[, Circ := DBH*pi]
  #
  # Data[, DBH := round(Circ/pi, 2)]

  # destandardize column names ####

  setDF(Data) # just for this step then we can put back in data.table

  m <- na.omit(match(colnames(Data), names(input)))
  idx_complete <- which(!input[m] %in% "none") # keep standard name when is not asked in the output Profile

  colnames(Data)[idx_complete] <- input[m[idx_complete]]


  setDT(Data)


  # return output ####
  return(Data)

}


