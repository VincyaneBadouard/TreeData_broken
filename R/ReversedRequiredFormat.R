#'ReversedRequiredFormat
#'
#'@param Data Standardized data, returned from [RequiredFormat()]
#'
#'@param input A named list, typically the output of function
#'  RequiredFormat_interactive, also called site profile. It has information on
#'  column names correspondence, size units etc...
#'
#'@param x For internal use when function used by Shiny app
#'
#'@param ThisIsShinyApp For internal use when function used by Shiny app
#'  (logical)
#'
#'
#'@details This function takes the standardized forest inventory data.table (returned by [RequiredFormat()])
#'  and converts the column names to the names of the profile given as [input]
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
#' data("ForestGEOProfile")
#' ReversedRequiredFormat(ParacouSubsetFormated, ForestGEOProfile)
#'                }
#'

ReversedRequiredFormat <- function(
  Data,
  input,
  x = NULL,
  ThisIsShinyApp = FALSE
){
  # data(ParacouSubsetFormated)
  # data("ForestGEOProfile")
  # Data <- ParacouSubsetFormated
  # input <- ForestGEOProfile

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


  ## format date of measurement like output profile ####
  if(!input$Date %in% "none"){

    DateFormat <- input$DateFormat
    DateFormat <- gsub("(?<=^)\\w|(?<=[[:punct:]])\\w", "%", DateFormat, perl = T, ignore.case = T) # replace first letter of each word by '%'
    DateFormat <- gsub("yyy", "Y",DateFormat, ignore.case = T)# if remains 3 `y`, change to upper case Y


    Data[, Date := as.Date(Date)]
    Data[, Date := format(Date, format = DateFormat)]

  }





  # Units reverting from standard one ####

  ### DBH in cm ####

  if(!input$DBH %in% "none" | !input$Circ %in% "none") {
    SizeUnit <- grep("[^none]", c(input$DBHUnitMan, input$CircUnitMan), value = T)[1] # take DBH in priority, otherwise CircUnit (not a big deal since we only care about DBH and we already converted it from Circ if that was the only size we had)

    if (substr(SizeUnit, 1, 2) == "mm" | substr(SizeUnit, 1, 2) == "mi")
      Data[, DBH := DBH*10] # cm -> mm

    if (substr(SizeUnit, 1, 1) == "d")
      Data[, DBH := DBH/10] # cm -> dmm

    if (substr(SizeUnit, 1, 1) == "m")
      Data[, DBH := DBH/100] # cm -> m
  }



  ### POM in m ####

  if(!input$POM %in% "none" & !input$POMUnitMan %in% "none") {

    POMUnit <- input$POMUnitMan


    if (substr(POMUnit, 1, 2) == "mm" | substr(POMUnit, 1, 2) == "mi")
      Data[, POM := POM*1000] # m -> mm

    if (substr(POMUnit, 1, 1) == "m")
      Data[, POM := POM*100] # m -> cm


    if (substr(POMUnit, 1, 1) == "d")
      Data[, POM := POM*10] # m -> dm
  }



  ### TreeHeight in m ####

  if(!input$TreeHeight %in% "none" & !input$TreeHeightUnitMan %in% "none") {

    TreeHeightUnit <- input$TreeHeightUnitMan


    if (substr(TreeHeightUnit, 1, 2) == "mm" | substr(TreeHeightUnit, 1, 2) == "mi")
      Data[, TreeHeight := TreeHeight*1000] # m -> mm

    if (substr(TreeHeightUnit, 1, 1) == "m")
      Data[, TreeHeight := TreeHeight*100] # m -> cm


    if (substr(TreeHeightUnit, 1, 1) == "d")
      Data[, TreeHeight := TreeHeight*10] # m -> dm
  }




  ### PlotArea in ha ####

  if(!input$PlotArea %in% "none" & !input$PlotAreaUnitMan %in% "none") {

    PlotAreaUnit <- input$PlotAreaUnitMan



    if (substr(PlotAreaUnit, 1, 2) == "m2")
      Data[, PlotArea := PlotArea*10000] # ha -> m2


    if (substr(PlotAreaUnit, 1, 1) == "km2")
      Data[, PlotArea := PlotArea/100] # ha -> km2
  }

  ### SubPlotArea in ha ####

  if(!input$SubPlotArea %in% "none" & !input$SubPlotAreaUnitMan %in% "none") {

    SubPlotAreaUnitMan <- input$SubPlotAreaUnitMan



      if (substr(SubPlotAreaUnitMan, 1, 2) == "m2")
        Data[, SubPlotArea := SubPlotArea*10000] # ha -> m2


      if (substr(SubPlotAreaUnitMan, 1, 1) == "km2")
        Data[, SubPlotArea := SubPlotArea/100] # ha -> km2

  }

  ## get circumference if needed ####
  if(!input$Circ %in% "none") Data[, Circ := DBH*pi]

  Data[, DBH := round(Circ/pi, 2)]


  # destandardize column names ####

  setDF(Data) # just for this step then we can put back in data.table

  m <- match(colnames(Data), names(input))
  idx_complete <- which(!input[m] %in% "none") # keep standard name when is not asked in the output Profile

  colnames(Data)[idx_complete] <- input[m[idx_complete]]


  setDT(Data)


  # return output ####
  return(Data)

}


