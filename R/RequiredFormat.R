#'RequiredFormat
#'
#'@param Data Forest inventory data set (data.frame or data.table) - already stacked, merged and tidyed
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
#'@details This function takes the forest inventory data.frame or data.table as
#'  it is, and converts the column names to the standardized names used in this
#'  package. It also generates missing information, when possible (e.g. DBH when
#'  only circumference is givent, Genus and Species when only scientifique name
#'  is given etc...). All the decisions are made based on what is provided in
#'  the input argument, which is a named list, as returned by function
#'  RequiredFormat_interactive or Profile.rds file downloaded from shiny app
#'
#'@return Input inventory (data.frame) in the required package format.
#'
#'@export
#'
#'@importFrom data.table copy setDT setDF melt tstrsplit :=
#'@importFrom utils read.csv
#'
#' @examples
#'\dontrun{
#' data(ParacouSubset)
#' data(ParacouProfile)
#' ParacouSubsetFormated <- RequiredFormat(
#'   ParacouSubset,
#'   input = ParacouProfile)
#'                }
#'

RequiredFormat <- function(
  Data,
  input,
  x = NULL,
  ThisIsShinyApp = FALSE
){
  # data(ParacouSubset)
  # data(ParacouProfile)
  # Data <- ParacouSubset
  # input <- ParacouProfile

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

  CharacVar <- x$ItemID[x$DataType %in% "character"]
  NumVar <- x$ItemID[x$DataType %in% "numeric"]
  LogicVarm <- x$ItemID[x$DataType %in% "logical"]

  # standardize column names ####

  setDF(Data) # just for this step then we can put back in data.table

  idx <- match(colnames(Data), input[x$ItemID])

  colnames(Data) <- names(input[x$ItemID])[idx]

  ## delete columns we don't want
  Data[which(is.na(idx))] <- NULL

  ## add columns missing
  Data[, setdiff(x$ItemID[x$RequiredColumn], colnames(Data))] <- NA

  setDT(Data)


  # coerce to data types ####
  ### as.character

  CharacVar <- CharacVar[CharacVar %in% colnames(Data)]

  Data[, (CharacVar) := lapply(.SD, as.character), .SDcols = CharacVar] # (CharacVar) to say that these are existing columns and not new ones to create

  ### as.numeric

  NumVar <- NumVar[NumVar  %in% colnames(Data)]

  Data[, (NumVar) := lapply(.SD, as.character), .SDcols = NumVar] # first as character when the variable is in factor, to preserve information
  Data[, (NumVar) := lapply(.SD, as.numeric), .SDcols = NumVar] # () to say that these are existing columns and not new ones to create

  ### as.logical
  ## Here we have to use user input to know what is TRUE and what is not

  ### Life status
  if( !input$LifeStatus %in% "none") {
    Data[, LifeStatusOriginal := LifeStatus]
    Data[, LifeStatus := ifelse(LifeStatus %in% input$IsLive, TRUE, FALSE)]
  }

  ### commercial species
  if( !input$CommercialSp %in% "none") {
    Data[, CommercialSpOriginal := CommercialSp]
    Data[, CommercialSp := ifelse(CommercialSp %in% input$IsCommercial, TRUE, FALSE)]
  }

  # LogicVar <- LogicVar[LogicVar %in% colnames(Data)]
  # Data[, (LogicVar) := lapply(.SD, as.logical), .SDcols = LogicVar] # () to say that these are existing columns and not new ones to create


  ## Date of measurement ####
  if(!input$Date %in% "none"){

    # save the orginal dates
    Data[, DateOriginal := Date]

    # transform to standard format
    DateFormat <- trimws(input$DateFormat)


    if(grepl("num|dec", DateFormat, ignore.case = T)) {

      if(grepl("num", DateFormat, ignore.case = T)) Data[, Date := as.Date(as.numeric(trimws(Date)), origin = "1970-01-01")]

      if(grepl("dec", DateFormat, ignore.case = T)) Data[, Date := as.Date(lubridate::date_decimal(as.numeric(trimws(Date))))]

    } else {

      DateFormat <- gsub("(?<=^)\\w|(?<=[[:punct:]])\\w", "%", DateFormat, perl = T, ignore.case = T) # replace first letter of each word by '%'
      DateFormat <- gsub("yyy", "Y",DateFormat, ignore.case = T)# if remains 3 `y`, change to upper case Y

      Data[, Date := as.Date(trimws(as.character(Date)), format = DateFormat)]

    }

# send waring if some dates translated as NA
    if(any(!is.na(Data$DateOriginal) & is.na(Data$Date))) warning("Some dates were translated as NA... Either your data format does not corresponf to the format of your date column, or you do not have a consistent format across all your dates")

  }




  # make input complete ####

  ## enter all itemID in input as "none" so we can refer to them - make sure this happens after standardizing column names otherwise that won't work...
  input[setdiff(x$ItemID, names(input))] <- "none"



  # Fill in info in column missing ####

  ## Year
  if(input$Year %in% "none") {
    if(!input$Date %in% "none") Data[, Year := format(Date, "%Y")] else Data[, Year := input$YearMan]

    Data$Year <- as.numeric(as.character(Data$Year))

  }



  ## IdTree (unique along Plot, SubPlot, TreeFieldNum) ####
  if (input$Site %in% "none") Data[, Site :=  input$SiteMan]
  if (input$Plot %in% "none") Data[, Plot :=  input$PlotMan]
  if (input$SubPlot %in% "none") Data[, SubPlot := input$SubPlotMan]

  if (input$IdTree %in% "none") {

    # if we also don't have TreeFieldNum, we are just considering that each row within a plot and subplot is one tree
    if (input$TreeFieldNum %in% "none") {
      warning("You do not have a column with unique tree IDs and we are considering that each row within a Plot and SubPlot refers to one unique tree")
      Data[, IdTree := seq(1, .N) , by = .(Plot, SubPlot)]
    }

    # if we have TreeFieldNum, we use it

    if (!input$TreeFieldNum %in% "none")
      Data[, IdTree := paste(Plot, SubPlot, TreeFieldNum, sep = "_")]

  }


  ## Genus, Species, ScientificNameSep ####

  ### Genus and species if we have ScientificName and ScientificNameSep
  if(input$Genus %in% "none" & input$Species %in% "none" & !input$ScientificName %in% "none" & !input$ScientificNameSep %in% "none")
    Data[, c("Genus", "Species") := tstrsplit(ScientificName, input$ScientificNameSep , fixed = TRUE, keep  = c(1,2))]

  ### ScientificName if we have Genus and species

  if(!input$Genus %in% "none" & !input$Species %in% "none" & input$ScientificName %in% "none" )
    Data[, ScientificName := paste(Genus, Species)]


  ## DBH if we have circumference ####
  if(input$DBH %in% "none" & input$Circ %in% "none") stop("You do not have tree size (DBH or Circonference) in your data (or you have not specified what column that information is store in. We cannot move forward.")

  if(input$DBH %in% "none" & !input$Circ %in% "none") Data[, DBH := round(Circ/pi, 2)]

  # Units changing ####

  unitOptions <- c("mm", "cm", "dm", "m") # c("mm", "millimetre", "millimeter", "milimetro", "milimetrica", "cm", "centimetre", "centimeter", "centimetro", "dm", "decimetre", "decimeter", "decimetro", "m", "metre", "meter", "metro")

  AreaUnitOptions <- c("m2", "ha", "km2")

  ### DBH in cm ####
  # if((!input$DBH %in% "none" & !input$DBHUnit %in% "none") | (!input$Circ %in% "none" & !input$CircUnit %in% "none")) stop("We have not coded the case where size units are not constant across your data yet - Please contact us or unify your units first.")

  if(!input$DBH %in% "none" | !input$Circ %in% "none") {

    SizeUnit <- grep("[^none]", c(input$DBHUnitMan, input$CircUnitMan), value = T)[1] # take DBH in priority, otherwise CircUnit (not a big deal since we only care about DBH and we already converted it from Circ if that was the only size we had)

    if(!SizeUnit %in% unitOptions) stop(paste("Your size units are not one of:", paste(unitOptions, collapse = ", ")))

    if(SizeUnit %in% unitOptions) {
    if (SizeUnit == "mm") Data[, DBH := DBH/10] # mm -> cm

    if (SizeUnit == "dm") Data[, DBH := DBH*10] # dm -> cm

    if (SizeUnit == "m") Data[, DBH := DBH*100] # m -> cm
    }
  }

  ### POM in m ####
  # if(!input$POM %in% "none" & !input$POMUnit %in% "none") stop("We have not coded the case where POM units are not constant across your data yet - Please contact us or unify your units first.")

  if(!input$POM %in% "none" & !input$POMUnitMan %in% "none") {

    POMUnit <- input$POMUnitMan

    if(!POMUnit %in% unitOptions) stop(paste("Your POM units are not one of:", paste(unitOptions, collapse = ", ")))

    if (POMUnit %in% unitOptions) {

      if (POMUnit == "mm") Data[, POM := POM/1000] # mm -> m

      if (POMUnit == "cm") Data[, POM := POM/100] # cm -> m


      if (POMUnit == "dm") Data[, POM := POM/10] # dm -> m
    }
  }



  ### TreeHeight in m ####
  # if(!input$TreeHeight %in% "none" & !input$TreeHeightUnit %in% "none") stop("We have not coded the case where height units are not constant across your data yet - Please contact us or unify your units first.")


  if(!input$TreeHeight %in% "none" & !input$TreeHeightUnitMan %in% "none") {

    TreeHeightUnit <- input$TreeHeightUnitMan

    if(! TreeHeightUnit %in% unitOptions) stop(paste("Your height units are not one of:", paste(unitOptions, collapse = ", ")))

    if (TreeHeightUnit %in% unitOptions) {

      if (TreeHeightUnit == "mm") Data[, TreeHeight := TreeHeight/1000] # mm -> m

      if (TreeHeightUnit == "cm") Data[, TreeHeight := TreeHeight/100] # cm -> m

      if (TreeHeightUnit == "dm") Data[, TreeHeight := TreeHeight/10] # dm -> m
    }
  }




  ### PlotArea in ha ####

  if(!input$PlotArea %in% "none" & !input$PlotAreaUnitMan %in% "none") {

    PlotAreaUnit <- input$PlotAreaUnitMan

    if(!PlotAreaUnit %in% AreaUnitOptions) stop(paste("Your height units are not one of:", paste(AreaUnitOptions, collapse = ", ")))

    if (PlotAreaUnit %in% AreaUnitOptions) {

      if (PlotAreaUnit == "m2") Data[, PlotArea := PlotArea/10000] # m2 -> ha

      if (PlotAreaUnit == "km2") Data[, PlotArea := PlotArea*100] # km2 -> ha
    }
  }

  # if area is entered manually, it is supposed to be in ha already
  if(input$PlotArea %in% "none" & !input$PlotAreaMan %in% "none") {
    Data[, PlotArea := input$PlotAreaMan]
  }

  ### SubPlotArea in ha ####

  if(!input$SubPlotArea %in% "none" & !input$SubPlotAreaUnitMan %in% "none") {

    SubPlotAreaUnitMan <- input$SubPlotAreaUnitMan

    if(!SubPlotAreaUnitMan %in% AreaUnitOptions) stop(paste("Your height units are not one of:", paste(AreaUnitOptions, collapse = ", ")))

    if (SubPlotAreaUnitMan %in% AreaUnitOptions){

      if (SubPlotAreaUnitMan == "m2") Data[, SubPlotArea := SubPlotArea/10000] # m2 -> ha


    if (SubPlotAreaUnitMan == "km2") Data[, SubPlotArea := SubPlotArea*100] # km2 -> ha
    }
  }

  # if area is entered manually, it is supposed to be in ha already
  if(input$SubPlotArea %in% "none" & !input$SubPlotAreaMan %in% "none") {
    Data[, SubPlotArea := input$SubPlotAreaMan]
  }


  # return output ####
  ColumnsToReturn <- intersect(c(x$ItemID, paste0(x$ItemID, "Original")), colnames(Data))
  return(Data[, ..ColumnsToReturn])

}


