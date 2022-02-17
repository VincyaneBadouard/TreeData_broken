#'RequiredFormat
#'
#'@param Data Forest inventory data set (data.frame or data.table)
#'
#'@param input A named list, typically the output of function
#'  RequiredFormat_interactive, also called site profile. It has information on
#'  column names correspondence, size units etc...
#'
#'@param x For internal use when function used by Shiny app
#'
#'@param ThisIsShinyApp For internal use when function used by Shiny app (logical)
#'
#'
#'@details This function takes the forest inventory data.frame or data.table as
#'  it is, and converts the column names to the standardized names used in this
#'  package. It also generates missing information, when possible (e.g. DBH when
#'  only circumference is givent, Genus and Species when only scientifique name
#'  is given etc...). All the decisions are made based on what is provided in
#'  the input argument. It is a named list, output of function
#'  RequiredFormat_interactive.
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
# data(ParacouSubset)
# data(ParacouProfile)
# ParacouSubsetFormated <- RequiredFormat(
#   ParacouSubset,
#   input = ParacouProfile)
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
    stop("input must be a list (typically, the output of funcion RequireFormat_interactive.R, or a profile saved viw the Shiny App")
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

  idx <- match(colnames(Data), input)

  colnames(Data) <- names(input)[match(colnames(Data), input)]

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
    Data[, LifeStatus := ifelse(LifeStatus %in% input$IsLive, TRUE, FALSE)]
  }

  ### commercial species
  if( !input$CommercialSp %in% "none") {
    Data[, CommercialSp := ifelse(CommercialSp %in% input$IsCommercial, TRUE, FALSE)]
  }

  # LogicVar <- LogicVar[LogicVar %in% colnames(Data)]
  # Data[, (LogicVar) := lapply(.SD, as.logical), .SDcols = LogicVar] # () to say that these are existing columns and not new ones to create


  ## Date of measurement ####
  if(!input$CensusDate %in% "none"){

    CensusDateFormat <- input$CensusDateFormat
    CensusDateFormat <- gsub("(?<=^)\\w|(?<=[[:punct:]])\\w", "%", CensusDateFormat, perl = T, ignore.case = T) # replace first letter of each word by '%'
    CensusDateFormat <- gsub("yyy", "Y", CensusDateFormat, ignore.case = T)# if remains 3 `y`, change to upper case Y


    Data[, CensusDateOriginal := CensusDate]
    Data[, CensusDate := as.Date(trimws(as.character(CensusDate)), format = CensusDateFormat)]

    if(any(!is.na(Data$CensusDateOriginal) & is.na(Data$CensusDate))) warning("Some dates were translated as NA... Either your data format does not corresponf to the format of your date column, or you do not have a consistent format across all your dates")

  }




  # make input complete ####

  ## enter all itemID in inptu as "none" so we can refer to them - make sure this happens after standardizing column names otherwise that won't work...
  input[setdiff(x$ItemID, names(input))] <- "none"



  # Fill in info in column missing ####

  ## Year
  if(input$CensusYear %in% "none") {
    if(!input$CensusDate %in% "none") Data[, CensusYear := format(CensusDate, "%Y")] else Data[, CensusYear := input$CensusYearMan]
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

  if(input$DBH %in% "none" & !input$Circ %in% "none")
    Data[, DBH := round(Circ/pi, 2)]




  # Units changing ####

  unitOptions <- c("mm", "millimetre", "millimeter", "milimetro", "milimetrica", "cm", "centimetre", "centimeter", "centimetro", "dm", "decimetre", "decimeter", "decimetro", "m", "metre", "meter", "metro")

  AreaUnitOptions <- c("m2", "ha", "km2")

  ### DBH in cm ####
  if((!input$DBH %in% "none" & !input$DBHUnit %in% "none") | (!input$Circ %in% "none" & !input$CircUnit %in% "none")) stop("We have not coded the case where size units are not constant across your data yet - Please contact us or unify your units first.")

  if(!input$DBH %in% "none" | !input$Circ %in% "none") {

    SizeUnit <- grep("[^none]", c(input$DBHUnitMan, input$CircUnitMan), value = T)[1] # take DBH in priority, otherwise CircUnit (not a big deal since we only care about DBH and we already converted it from Circ if that was the only size we had)

    if(! SizeUnit %in% unitOptions) stop(paste("Your size units are not one of:", paste(unitOptions, collapse = ", ")))

    if (substr(SizeUnit, 1, 2) == "mm" | substr(SizeUnit, 1, 2) == "mi")
      Data[, DBH := DBH/10] # mm -> cm

    if (substr(SizeUnit, 1, 1) == "d")
      Data[, DBH := DBH*10] # dm -> cm

    if (substr(SizeUnit, 1, 1) == "m")
      Data[, DBH := DBH*100] # m -> cm
  }

  ### POM in m ####
  if(!input$POM %in% "none" & !input$POMUnit %in% "none") stop("We have not coded the case where POM units are not constant across your data yet - Please contact us or unify your units first.")

  if(!input$POM %in% "none" & !input$POMUnitMan %in% "none") {

    POMUnit <- input$POMUnitMan

    if(!POMUnit %in% unitOptions) stop(paste("Your POM units are not one of:", paste(unitOptions, collapse = ", ")))

    if (POMUnit %in% unitOptions)

      if (substr(POMUnit, 1, 2) == "mm" | substr(POMUnit, 1, 2) == "mi")
        Data[, POM := POM/1000] # mm -> m

    if (substr(POMUnit, 1, 1) == "m")
      Data[, POM := POM/100] # cm -> m


    if (substr(POMUnit, 1, 1) == "d")
      Data[, POM := POM/10] # dm -> m
  }



  ### TreeHeight in m ####
  if(!input$TreeHeight %in% "none" & !input$TreeHeightUnit %in% "none") stop("We have not coded the case where height units are not constant across your data yet - Please contact us or unify your units first.")


  if(!input$TreeHeight %in% "none" & !input$TreeHeightUnitMan %in% "none") {

    TreeHeightUnit <- input$TreeHeightUnitMan

    if(! TreeHeightUnit %in% unitOptions) stop(paste("Your height units are not one of:", paste(unitOptions, collapse = ", ")))

    if (TreeHeightUnit %in% unitOptions)

    if (substr(TreeHeightUnit, 1, 2) == "mm" | substr(TreeHeightUnit, 1, 2) == "mi")
      Data[, TreeHeight := TreeHeight/1000] # mm -> m

    if (substr(TreeHeightUnit, 1, 1) == "m")
      Data[, TreeHeight := TreeHeight/100] # cm -> m


    if (substr(TreeHeightUnit, 1, 1) == "d")
      Data[, TreeHeight := TreeHeight/10] # dm -> m
  }




  ### PlotArea in ha ####

  if(!input$PlotArea %in% "none" & !input$PlotAreaUnitMan %in% "none") {

    PlotAreaUnit <- input$PlotAreaUnitMan

    if(!PlotAreaUnit %in% AreaUnitOptions) stop(paste("Your height units are not one of:", paste(AreaUnitOptions, collapse = ", ")))

    if (PlotAreaUnit %in% AreaUnitOptions)

      if (substr(PlotAreaUnit, 1, 2) == "m2")
        Data[, PlotArea := PlotArea/10000] # m2 -> ha


    if (substr(PlotAreaUnit, 1, 1) == "km2")
      Data[, PlotArea := PlotArea*100] # km2 -> ha
  }



  # return output ####
  ColumnsToReturn <- intersect(x$ItemID, colnames(Data))
  return(Data[, ..ColumnsToReturn])

}


