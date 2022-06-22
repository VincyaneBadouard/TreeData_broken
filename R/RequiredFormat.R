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
#'  package. It also generates missing information, when possible (e.g. Diameter when
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
  Data <- copy(Data)   # <~~~~~ KEY LINE so things don't happen on the global environemnt

  # coerce to data types ####
  ### as.character

  CharacVar <- CharacVar[CharacVar %in% colnames(Data)]

  Data[, (CharacVar) := lapply(.SD, as.character), .SDcols = CharacVar] # (CharacVar) to say that these are existing columns and not new ones to create

  ### as.numeric

  NumVar <- NumVar[NumVar  %in% colnames(Data)]

  Data[, (NumVar) := lapply(.SD, as.character), .SDcols = NumVar] # first as character when the variable is in factor, to preserve information
  suppressWarnings(Data[, (NumVar) := lapply(.SD, as.numeric), .SDcols = NumVar]) # () to say that these are existing columns and not new ones to create

  ### as.logical
  ## Here we have to use user input to know what is TRUE and what is not

  ### Life status
  if( !is.null(input$LifeStatus)) {
    if(!input$LifeStatus %in% "none") {
      Data[, LifeStatusOriginal := LifeStatus]
      Data[, LifeStatus := ifelse(LifeStatus %in% input$IsLive, TRUE, FALSE)]
    }
  }

  ### commercial species
  if( !is.null(input$CommercialSp)) {
    if( !input$CommercialSp %in% "none") {
    Data[, CommercialSpOriginal := CommercialSp]
    Data[, CommercialSp := ifelse(CommercialSp %in% input$IsCommercial, TRUE, FALSE)]
  }
    }

  # LogicVar <- LogicVar[LogicVar %in% colnames(Data)]
  # Data[, (LogicVar) := lapply(.SD, as.logical), .SDcols = LogicVar] # () to say that these are existing columns and not new ones to create


  ## Date of measurement ####
  # concatenate if in 3 different columns
  if(!input$Month %in% "none" & !input$Day %in% "none" & input$Date %in% "none") {
    if(!input$Year %in% "none") {
      Data[, Date := paste(trimws(Year), trimws(Month), trimws(Day), sep = "-")]
    } else {
      if(!input$YearMan %in% -999) Data[, Date := paste(input$YearMan, trimws(Month), trimws(Day), sep = "-")] else warning("You did not provide a Year or date.")
    }

    # overwrite input
    input$Date = "Date"
    input$DateFormatMan = "yyyy-mm-dd"
  }

  # put in date format

    if(!input$Date %in% "none"){

      # save the orginal dates
      Data[, DateOriginal := Date]

      # transform to standard format
      DateFormat <- trimws(input$DateFormatMan)


      if(grepl("num|dec", DateFormat, ignore.case = T)) {

        if(grepl("num", DateFormat, ignore.case = T)) suppressWarnings(Data[, Date := as.Date(as.numeric(trimws(Date)), origin = "1970-01-01")])

        if(grepl("dec", DateFormat, ignore.case = T)) suppressWarnings(Data[, Date := as.Date(lubridate::date_decimal(as.numeric(trimws(Date))))])

      } else {

        DateFormat <- gsub("(?<=^)\\w|(?<=[[:punct:]])\\w", "%", DateFormat, perl = T, ignore.case = T) # replace first letter of each word by '%'
        DateFormat <- gsub("yyy", "Y", DateFormat, ignore.case = T)# if remains 3 `y`, change to upper case Y

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
    if(!input$Date %in% "none") Data[, Year := format(Date, "%Y")] else if(!input$YearMan %in% -999) Data[, Year := input$YearMan] else waring("You did not provide Date or Year")

    Data$Year <- as.numeric(as.character(Data$Year))

  }

 ## Site, Plote, subplot
  if (input$Site %in% "none") {
    if(input$SiteMan %in% "")  warning("You did not specify a Site column or name, we will consider you have only one site called 'SiteA'")

    SiteMan <- ifelse(input$SiteMan %in% "", "SiteA", input$SiteMan)
    Data[, Site :=  SiteMan]

  }
  if (input$Plot %in% "none") {
    if(input$PlotMan %in% "")  warning("You did not specify a Plot column or name, we will consider you have only one plot called 'PlotA'")

    PlotMan <- ifelse(input$PlotMan %in% "", "PlotA", input$PlotMan)
    Data[, Plot :=  PlotMan]
  }

  if (input$SubPlot %in% "none"){
    if(input$SubPlotMan %in% "")  warning("You did not specify a subplot column or name, we will consider you have only one subplot called 'SubPlotA'")

    SubPlotMan <- ifelse(input$SubPlotMan %in% "", "SubPlotA", input$SubPlotMan)
    Data[, SubPlot := SubPlotMan]
  }

  ## IdTree (unique along IdCensus) ####

  if (input$IdTree %in% "none" | any(is.na(Data$IdTree))) {

    # if we also don't have TreeFieldNum, we are just considering that each row within a plot and subplot is one tree
    if(input$IdTree %in% "none") Data$IdTree <- NA

    if (input$TreeFieldNum %in% "none") {
      if(input$IdCensus %in% "none") {
        warning("You are missing treeIDs (either you are missing some tree IDs or you  did not specify a column for tree IDs). You also did not specify a column for Tree Tags, nore Census ID, so we are considering that each row without a tree ID refers to one unique tree.")
        Data[is.na(IdTree), IdTree := paste(seq(1, .N), "_auto") ]
      }
      if(!input$IdCensus %in% "none") {
        warning("You are missing treeIDs (either you are missing some tree IDs or you  did not specify a column for tree IDs). You also did not specify a column for Tree Tags, so we are considering that each row within a census ID refers to one unique tree. This is assuming the order of your trees is consistent accross censuses.")

        Data[is.na(IdTree), IdTree := paste(seq(1, .N), "_auto")  , by = .(IdCensus)]
      }
    }

    # if we have TreeFieldNum, we use it

    if (!input$TreeFieldNum %in% "none") {
      if(input$IdCensus %in% "none") {
        warning("You are missing treeIDs (either you are missing some tree IDs or you  did not specify a column for tree IDs). You also did not specify a column Census ID. But you did specify a column for tree tag, so we are considering that each row without a tree ID within a Site, plot and subplot refers to one unique tree, and we are using your tree field number to construct the tree ID.")
        Data[is.na(IdTree), IdTree := paste(Site, Plot, SubPlot, TreeFieldNum, "auto", sep = "_")]
      }

      if(!input$IdCensus %in% "none") {
        warning("You are missing treeIDs (either you are missing some tree IDs or you  did not specify a column for tree IDs). But you did specify a column for tree tag and census ID, so we are considering that each row within a Site, plot, subplot and census ID refers to one unique tree, and we are using your tree field number to construct the tree ID.")
        Data[is.na(IdTree), IdTree := paste(Site, Plot, SubPlot, TreeFieldNum, "auto", sep = "_") , by = .(IdCensus)]
      }
    }


  }

  # if (!input$IdTree %in% "none" & any(is.na(Data$IdTree))) {
  #
  #   # if there is an IdTree but some IDs are NA --> fill those with unique values  - This patch is to handle ForestPlot data, where they have stem level information, so each stem gets a stemID, and IdTree is the stem group ID. if a stem is alone, its stem group ID is NA, so there are some NA's in IdTree.
  #  if(input$IdCensus %in% "none") {
  #    warning("You have some IdTree that are missing and you did not specify a column for Census ID, so we are considering that each row, with a missing IdTree refers to one unique tree.")
  #
  #    Data[is.na(IdTree), IdTree := paste(seq(1, .N), "_auto")  ]
  #    }
  #  if(!input$IdCensus %in% "none") {
  #    warning("You have some IdTree that are missing so we are considering that each row within a census ID refers to one unique tree. This is assuming the order of your trees is consistent accross censuses.")
  #
  #    Data[is.na(IdTree), IdTree := paste0(seq(1, .N), "_auto") , by = .(IdCensus)]
  #  }
  #
  #
  # }
  ## Genus, Species, ScientificNameSepMan ####

  ### Genus and species if we have ScientificName and ScientificNameSepMan
  if(input$Genus %in% "none" & input$Species %in% "none" & !input$ScientificName %in% "none" & !input$ScientificNameSepMan %in% "none") Data[, c("Genus", "Species") := tstrsplit(ScientificName, input$ScientificNameSepMan , fixed = TRUE, keep  = c(1,2))]

  ### ScientificName if we have Genus and species

  if(!input$Genus %in% "none" & !input$Species %in% "none" & input$ScientificName %in% "none" ) Data[, ScientificName := paste(Genus, Species)]


  ## Diameter if we have circumference ####
  if(input$Diameter %in% "none" & input$Circ %in% "none" & input$BD %in% "none" & input$BCirc %in% "none") stop("You do not have tree size (Diameter, Circonference, BD or basal circonference) in your data (or you have not specified what column that information is store in. We cannot move forward.")

  if(input$Diameter %in% "none" & !input$Circ %in% "none") Data[, Diameter := round(Circ/pi, 2)]
  if(input$BD %in% "none" & !input$BCirc %in% "none") Data[, BD := round(BCirc/pi, 2)]


  ## MinDBH if we don't have it
  if(input$MinDBH %in% "none") {

    if(!input$MinDBHMan %in% -999) {
      Data[, MinDBH := input$MinDBHMan]
      input$MinDBHUnitMan <- "cm" # if MinDBH given by hand, it should be in cm

    }
    if(input$MinDBHMan %in% -999) {
    Data[, MinDBH := min(Diameter)]
    input$MinDBHUnitMan <- grep("[^none]", c(input$DiameterUnitMan, input$CircUnitMan), value = T)[1] # take Diameter in priority, otherwise CircUnit
    warning("MinDBH was calculated.")
    }
  }


  # Units changing ####

  unitOptions <- c("mm", "cm", "dm", "m") # c("mm", "millimetre", "millimeter", "milimetro", "milimetrica", "cm", "centimetre", "centimeter", "centimetro", "dm", "decimetre", "decimeter", "decimetro", "m", "metre", "meter", "metro")

  AreaUnitOptions <- c("m2", "ha", "km2")

  ### Diameter, MinDBH and BD in cm ####
  # if((!input$Diameter %in% "none" & !input$DiameterUnit %in% "none") | (!input$Circ %in% "none" & !input$CircUnit %in% "none")) stop("We have not coded the case where size units are not constant across your data yet - Please contact us or unify your units first.")

  if(!input$Diameter %in% "none" | !input$Circ %in% "none") {

    SizeUnit <- grep("[^none]", c(input$DiameterUnitMan, input$CircUnitMan), value = T)[1] # take Diameter in priority, otherwise CircUnit (not a big deal since we only care about Diameter and we already converted it from Circ if that was the only size we had)

    if(!SizeUnit %in% unitOptions) stop(paste("Your tree size units are not one of:", paste(unitOptions, collapse = ", ")))

    if(SizeUnit %in% unitOptions) {

    if (SizeUnit == "mm") Data[, Diameter := Diameter/10] # mm -> cm

    if (SizeUnit == "dm") Data[, Diameter := Diameter*10] # dm -> cm

    if (SizeUnit == "m") Data[, Diameter := Diameter*100] # m -> cm
    }

    # (re)calculate Circ
    Data[, Circ := round(Diameter*pi, 2)]
  }

  if(!input$BD %in% "none" | !input$BCirc %in% "none") {

    BSizeUnit <- grep("[^none]", c(input$BDUnitMan, input$BCircUnitMan), value = T)[1] # take Diameter in priority, otherwise CircUnit (not a big deal since we only care about Diameter and we already converted it from Circ if that was the only size we had)

    if(!BSizeUnit %in% unitOptions) stop(paste("Your basal size units are not one of:", paste(unitOptions, collapse = ", ")))

    if(BSizeUnit %in% unitOptions) {
      if (BSizeUnit == "mm") Data[, BD := BD/10] # mm -> cm

      if (BSizeUnit == "dm") Data[, BD := BD*10] # dm -> cm

      if (BSizeUnit == "m") Data[, BD := BD*100] # m -> cm
    }

    Data[, BCirc := round(BD*pi, 2)]
  }

  if(!input$MinDBH %in% "none") {

    SizeUnit <- input$MinDBHUnitMan

    if(!SizeUnit %in% unitOptions) stop(paste("Your minimum DBH size units are not one of:", paste(unitOptions, collapse = ", ")))

    if(SizeUnit %in% unitOptions) {

      if (SizeUnit == "mm") Data[, MinDBH := MinDBH/10] # mm -> cm

      if (SizeUnit == "dm") Data[, MinDBH := MinDBH*10] # dm -> cm

      if (SizeUnit == "m") Data[, MinDBH := MinDBH*100] # m -> cm
    }

  }

  ### HOM and BHOM in m ####
  # if(!input$HOM %in% "none" & !input$HOMUnit %in% "none") stop("We have not coded the case where HOM units are not constant across your data yet - Please contact us or unify your units first.")

  if(!input$HOM %in% "none") {

    # if(input$HOMUnitMan %in% "none") stop("we need HOM units")

    HOMUnit <- input$HOMUnitMan

    if(!HOMUnit %in% unitOptions) stop(paste("Your HOM units are not one of:", paste(unitOptions, collapse = ", ")))

    if (HOMUnit %in% unitOptions) {

      if (HOMUnit == "mm") Data[, HOM := HOM/1000] # mm -> m

      if (HOMUnit == "cm") Data[, HOM := HOM/100] # cm -> m


      if (HOMUnit == "dm") Data[, HOM := HOM/10] # dm -> m
    }
  }

  if(!input$BHOM %in% "none") {

    # if(input$BHOMUnitMan %in% "none") stop("we need basal HOm units")


    BHOMUnit <- input$BHOMUnitMan

    if(!BHOMUnit %in% unitOptions) stop(paste("Your basal HOM units are not one of:", paste(unitOptions, collapse = ", ")))

    if (BHOMUnit %in% unitOptions) {

      if (BHOMUnit == "mm") Data[, BHOM := BHOM/1000] # mm -> m

      if (BHOMUnit == "cm") Data[, BHOM := BHOM/100] # cm -> m

      if (BHOMUnit == "dm") Data[, BHOM := BHOM/10] # dm -> m
    }
  }


  ### TreeHeight in m ####
  # if(!input$TreeHeight %in% "none" & !input$TreeHeightUnit %in% "none") stop("We have not coded the case where height units are not constant across your data yet - Please contact us or unify your units first.")


  if(!input$TreeHeight %in% "none") {

    # if(input$TreeHeightUnitMan %in% "none") stop("we need tree height units")

    TreeHeightUnit <- input$TreeHeightUnitMan

    if(!TreeHeightUnit %in% unitOptions) stop(paste("Your height units are not one of:", paste(unitOptions, collapse = ", ")))

    if (TreeHeightUnit %in% unitOptions) {

      if (TreeHeightUnit == "mm") Data[, TreeHeight := TreeHeight/1000] # mm -> m

      if (TreeHeightUnit == "cm") Data[, TreeHeight := TreeHeight/100] # cm -> m

      if (TreeHeightUnit == "dm") Data[, TreeHeight := TreeHeight/10] # dm -> m
    }
  }




  ### PlotArea in ha ####

  if(!input$PlotArea %in% "none") {

    # if(input$PlotAreaUnitMan %in% "none") stop("we need Plot Area units")

    PlotAreaUnit <- input$PlotAreaUnitMan

    if(!PlotAreaUnit %in% AreaUnitOptions) stop(paste("Your plot area units are not one of:", paste(AreaUnitOptions, collapse = ", ")))

    if (PlotAreaUnit %in% AreaUnitOptions) {

      if (PlotAreaUnit == "m2") Data[, PlotArea := PlotArea/10000] # m2 -> ha

      if (PlotAreaUnit == "km2") Data[, PlotArea := PlotArea*100] # km2 -> ha
    }
  }

  # if area is entered manually, it is supposed to be in ha already
  if(input$PlotArea %in% "none") {
    if(!input$PlotAreaMan %in% -999) Data[, PlotArea := input$PlotAreaMan]

    if(input$PlotAreaMan %in% -999) warning("You did not specify a plot area")
  }

  ### SubPlotArea in ha ####

  if(!input$SubPlotArea %in% "none") {

    SubPlotAreaUnitMan <- input$SubPlotAreaUnitMan

    if(!SubPlotAreaUnitMan %in% AreaUnitOptions) stop(paste("Your subplot area units are not one of:", paste(AreaUnitOptions, collapse = ", ")))

    if (SubPlotAreaUnitMan %in% AreaUnitOptions){

      if (SubPlotAreaUnitMan == "m2") Data[, SubPlotArea := SubPlotArea/10000] # m2 -> ha
      if (SubPlotAreaUnitMan == "km2") Data[, SubPlotArea := SubPlotArea*100] # km2 -> ha
    }
  }

  # if area is entered manually, it is supposed to be in ha already
  if(input$SubPlotArea %in% "none") {

    if(!input$SubPlotAreaMan %in% -999) Data[, SubPlotArea := input$SubPlotAreaMan]

    if(input$SubPlotAreaMan %in% -999) warning("You did not specify a subplot area")

  }

  ### XY coordinates in m ####


  if(!input$Xutm %in% "none") {

    utmUnitMan <- input$utmUnitMan

    if(!utmUnitMan %in% unitOptions) stop(paste("Your utm units are not one of:", paste(unitOptions, collapse = ", ")))

    if (utmUnitMan %in% unitOptions) {

      if (utmUnitMan == "mm") {
        Data[, Xutm := Xutm/1000] # mm -> m
        Data[, Yutm := Yutm/1000] # mm -> m
      }

      if (utmUnitMan == "cm") {
        Data[, Xutm := Xutm/100] # cm -> m
        Data[, Yutm := Yutm/100] # cm -> m

        }

      if (utmUnitMan == "dm") {
        Data[, Xutm := Xutm/10] # dm -> m
        Data[, Yutm := Yutm/10] # dm -> m
        }

    }
  }

  if(!input$Xplot %in% "none") {

    plotUnitMan <- input$plotUnitMan

    if(!plotUnitMan %in% unitOptions) stop(paste("Your plot coordinates units are not one of:", paste(unitOptions, collapse = ", ")))

    if (plotUnitMan %in% unitOptions) {

      if (plotUnitMan == "mm") {
        Data[, Xplot := Xplot/1000] # mm -> m
        Data[, Yplot := Yplot/1000] # mm -> m
      }

      if (plotUnitMan == "cm") {
        Data[, Xplot := Xplot/100] # cm -> m
        Data[, Yplot := Yplot/100] # cm -> m

      }

      if (plotUnitMan == "dm") {
        Data[, Xplot := Xplot/10] # dm -> m
        Data[, Yplot := Yplot/10] # dm -> m
      }

    }
  }

  if(!input$Xsubplot %in% "none") {

    subplotUnitMan <- input$subplotUnitMan

    if(!subplotUnitMan %in% unitOptions) stop(paste("Your subplot coordinates units are not one of:", paste(unitOptions, collapse = ", ")))

    if (subplotUnitMan %in% unitOptions) {

      if (subplotUnitMan == "mm") {
        Data[, Xsubplot := Xsubplot/1000] # mm -> m
        Data[, Ysubplot := Ysubplot/1000] # mm -> m
      }

      if (subplotUnitMan == "cm") {
        Data[, Xsubplot := Xsubplot/100] # cm -> m
        Data[, Ysubplot := Ysubplot/100] # cm -> m

      }

      if (subplotUnitMan == "dm") {
        Data[, Xsubplot := Xsubplot/10] # dm -> m
        Data[, Ysubplot := Ysubplot/10] # dm -> m
      }

    }
  }
  # return output ####
  ColumnsToReturn <- intersect(c(x$ItemID, paste0(x$ItemID, "Original")), colnames(Data))
  return(Data[, ..ColumnsToReturn])

}


