#' RequiredFormat
#'
#' @param Data Input forest inventory (data.frame)
#'
#' @param Plot The name of the column containing plot indices (character)
#'
#' @param SubPlot The name of the column containing sub-plot indices (character)
#'
#' @param Time The name of the column(s) containing census years (character or vector of character)
#'
#' @param IdTree The name of the column containing tree unique identifiers
#'   (character)
#'
#' @param LifeStatus The name of the column containing tree vital status
#'   (character)
#'
#' @param Size The name of the column(s) containing tree size measurements,
#'   diameter or circumference (character or vector of character)
#' @param SizeUnit The 'Size' measurement unit of the tree. The possible values
#'   are 'mm' for millimetres, 'cm' for centimetres, 'dm' for decimetres and 'm'
#'   for metres", or "code" if it is a code and not a measure (character)
#'
#' @param POM The name of the column containing Point of Measurement (POM)
#'   values: height at which the 'Size' was measured, or POM code (character)
#' @param POMUnit The 'POM' unit of the tree. The possible values are 'mm' for
#'   millimetres, 'cm' for centimetres, 'dm' for decimetres and 'm' for metres",
#'   or "code" if it is a code and not a measure (character)
#'
#' @param PlotArea The name of the column containing the area of the plot, in
#'   hectares (character), or the area of the plot in numeric
#'
#' @param X The name of the column containing the tree X coordinate (character)
#' @param Y The name of the column containing the tree Y coordinate (character)
#'
#' @param ScientificName The name of the column containing the binary scientific
#'   name of the tree (character)
#'
#' @param VernName The name of the column containing the vernacular name of the
#'   tree (character)
#'
#' @param Family The name of the column containing the family name of the tree
#'   (character)
#'
#' @param Genus The name of the column containing the genus name of the tree
#'   (character)
#' @param Species The name of the column containing the species name of the tree
#'   (character)
#'
#' @param CommercialSp The name of the column containing the commercial or
#'   non-commercial nature of the tree species (character)
#'
#' @param TreeHeight The name of the column containing the the measured height
#'   of the tree (character)
#' @param TreeHeightUnit The 'TreeHeight' unit. The possible values are 'mm' for
#'   millimetres, 'cm' for centimetres, 'dm' for decimetres and 'm' for
#'   metres", or "code" if it is a code and not a measure (character)
#'
#' @return Input inventory (data.frame) in the required package format.
#'
#' @export
#'
#' @importFrom data.table copy setDT setDF :=
#'
#' @examples
#'\dontrun{
#' data(ParacouSubsetWide)
#' Required_Format <- RequiredFormat(
#'   ParacouSubsetWide,
#'   Plot = "SubPlot",
#'   SubPlot = "SubSubPlot",
#'   Time = c("2016","2017","2018","2019","2020"),
#'   IdTree = "idTree",
#'   LifeStatus = "CodeAlive",
#'   Size = "Circ",
#'   SizeUnit = "m",
#'   POM = "MeasCode",
#'   POMUnit = "code",
#'   PlotArea = "PlotArea",
#'   X = "Xutm",
#'   Y = "Yutm",
#'   ScientificName = NULL,
#'   VernName = "VernName",
#'   Family = "Family",
#'   Genus = "Genus",
#'   Species = "Species",
#'   CommercialSp = "CommercialSp",
#'   TreeHeight = NULL,
#'   TreeHeightUnit = NULL)
#'                }
#'
RequiredFormat <- function(
  Data,
  Plot = NULL,
  SubPlot = NULL,
  Time = NULL,
  IdTree = NULL,
  LifeStatus = NULL,
  Size = NULL,
  SizeUnit = NULL,
  POM = NULL,
  POMUnit = NULL,
  PlotArea = NULL,
  X = NULL,
  Y = NULL,
  ScientificName = NULL,
  VernName = NULL,
  Family = NULL,
  Genus = NULL,
  Species = NULL,
  CommercialSp = NULL,
  TreeHeight = NULL,
  TreeHeightUnit = NULL
){

  #### Check arguments ####

  args <- c(Plot, SubPlot, Time, IdTree, LifeStatus,
            Size, SizeUnit, POM, POMUnit, X, Y,
            ScientificName, VernName, Family, Genus, Species, CommercialSp,
            TreeHeight, TreeHeightUnit)

  argsCol <- args[!args %in% c(SizeUnit, POMUnit, TreeHeightUnit)] # columns names arguments
  argsUnit <- args[args %in% c(SizeUnit, POMUnit, TreeHeightUnit)] # units arguments

  ## Argument class

  if (!inherits(Data, "data.frame"))
    stop("Data must be a data.frame")

  if(!any(is.character(PlotArea) || is.numeric(PlotArea)))
    stop(cat(PlotArea, "must be in character or numeric"))

  if(is.character(PlotArea))
    argsCol <- c(argsCol, PlotArea) # PlotArea is a column name


  for(i in 1:length(args)) # all args
    if(!inherits(args[i], "character"))
      stop(paste0(args[i], "must be in character"))

  ## Units

  if(length(argsUnit) > 0){
    for(i in 1:length(argsUnit))
      if(!argsUnit[i] %in% c("code", "mm", "millimetre", "millimeter", "milimetro", "milimetrica",
                             "cm", "centimetre", "centimeter", "centimetro",
                             "dm", "decimetre", "decimeter", "decimetro",
                             "m", "metre", "meter", "metro")
      )
        stop("Please indicate units with 'mm' for millimetres, 'cm' for
          centimetres, 'dm' for decimetres and 'm' for metres")
  }

  ## The column name exists

  for(i in 1:length(argsCol))
    if(!argsCol[i] %in% names(Data))
      stop(paste0(argsCol[i]," is not a column name of your dataset"))

  # if the variable exists but the unit is NULL
  if(is.null(SizeUnit) & !is.null(Size))
    SizeUnit <- readline(cat("In what unit is your variable '", Size, "'?")) # question to the user


  if(is.null(POMUnit) & !is.null(POM))
    POMUnit <- readline(cat("In what unit is your variable '", POM, "'?")) # question to the user. "code" is a possible answer


  if(is.null(TreeHeightUnit) & !is.null(TreeHeight))
    TreeHeightUnit <- readline(cat("In what unit is your variable '", TreeHeight, "'?")) # question to the user

  #### Formatting ####

  DataInput <- copy(Data) # input data copy

  ## data.frame to data.table
  setDT(Data) # with "set" "<-" is not necessary

  ## Wide to long format (detect and change)
  # detect : arguments pour lesquels un vecteur est renseigné

  # melt(Data,
  #      id.vars = c("col1", "col2"), # columns that remain columns (arguments pour lesquels il n'ya qu'1 val renseignée)
  #      measure.vars = patterns("^a", "^b"), # cols to rows (arguments pour lesquels un vecteur est renseigné)
  #      variable.name = "Time", # name of the new column that contains the names of the transposed variables
  #      value.name = "Size") # name of the new column that contains the values of the transposed variables

  ## Class changing

  ### if it's a code
  # for(u in 1:length(argsUnit)){ # argsUnit: SizeUnit, POMUnit, TreeHeightUnit
  #   if(argsUnit[u] == "code"){
  #
  #   }
  # }

  ### as.character
  CharacVar <- c(Plot, SubPlot, IdTree, ScientificName, VernName, Family, Genus, Species) # character variables

  for(v in 1:length(CharacVar))
    Data[, c(CharacVar[v]) := as.character(get(CharacVar[v]))]

  ### as.numeric
  NumVar <- c(Time, Size, PlotArea, X, Y, TreeHeight) # numeric variables

  for(v in 1:length(NumVar))
    Data[, c(NumVar[v]) := as.numeric(get(NumVar[v]))]

  ### as.logical
  LogicVar <- c(LifeStatus, CommercialSp) # logical variables

  for(v in 1:length(LogicVar))
    Data[, c(LogicVar[v]) := as.logical(get(LogicVar[v]))]

  ## Units changing
  ### Size in cm
  if(Size %in% names(Data)){

    if (substr(SizeUnit, 1, 2) == "mm" | substr(SizeUnit, 1, 2) == "mi")
      Data[, c(Size) := get(Size)/10] # mm -> cm

    if (substr(SizeUnit, 1, 1) == "d")
      Data[, c(Size) := get(Size)*10] # dm -> cm

    if (substr(SizeUnit, 1, 1) == "m")
      Data[, c(Size) := get(Size)*100] # m -> cm
  }

  ### TreeHeight in m
  if(TreeHeight %in% names(Data)){

    if (substr(TreeHeightUnit, 1, 2) == "mm" | substr(TreeHeightUnit, 1, 2) == "mi")
      Data[, c(TreeHeight) := get(TreeHeight)/1000] # mm -> m

    if (substr(TreeHeightUnit, 1, 1) == "m")
      Data[, c(TreeHeight) := get(TreeHeight)/100] # cm -> m


    if (substr(TreeHeightUnit, 1, 1) == "d")
      Data[, c(TreeHeight) := get(TreeHeight)/10] # dm -> m
  }

  ## Necessary columns creation from the existing

  ### IdTree

  ### POM ? (if pom is a code)

  ### PlotArea (not a column but a value)
  if(!PlotArea %in% names(Data) & is.numeric(PlotArea)){

    if(is.numeric(PlotArea)){ # if PlotArea is a (1) (numeric value) (cas à faire : 1 val par plot)
      Data[,  PlotArea := PlotArea]
    }
  }

  ### Genus Species (if ScientificName exists) (how to know the sep?)
  tidyr::separate(ScientificName, sep = " ", into = c("Genus", "Species"), remove = F) # data.table version : https://stackoverflow.com/questions/18154556/split-text-string-in-a-data-table-columns

  ### ScientificName (if Genus & Species exist)
  tidyr::unite(Genus, Species, col = "ScientificName", sep = "_", remove = F)

  # Return in data.frame
  setDF(Data)
  return(Data)

}
